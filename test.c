
#define _POSIX_C_SOURCE	2 /* getopt(3) */

#include <stdio.h>

#include "cclerical.h"
#include "cclerical.tab.h" /* cclerical_parse*() */
#include "cclerical.lex.h" /* cclerical_lex_(init|destroy) */

static void pprog(const struct cclerical_prog *p, int lvl);

static void pexpr(const struct cclerical_expr *e, int lvl)
{
	static const char *const st[] = {
		"decl-asgn", "cnst", "var", "case", "lim", "op",
	};
	fprintf(stderr, "%*sexpr: %s of type %s\n", lvl, "", st[e->type],
	        CCLERICAL_TYPE_STR[e->result_type]);
	switch (e->type) {
	case CCLERICAL_EXPR_DECL_ASGN:
		fprintf(stderr, "%*sdecl-asgn setting var #%zu to\n", lvl, "",
		        e->decl_asgn.var);
		pexpr(e->decl_asgn.expr, lvl+1);
		fprintf(stderr, "%*sdecl-asgn computing\n", lvl, "");
		pprog(e->decl_asgn.prog, lvl+1);
		fprintf(stderr, "%*s decl-asgn done\n", lvl, "");
		break;
	case CCLERICAL_EXPR_OP: {
		static const char *const ops[] = {
			[CCLERICAL_OP_NEG] = "neg",
			[CCLERICAL_OP_ADD] = "add",
			[CCLERICAL_OP_SUB] = "sub",
			[CCLERICAL_OP_MUL] = "mul",
			[CCLERICAL_OP_DIV] = "div",
			[CCLERICAL_OP_EXP] = "exp",
			[CCLERICAL_OP_LT]  = "lt",
			[CCLERICAL_OP_GT]  = "gt",
			[CCLERICAL_OP_NE]  = "ne",
		};
		fprintf(stderr, "%*sop: %s\n", lvl, "", ops[e->op.op]);
		pexpr(e->op.arg1, lvl+1);
		pexpr(e->op.arg2, lvl+1);
		break;
	}
	case CCLERICAL_EXPR_CNST: {
		fprintf(stderr, "%*scnst: ", lvl, "");
		if (e->cnst.lower_type == CCLERICAL_TYPE_BOOL)
			fprintf(stderr, "%s",
			        e->cnst.boolean ? "true" : "false");
		else
			fprintf(stderr, "(%s)_%u",
			        e->cnst.numeric.str, e->cnst.numeric.base);
		fprintf(stderr, " : %s\n",
		        CCLERICAL_TYPE_STR[e->cnst.lower_type]);
		break;
	}
	case CCLERICAL_EXPR_VAR:
		fprintf(stderr, "%*svar #%zu\n", lvl, "", e->var);
		break;
	case CCLERICAL_EXPR_FUN_CALL:
		fprintf(stderr, "%*sfun #%zu\n", lvl, "", e->fun_call.fun);
		for (size_t i=0; i<e->fun_call.params.valid; i++) {
			fprintf(stderr, "%*s param %zu:\n", lvl, "", i);
			pexpr(e->fun_call.params.data[i], lvl+2);
		}
		break;
	case CCLERICAL_EXPR_LIM:
		fprintf(stderr, "%*sseq_idx: #%zu in seq:\n", lvl, "",
		        e->lim.seq_idx);
		pprog(e->lim.seq, lvl+1);
		break;
	case CCLERICAL_EXPR_CASE:
		for (size_t i=0; i<e->cases.valid; i+=2) {
			fprintf(stderr, "%*scase %zu:\n", lvl, "", i/2);
			pexpr(e->cases.data[i], lvl+1);
			pprog(e->cases.data[i+1], lvl+1);
		}
		break;
	}
}

static void pstmt(const struct cclerical_stmt *s, int lvl)
{
	static const char *const st[] = {
		"skip", "while", "if", "asgn", "expr"
	};
	fprintf(stderr, "%*sstmt: %s\n", lvl, "", st[s->type]);
	switch (s->type) {
	case CCLERICAL_STMT_EXPR:
		pexpr(s->expr, lvl+1);
		break;
	case CCLERICAL_STMT_SKIP:
		break;
	case CCLERICAL_STMT_ASGN:
		fprintf(stderr, "%*sasgn var #%zu from\n", lvl, "", s->asgn.var);
		pexpr(s->asgn.expr, lvl+1);
		break;
	case CCLERICAL_STMT_WHILE:
		fprintf(stderr, "%*swhile cond:\n", lvl, "");
		pexpr(s->loop.cond, lvl+1);
		fprintf(stderr, "%*swhile body:\n", lvl, "");
		pprog(s->loop.body, lvl+1);
		break;
	case CCLERICAL_STMT_IF:
		fprintf(stderr, "%*sif cond:\n", lvl, "");
		pexpr(s->branch.cond, lvl+1);
		fprintf(stderr, "%*sif true branch:\n", lvl, "");
		pprog(s->branch.if_true, lvl+1);
		if (s->branch.if_false) {
			fprintf(stderr, "%*sif false branch:\n", lvl, "");
			pprog(s->branch.if_false, lvl+1);
		}
		break;
	}
}

static void pprog(const struct cclerical_prog *p, int lvl)
{
	fprintf(stderr, "%*sprogram:\n", lvl, "");
	for (size_t i=0; i<p->stmts.valid; i++)
		pstmt(p->stmts.data[i], lvl+1);
}

#define IRRAM_HEADER "\n\
/* generated by cclerical */\n\
\n\
#include <iRRAM.h>\n\
\n\
void compute()\n\
{\
"
#define IRRAM_FOOTER "\
}\n\
"

static void export_irram(const struct cclerical_prog *p,
                         const struct cclerical_vector *decls)
{
	printf("%s\n", IRRAM_HEADER);
//	printf("\tREAL vars[%zu];\n", decls->valid);
	struct cclerical_vector blocks = CCLERICAL_VECTOR_INIT;
	for (size_t i=0; i<p->stmts.valid; i++) {
		struct cclerical_stmt *s = p->stmts.data[i];
		switch (s->type) {
		case CCLERICAL_STMT_WHILE:
		case CCLERICAL_STMT_IF:
			cclerical_vector_add(&blocks, s);
			break;
		case CCLERICAL_STMT_ASGN:
		case CCLERICAL_STMT_EXPR:
		case CCLERICAL_STMT_SKIP: break;
		}
	}
	cclerical_vector_fini(&blocks);
	printf("%s\n", IRRAM_FOOTER);
}

static int getoptopt(int argc, char **argv, const char *shortopts,
                      const char *argopt)
{
	int opt = getopt(argc, argv, shortopts);
	if (opt == ':') {
		const char *c = strchr(argopt, optopt);
		if (c) {
			optarg = NULL;
			return *c;
		}
	}
	return opt;
}

#define DIE(code,...) do { fprintf(stderr, __VA_ARGS__); exit(code); } while (0)

int main(int argc, char **argv)
{
	for (int opt; (opt = getoptopt(argc, argv, ":b:D:hi:mt:U:x:", "b")) != -1;)
		switch (opt) {
		case 'b': printf("b: %s\n", optarg); break;
		case 'h':
			printf("usage: %s [-OPTS] [FILE|-]\n", argv[0]);
			printf("\n\
Options [default]:\n\
  -b TGT     target backend TGT [iRRAM]; supported values for TGT:\n\
             AERN2, Ariadne, iRRAM[:type] (where type defaults to REAL), kirm\n\
  -d         dump parse tree to stderr for debugging purposes\n\
  -D DEFINE  #define DEFINE constant\n\
  -h         print this help message\n\
  -i INT     run in interpreter mode INT; supported: iRRAM-bc, kirk-iRRAM\n\
  -m         enable support for TGT's default math library functions\n\
  -o OUTPUT  write compiled TGT source file to OUTPUT [stdout]\n\
  -U DEFINE  #undef DEFINE constant\n\
  -x DIALECT choose cclerical dialect DIALECT [T17]; supported: T17, Matlab, bc\n\
\n\
This program is distributed under BSD-3 license.\n\
Author: Franz Brausse <brausse@informatik.uni-trier.de>\n");
			exit(0);
		case '?': DIE(1,"error: unknown option '-%c'\n",optopt);
		case ':': DIE(1,"error: option '-%c' requires a parameter\n",
		              optopt);
		}

	struct cclerical_parser p;
	yyscan_t scanner;
	cclerical_lex_init(&scanner);
	cclerical_parser_init(&p);
	int r = cclerical_parse(&p, scanner);
	struct cclerical_prog *cp = p.prog;
	p.prog = NULL;
	if (r) {
		fprintf(stderr, "parse error %d, aborting\n", r);
		goto done;
	}

	pprog(cp, 0);
	export_irram(cp, &p.decls);

done:
	if (cp)
		cclerical_prog_destroy(cp);
	cclerical_parser_fini(&p);
	cclerical_lex_destroy(scanner);
	return !!r;
}
