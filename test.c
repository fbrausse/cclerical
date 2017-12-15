/* SPDX short identifier: BSD-3-Clause */
#define _POSIX_C_SOURCE	2 /* getopt(3) */

#include <stdio.h>
#include <stdarg.h>

#include "cclerical.h"
#include "cclerical.tab.h" /* cclerical_parse*() */
#include "cclerical.lex.h" /* cclerical_lex_(init|destroy) */

static void pprog(const struct cclerical_prog *p, int lvl);

static void pexpr(const struct cclerical_expr *e, int lvl)
{
	static const char *const st[] = {
		[CCLERICAL_EXPR_DECL_ASGN] = "decl-asgn",
		[CCLERICAL_EXPR_CNST     ] = "cnst",
		[CCLERICAL_EXPR_VAR      ] = "var",
		[CCLERICAL_EXPR_FUN_CALL ] = "fun-call",
		[CCLERICAL_EXPR_CASE     ] = "case",
		[CCLERICAL_EXPR_LIM      ] = "lim",
		[CCLERICAL_EXPR_OP       ] = "op",
	};
	fprintf(stderr, "%*sexpr: %s of type %s\n", lvl, "", st[e->type],
	        CCLERICAL_TYPE_STR[e->result_type]);
	switch (e->type) {
	case CCLERICAL_EXPR_DECL_ASGN:
		for (size_t i=0; i<e->decl_asgn.inits.valid; i+=2) {
			cclerical_id_t v = (uintptr_t)e->decl_asgn.inits.data[i];
			fprintf(stderr, "%*sinit %zu: setting var #%zu to\n",
			        lvl, "", i/2, v);
			pexpr(e->decl_asgn.inits.data[i+1], lvl+1);
		}
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
"

static const char *const CCLERICAL_iRRAM_TYPES[] = {
	[CCLERICAL_TYPE_UNIT] = "void",
	[CCLERICAL_TYPE_BOOL] = "bool",
	[CCLERICAL_TYPE_INT ] = "iRRAM::INTEGER",
	[CCLERICAL_TYPE_REAL] = "iRRAM::REAL",
};

static const char *const CCLERICAL_CPP_OPS[] = {
	[CCLERICAL_OP_NEG] = "!",
	[CCLERICAL_OP_ADD] = "+",
	[CCLERICAL_OP_SUB] = "-",
	[CCLERICAL_OP_MUL] = "*",
	[CCLERICAL_OP_DIV] = "/",
	[CCLERICAL_OP_EXP] = "^",
	[CCLERICAL_OP_LT]  = "<",
	[CCLERICAL_OP_GT]  = ">",
	[CCLERICAL_OP_NE]  = "!=",
};

#define DIE(code,...) do { fprintf(stderr, __VA_ARGS__); exit(code); } while (0)

#define CCL_PREFIX	"ccl_"

typedef struct cclerical_vector vec_t;

static void export_irram_var_decl(const vec_t *decls, cclerical_id_t ai, int const_ref)
{
	struct cclerical_decl *a = decls->data[ai];
	printf("%s%s %s%s%zu /* clerical: %s */",
	       const_ref ? "const " : "", CCLERICAL_iRRAM_TYPES[a->value_type],
	       const_ref ? "&" : "", CCL_PREFIX, ai, a->id);
}

static void export_irram_fun_decl(const vec_t *decls, cclerical_id_t i)
{
	struct cclerical_decl *d = decls->data[i];
	printf("/* clerical fun: %s */\n%s%s %s%zu(",
	       d->id, d->fun.body ? "static " : "",
	       CCLERICAL_iRRAM_TYPES[d->value_type],
	       CCL_PREFIX, i);
	for (size_t j=0; j<d->fun.arguments.valid; j++) {
		cclerical_id_t ai = (uintptr_t)d->fun.arguments.data[j];
		export_irram_var_decl(decls, ai, 0);
		if (j+1 < d->fun.arguments.valid)
			printf(", ");
	}
	printf(")");
}

static void cclprintf(int lvl, const char *fmt, ...)
{
	while (lvl--)
		printf("\t");
	va_list ap;
	va_start(ap,fmt);
	vprintf(fmt, ap);
	va_end(ap);
}

enum var_access {
	VAR_ACCESS_RO,
	VAR_ACCESS_RW,
	VAR_ACCESS_DEF,
	VAR_ACCESS_CALL,
};

typedef void visit_varrefs_f(const vec_t *decls, cclerical_id_t v, enum var_access access, void *cb_data);

static void visit_varrefs_prog(const vec_t *decls, const struct cclerical_prog *p,
                               visit_varrefs_f *visit, void *cb_data);

static void visit_varrefs_expr(const vec_t *decls, const struct cclerical_expr *e,
                               visit_varrefs_f *visit, void *cb_data)
{
	switch (e->type) {
	case CCLERICAL_EXPR_CASE:
		for (size_t i=0; i<e->cases.valid; i+=2) {
			visit_varrefs_expr(decls, e->cases.data[i], visit, cb_data);
			visit_varrefs_prog(decls, e->cases.data[i+1], visit, cb_data);
		}
		break;
	case CCLERICAL_EXPR_CNST:
		break;
	case CCLERICAL_EXPR_DECL_ASGN:
		for (size_t i=0; i<e->decl_asgn.inits.valid; i+=2) {
			visit(decls, (uintptr_t)e->decl_asgn.inits.data[i], VAR_ACCESS_DEF, cb_data);
			visit_varrefs_expr(decls, e->decl_asgn.inits.data[i+1], visit, cb_data);
		}
		visit_varrefs_prog(decls, e->decl_asgn.prog, visit, cb_data);
		break;
	case CCLERICAL_EXPR_FUN_CALL:
		visit(decls, e->fun_call.fun, VAR_ACCESS_CALL, cb_data);
		for (size_t i=0; i<e->fun_call.params.valid; i++)
			visit_varrefs_expr(decls, e->fun_call.params.data[i], visit, cb_data);
		break;
	case CCLERICAL_EXPR_LIM:
		visit(decls, e->lim.seq_idx, VAR_ACCESS_DEF, cb_data);
		visit_varrefs_prog(decls, e->lim.seq, visit, cb_data);
		break;
	case CCLERICAL_EXPR_OP:
		visit_varrefs_expr(decls, e->op.arg1, visit, cb_data);
		if (e->op.op != CCLERICAL_OP_NEG)
			visit_varrefs_expr(decls, e->op.arg2, visit, cb_data);
		break;
	case CCLERICAL_EXPR_VAR:
		visit(decls, e->var, VAR_ACCESS_RO, cb_data);
		break;
	}
}

static void visit_varrefs_prog(const vec_t *decls, const struct cclerical_prog *p,
                               visit_varrefs_f *visit, void *cb_data)
{
	for (size_t i=0; i<p->stmts.valid; i++) {
		const struct cclerical_stmt *s = p->stmts.data[i];
		switch (s->type) {
		case CCLERICAL_STMT_ASGN:
			visit(decls, s->asgn.var, VAR_ACCESS_RW, cb_data);
			visit_varrefs_expr(decls, s->asgn.expr, visit, cb_data);
			break;
		case CCLERICAL_STMT_EXPR:
			visit_varrefs_expr(decls, s->expr, visit, cb_data);
			break;
		case CCLERICAL_STMT_IF:
			visit_varrefs_expr(decls, s->branch.cond, visit, cb_data);
			visit_varrefs_prog(decls, s->branch.if_true, visit, cb_data);
			if (s->branch.if_false)
				visit_varrefs_prog(decls, s->branch.if_false, visit, cb_data);
			break;
		case CCLERICAL_STMT_SKIP:
			break;
		case CCLERICAL_STMT_WHILE:
			visit_varrefs_expr(decls, s->loop.cond, visit, cb_data);
			visit_varrefs_prog(decls, s->loop.body, visit, cb_data);
			break;
		}
	}
}

static void export_irram_prog(const vec_t *decls,
                              const struct cclerical_prog *p, int lvl);

struct visit_prev_scope_args {
	vec_t *vars;
	cclerical_id_t up_excl;
};

static void visit_prev_scope(const vec_t *decls, cclerical_id_t v,
                             enum var_access access, void *cb_data)
{
	struct visit_prev_scope_args *a = cb_data;
	(void)decls;
	(void)access;
	if (v < a->up_excl)
		cclerical_vector_add(a->vars, (void *)(uintptr_t)v);
}

static void export_irram_expr(const vec_t *decls,
                              const struct cclerical_expr *e, int lvl)
{
	switch (e->type) {
	case CCLERICAL_EXPR_CNST:
		switch (e->cnst.lower_type) {
		case CCLERICAL_TYPE_UNIT:
			abort();
		case CCLERICAL_TYPE_BOOL:
			cclprintf(0, "%s", e->cnst.boolean ? "true" : "false");
			break;
		case CCLERICAL_TYPE_INT:
		case CCLERICAL_TYPE_REAL:
			cclprintf(0, "%s(\"%s\")",
			          CCLERICAL_iRRAM_TYPES[e->cnst.lower_type],
			          e->cnst.numeric.str);
			break;
		}
		break;
	case CCLERICAL_EXPR_DECL_ASGN:
		cclprintf(0, "[&](");
		for (size_t i=0; i<e->decl_asgn.inits.valid; i+=2) {
			cclerical_id_t v = (uintptr_t)e->decl_asgn.inits.data[i];
			export_irram_var_decl(decls, v, 0);
			if (i+2 < e->decl_asgn.inits.valid)
				cclprintf(0, ", ");
		}
		cclprintf(0, ")\n");
		export_irram_prog(decls, e->decl_asgn.prog, lvl+1);
		cclprintf(lvl, "(");
		for (size_t i=0; i<e->decl_asgn.inits.valid; i+=2) {
			const struct cclerical_expr *f = e->decl_asgn.inits.data[i+1];
			export_irram_expr(decls, f, lvl+1);
			if (i+2 < e->decl_asgn.inits.valid)
				cclprintf(0, ", ");
		}
		cclprintf(0, ")");
		break;
	case CCLERICAL_EXPR_VAR:
		cclprintf(0, "%s%zu", CCL_PREFIX, e->var);
		break;
	case CCLERICAL_EXPR_FUN_CALL:
		cclprintf(0, "%s%zu(", CCL_PREFIX, e->fun_call.fun);
		for (size_t i=0; i<e->fun_call.params.valid; i++) {
			export_irram_expr(decls, e->fun_call.params.data[i], lvl);
			cclprintf(0, i+1 < e->fun_call.params.valid ? ", " : ")");
		}
		break;
	case CCLERICAL_EXPR_OP:
		if (e->op.op == CCLERICAL_OP_NEG) {
			cclprintf(0, "-(");
			export_irram_expr(decls, e->op.arg1, lvl);
			cclprintf(0, ")");
			break;
		}
		cclprintf(0, "(");
		export_irram_expr(decls, e->op.arg1, lvl);
		cclprintf(0, ") %s (", CCLERICAL_CPP_OPS[e->op.op]);
		export_irram_expr(decls, e->op.arg2, lvl);
		cclprintf(0, ")");
		break;
	case CCLERICAL_EXPR_CASE:
		if (e->cases.valid > 2*6)
			DIE(2,"iRRAM backend does not support more than 6 cases\n");
		cclprintf(0, "[&]{\n");
		cclprintf(lvl+1, "switch (iRRAM::choose(");
		for (size_t i=0; i<e->cases.valid; i+=2) {
			const struct cclerical_expr *f = e->cases.data[i];
			export_irram_expr(decls, f, lvl+1);
			if (i+2 < e->cases.valid)
				cclprintf(0, ", ");
		}
		cclprintf(0, ")) {\n");
		cclprintf(lvl+1, "default: abort();\n");
		for (size_t i=0; i<e->cases.valid; i+=2) {
			const struct cclerical_prog *p = e->cases.data[i+1];
			cclprintf(lvl+1, "case %zu:\n", i/2+1);
			export_irram_prog(decls, p, lvl+2);
		}
		cclprintf(lvl+1, "}\n");
		cclprintf(lvl, "}()");
		break;
	case CCLERICAL_EXPR_LIM: {
		vec_t prev_scope_vars = CCLERICAL_VECTOR_INIT;
		struct visit_prev_scope_args data = { &prev_scope_vars, e->lim.seq_idx };
		visit_varrefs_prog(decls, e->lim.seq, visit_prev_scope, &data);

		cclprintf(0, "iRRAM::limit([](int p");
		for (size_t i=0; i<prev_scope_vars.valid; i++) {
			cclprintf(0, ", ");
			export_irram_var_decl(decls, (uintptr_t)prev_scope_vars.data[i], 1);
		}
		cclprintf(0, "){\n");
		cclprintf(lvl+1, "");
		export_irram_var_decl(decls, e->lim.seq_idx, 0);
		cclprintf(0, " = -p;\n");
		export_irram_prog(decls, e->lim.seq, lvl+2);
		cclprintf(lvl, "}");
		for (size_t i=0; i<prev_scope_vars.valid; i++) {
			cclerical_id_t v = (uintptr_t)prev_scope_vars.data[i];
			cclprintf(0, ", %s%zu", CCL_PREFIX, v);
		}
		cclprintf(0, ")");

		cclerical_vector_fini(&prev_scope_vars);
		break;
	}
	}
}

static void export_irram_prog(const vec_t *decls,
                              const struct cclerical_prog *p, int lvl)
{
	cclprintf(lvl-1, "{\n");
	for (size_t i=0; i<p->stmts.valid; i++) {
		struct cclerical_stmt *s = p->stmts.data[i];
		switch (s->type) {
		case CCLERICAL_STMT_WHILE:
			cclprintf(lvl, "while (");
			export_irram_expr(decls, s->loop.cond, lvl);
			cclprintf(0, ")\n");
			export_irram_prog(decls, s->loop.body, lvl+1);
			break;
		case CCLERICAL_STMT_IF:
			cclprintf(lvl, "if (");
			export_irram_expr(decls, s->branch.cond, lvl);
			cclprintf(0, ")\n");
			export_irram_prog(decls, s->branch.if_true, lvl+1);
			if (s->branch.if_false) {
				cclprintf(lvl, "else\n");
				export_irram_prog(decls, s->branch.if_false, lvl+1);
			}
			break;
		case CCLERICAL_STMT_ASGN:
			cclprintf(lvl, "%s%zu = ", CCL_PREFIX, s->asgn.var);
			export_irram_expr(decls, s->asgn.expr, lvl);
			cclprintf(0, ";\n");
			break;
		case CCLERICAL_STMT_EXPR: {
			cclprintf(lvl, "%s", i+1 == p->stmts.valid ? "return " : "");
			export_irram_expr(decls, s->expr, lvl);
			cclprintf(0, ";\n");
			break;
		}
		case CCLERICAL_STMT_SKIP:
			cclprintf(lvl, ";\n");
			break;
		}
	}
	cclprintf(lvl-1, "}\n");
}

static void export_irram(const struct cclerical_prog *p, const vec_t *decls)
{
	printf("%s\n", IRRAM_HEADER);
	for (size_t i=0; i<decls->valid; i++) {
		struct cclerical_decl *d = decls->data[i];
		if (d->type != CCLERICAL_DECL_FUN)
			continue;
		export_irram_fun_decl(decls, i);
		printf(";\n");
	}
	printf("\n");
	for (size_t i=0; i<decls->valid; i++) {
		struct cclerical_decl *d = decls->data[i];
		if (d->type != CCLERICAL_DECL_FUN || !d->fun.body)
			continue;
		export_irram_fun_decl(decls, i);
		printf("\n");
		export_irram_prog(decls, d->fun.body, 1);
		printf("\n");
	}
	printf("void compute()\n");
	printf("{\n");
	cclprintf(1, "using iRRAM::cout;\n");
	cclprintf(1, "cout << []");
	export_irram_prog(decls, p, 2);
	cclprintf(1, "() << \"\\n\";\n");
	printf("}\n");
//	printf("\tREAL vars[%zu];\n", decls->valid);
}

int main(int argc, char **argv)
{
	for (int opt; (opt = getopt(argc, argv, ":b:D:hi:mt:U:x:")) != -1;)
		switch (opt) {
		case 'b': break;
		case 'h':
			printf("usage: %s [-OPTS] [--]\n", argv[0]);
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
	if (argc - optind > 0)
		DIE(1,"unrecognized trailing arguments\n");

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
