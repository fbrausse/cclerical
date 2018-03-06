/* SPDX short identifier: BSD-3-Clause */

#if _POSIX_C_SOURCE < 2
# undef _POSIX_C_SOURCE
# define _POSIX_C_SOURCE	2 /* getopt(3p) */
#endif

#include <stdio.h>
#include <stdarg.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "cclerical.h"
#include "cclerical.tab.h" /* cclerical_parse*() */
#include "cclerical.lex.h" /* cclerical_lex_(init|destroy) */

#include "ccl.h"
#include "ccl-ssa.h"

typedef ccl_vec_t vec_t;

#define MIN(a,b)	((a) < (b) ? (a) : (b))
#define MAX(a,b)	((a) > (b) ? (a) : (b))

static const char *const ops[] = {
	[CCLERICAL_OP_NEG] = "neg",
	[CCLERICAL_OP_NOT] = "not",
	[CCLERICAL_OP_AND] = "and",
	[CCLERICAL_OP_OR]  = "or",
	[CCLERICAL_OP_ADD] = "add",
	[CCLERICAL_OP_SUB] = "sub",
	[CCLERICAL_OP_MUL] = "mul",
	[CCLERICAL_OP_DIV] = "div",
	[CCLERICAL_OP_EXP] = "exp",
	[CCLERICAL_OP_LT]  = "lt",
	[CCLERICAL_OP_LE]  = "le",
	[CCLERICAL_OP_GT]  = "gt",
	[CCLERICAL_OP_GE]  = "ge",
	[CCLERICAL_OP_NE]  = "ne",
	[CCLERICAL_OP_EQ]  = "eq",
};

static void pcnst(FILE *out, const struct cclerical_constant *c)
{
	if (c->type == CCLERICAL_TYPE_BOOL)
		fprintf(out, "%s",
		        c->boolean ? "true" : "false");
	else
		fprintf(out, "(%s)_%u",
		        c->numeric.str, c->numeric.base);
	fprintf(out, " : %s", CCLERICAL_TYPE_STR[c->type]);
}

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
		[CCLERICAL_EXPR_IF       ] = "if",
		[CCLERICAL_EXPR_ASGN     ] = "asgn",
		[CCLERICAL_EXPR_SKIP     ] = "skip",
		[CCLERICAL_EXPR_WHILE    ] = "while",
		[CCLERICAL_EXPR_SEQ      ] = "sequence",
	};
	fprintf(stderr, "%*sexpr: %s of type %s, min_asgn_scope: %zu\n",
	        lvl, "", st[e->type], CCLERICAL_TYPE_STR[e->result_type],
	        e->min_scope_asgn);
	if (e->hoare_conds.pre)
		fprintf(stderr, "%*s hoare pre: '%s', post: '%s'\n",
		        lvl, "", e->hoare_conds.pre, e->hoare_conds.post);
	switch (e->type) {
	case CCLERICAL_EXPR_DECL_ASGN:
		for (size_t i=0; i<e->decl_asgn.inits.valid; i++) {
			const struct cclerical_decl_asgn *da =
				&e->decl_asgn.inits.data[i];
			fprintf(stderr, "%*sinit %zu: setting var #%zu to\n",
			        lvl, "", i, da->id);
			pexpr(da->init, lvl+1);
		}
		fprintf(stderr, "%*sdecl-asgn computing\n", lvl, "");
		pexpr(e->decl_asgn.body, lvl+1);
		fprintf(stderr, "%*s decl-asgn done\n", lvl, "");
		break;
	case CCLERICAL_EXPR_OP: {
		fprintf(stderr, "%*sop: %s\n", lvl, "", ops[e->op.op]);
		for (unsigned i=0; i<cclerical_op_arity(e->op.op); i++)
			pexpr(e->op.args[i], lvl+1);
		break;
	}
	case CCLERICAL_EXPR_CNST: {
		fprintf(stderr, "%*scnst: ", lvl, "");
		pcnst(stderr, &e->cnst);
		fprintf(stderr, "\n");
		break;
	}
	case CCLERICAL_EXPR_IF:
		fprintf(stderr, "%*sif cond:\n", lvl, "");
		pexpr(e->branch.cond, lvl+1);
		fprintf(stderr, "%*sif true branch:\n", lvl, "");
		pexpr(e->branch.if_true, lvl+1);
		if (e->branch.if_false) {
			fprintf(stderr, "%*sif false branch:\n", lvl, "");
			pexpr(e->branch.if_false, lvl+1);
		}
		break;
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
		pexpr(e->lim.seq, lvl+1);
		break;
	case CCLERICAL_EXPR_CASE:
		for (size_t i=0; i<e->cases.valid; i++) {
			fprintf(stderr, "%*scase %zu:\n", lvl, "", i);
			pexpr(e->cases.data[i].cond, lvl+1);
			pexpr(e->cases.data[i].body, lvl+1);
		}
		break;
	case CCLERICAL_EXPR_SKIP:
		break;
	case CCLERICAL_EXPR_ASGN:
		fprintf(stderr, "%*sasgn var #%zu from\n", lvl, "", e->asgn.var);
		pexpr(e->asgn.expr, lvl+1);
		break;
	case CCLERICAL_EXPR_WHILE:
		fprintf(stderr, "%*swhile cond:\n", lvl, "");
		pexpr(e->loop.cond, lvl+1);
		fprintf(stderr, "%*swhile body:\n", lvl, "");
		pexpr(e->loop.body, lvl+1);
		break;
	case CCLERICAL_EXPR_SEQ:
		pprog(e->seq, lvl+1);
		break;
	}
}

static void pprog(const struct cclerical_prog *p, int lvl)
{
	fprintf(stderr, "%*sprogram:\n", lvl, "");
	for (size_t i=0; i<p->exprs.valid; i++)
		pexpr(p->exprs.data[i], lvl+1);
}

void cclprintf(FILE *f, int lvl, const char *fmt, ...)
{
	while (lvl--)
		fprintf(f, "\t");
	va_list ap;
	va_start(ap,fmt);
	vfprintf(f, fmt, ap);
	va_end(ap);
}

static const char *const CCL_TYPE_STR[] = {
	[CCL_TYPE_UNIT]     = "Unit",
	[CCL_TYPE_BOOL]     = "Bool",
	[CCL_TYPE_INT ]     = "Int",
	[CCL_TYPE_REAL]     = "Real",
	[CCL_TYPE_KLEENEAN] = "Kleenean",
};

static const char *const CCL_UNIT_STR[] = {
	[CCL_UNIT_STAR] = "*",
};

static const char *const CCL_KLEENEAN_STR[] = {
	[CCL_KLEENEAN_FALSE] = "false",
	[CCL_KLEENEAN_TRUE ] = "true",
	[CCL_KLEENEAN_BOT  ] = "bot",
};

static void ccl_cfg_dump_decl(FILE *out, const struct ccl_tu *tu,
                              ccl_decl_id_t id, int lvl)
{
	const struct ccl_decl *d = tu->decl_storage.data[id.id];
	cclprintf(out, lvl,
	          "decl #%zu: value_type: %s, constant: %d, org_artificial: %d",
	          id.id, CCL_TYPE_STR[d->value_type], d->is_constant,
	          d->origin_is_artificial);
	if (d->is_constant && d->origin_is_artificial) {
		const char *ts = NULL;
		switch (d->value_type) {
		case CCL_TYPE_UNIT:
			ts = CCL_UNIT_STR[d->art_cnst_value.unit];
			break;
		case CCL_TYPE_BOOL:
		case CCL_TYPE_INT:
		case CCL_TYPE_REAL:
			break;
		case CCL_TYPE_KLEENEAN:
			ts = CCL_KLEENEAN_STR[d->art_cnst_value.kleenean];
			break;
		}
		cclprintf(out, 0, ", org-imm: %s", ts);
	} else if (d->is_constant) {
		cclprintf(out, 0, ", org-cnst: ");
		pcnst(out, d->origin.cnst);
	} else if (d->origin_is_artificial) {
		cclprintf(out, 0, ", org-insn: #%zu", d->origin.art);
	} else {
		const struct cclerical_decl *dd = d->origin.org;
		cclprintf(out, 0, ", org-decl: %s, type: %s, id: %s",
		          dd->type == CCLERICAL_DECL_VAR ? "var" : "fun",
		          CCLERICAL_TYPE_STR[dd->value_type], dd->id);
		if (dd->type == CCLERICAL_DECL_FUN &&
		    !cclerical_decl_fun_is_external(dd))
			cclprintf(out, 0, ", f#%zu", d->fun_id.id);
	}
	cclprintf(out, 0, "\n");
}

static void ccl_cfg_dump_insn_asgn(FILE *out, const struct ccl_tu *tu,
                                   const struct ccl_insn_asgn *a, int lvl)
{
	cclprintf(out, 0, "asgn decl #%zu <- ", a->lhs.id);
	switch (a->type) {
	case CCL_INSN_ASGN_ALIAS:
		cclprintf(out, 0, "alias decl #%zu, args: ", a->alias.decl.id);
		for (size_t i=0; i<a->alias.args.valid; i++) {
			ccl_decl_id_t d = { .id = (uintptr_t)a->alias.args.data[i] };
			cclprintf(out, 0, "%sdecl #%zu", i ? ", " : "", d.id);
		}
		break;
	case CCL_INSN_ASGN_LIM:
		cclprintf(out, 0, "lim decl #%zu -> insn #%zu",
		          a->lim.seq_idx.id, a->lim.body.id);
		break;
	case CCL_INSN_ASGN_OP:
		cclprintf(out, 0, "op %s, args: ", ops[a->op.op]);
		for (size_t i=0; i<cclerical_op_arity(a->op.op); i++)
			cclprintf(out, 0, "%sdecl #%zu", i ? ", " : "",
			          a->op.args[i].id);
	}
	cclprintf(out, 0, "; next insn #%zu\n", a->next.id);
}

static void ccl_cfg_dump_insn(FILE *out, const struct ccl_tu *tu,
                              ccl_insn_id_t id, int lvl)
{
	const struct ccl_insn *in = tu->insn_storage.data[id.id];
	cclprintf(out, 0, "insn #%zu: ", id.id);
	switch (in->type) {
	case CCL_INSN_ASGN:
		ccl_cfg_dump_insn_asgn(out, tu, &in->asgn, lvl+1);
		break;
	case CCL_INSN_CASE:
		cclprintf(out, 0, "%zu cases\n", in->cases.conds.valid);
		for (size_t i=0; i<in->cases.conds.valid; i++) {
			ccl_decl_id_t c = { .id = (uintptr_t)in->cases.conds.data[i] };
			ccl_insn_id_t b = { .id = (uintptr_t)in->cases.bodies.data[i] };
			cclprintf(out, lvl+1, "case decl #%zu -> insn #%zu\n",
			          c.id, b.id);
		}
		break;
	case CCL_INSN_IF:
		cclprintf(out, 0,
		          "if: decl #%zu -> insn #%zu; else insn #%zu\n",
		          in->branch.cond.id,
		          in->branch.if_true.id, in->branch.if_false.id);
		break;
	case CCL_INSN_WHILE:
		cclprintf(out, 0,
		          "while: decl #%zu -> insn #%zu; next: insn #%zu\n",
		          in->loop.cond.id, in->loop.body.id, in->loop.next.id);
		break;
	case CCL_INSN_RETURN:
		cclprintf(out, 0, "return: decl #%zu\n", in->retval.id);
		break;
	}
}

static void ccl_cfg_dump(FILE *out, const struct cc_opts *opts,
                         const struct cclerical_input *input,
                         const struct ccl_tu *tu, const struct ccl_cfg_bb *cbb)
{
	(void)input;
	cclprintf(out, 0, "cfg decls:\n");
	for (size_t i=0; i<tu->decl_storage.valid; i++)
		ccl_cfg_dump_decl(out, tu, (ccl_decl_id_t){ .id = i }, 1);
	cclprintf(out, 0, "cfg insns:\n");
	for (size_t i=0; i<tu->insn_storage.valid; i++) {
		ccl_insn_id_t in_id = { .id = i };
		ccl_bb_id_t bb_id = { .id = (uintptr_t)cbb->entries.data[in_id.id] };
		cclprintf(out, 0, "bb#%zu:", bb_id.id);
		int indent = 2;
		for (size_t j=0; j<tu->fun_storage.valid; j++) {
			const struct ccl_fun *f = tu->fun_storage.data[j];
			if (f->body.id == in_id.id)
				cclprintf(out, --indent, "f#%zu:", j);
		}
		cclprintf(out, indent, "");
		ccl_cfg_dump_insn(out, tu, (ccl_insn_id_t){ .id = i }, 2);
		if (opts->verbosity > 0) {
			const struct ccl_insn *t = tu->insn_storage.data[i];
			cclerical_highlight(out, CCLERICAL_HIGHLIGHT_AUTO,
			                    input, &t->source_loc);
		}
	}
	cclprintf(out, 0, "bb graph:\n");
	for (size_t i=0; i<cbb->bb_storage.valid; i++) {
		const struct ccl_basic_block *bb = cbb->bb_storage.data[i];
		if (!bb->insns.valid)
			continue;
		cclprintf(out, 1, "bb#%zu:\t", i);
		for (size_t j=0; j<bb->insns.valid; j++) {
			ccl_insn_id_t in = { .id = (uintptr_t)bb->insns.data[j] };
			cclprintf(out, 0, "%s%zu", j ? ", " : "", in.id);
		}
		cclprintf(out, 0, "\n");
		cclprintf(out, 2, "bb-in: ");
		for (size_t j=0; j<bb->in.valid; j++) {
			ccl_bb_id_t pred = { .id = (uintptr_t)bb->in.data[j] };
			cclprintf(out, 0, "%s%zu", j ? ", " : "", pred.id);
		}
		cclprintf(out, 0, "\n");
		cclprintf(out, 2, "bb-out: ");
		for (size_t j=0; j<bb->out.valid; j++) {
			ccl_bb_id_t succ = { .id = (uintptr_t)bb->out.data[j] };
			cclprintf(out, 0, "%s%zu", j ? ", " : "", succ.id);
		}
		cclprintf(out, 0, "\n");
	}
}

static void export_ssa(FILE *out, const struct cc_opts *opts,
                       const struct cclerical_input *input,
                       const struct cclerical_prog *p,
                       const struct cclerical_vec_decl_ptr *decls)
{
	struct ccl_tu tu = CCL_TU_INIT;
	ccl_tu_init(&tu, decls);
	if (p) {
		struct cclerical_source_loc dummy = { 0, 0, 0, 0 };
		ccl_cfg_add(&tu, p, dummy);
	}

	struct ccl_cfg_bb cbb;
	ccl_cfg_bb_init(&cbb, &tu);
	ccl_cfg_dump(out, opts, input, &tu, &cbb);

	// ccl_tu_fini(&tu);
}

static struct compiler {
	const char *id;
	backend_f *compile;
} const COMPILERS[] = {
	{ "iRRAM", export_irram, },
	{ "ssa", export_ssa, },
	{ NULL, NULL, }
};

struct feat_value {
	const char *value;
	const char *help_msg;
};

static const struct feat_value FEAT_CFG_COMPACT_STRS[] = {
	[OPT_FEAT_CFG_COMPACT_ALIAS] = {
		.value = "alias",
		.help_msg = "compact alias assignments\n",
	},
	{ NULL, NULL, },
};

static struct {
	const char *feat;
	const struct feat_value *values;
	const char *help_msg;
} const FEAT_OPTS[] = {
	[OPT_FEAT_CFG_COMPACT] = {
		"cfg-compact",
		FEAT_CFG_COMPACT_STRS,
		"apply transformation passes on the control flow graph\n"
	},
};

static int compile_t17(const struct cclerical_input *in,
                       const struct compiler *cc, const struct cc_opts *opts)
{
	struct cclerical_parser p;
	yyscan_t scanner;
	cclerical_lex_init(&scanner);
	YY_BUFFER_STATE st = cclerical__scan_bytes(in->data, in->size, scanner);
	cclerical_parser_init(&p, in);
	int r = cclerical_parse(&p, scanner);
	if (r) {
		fprintf(stderr, "parse error %d, aborting\n", r);
		goto done;
	}

	if (opts->dump_parse_tree) {
		fprintf(stderr, "%s:\n", in->name);
		pprog(p.prog, 0);
	}
	cc->compile(opts->output, opts, in, p.prog, &p.decls);

done:
	cclerical_parser_fini(&p);
	cclerical__delete_buffer(st, scanner);
	cclerical_lex_destroy(scanner);
	return r;
}

static void input_munmap(struct cclerical_input *in)
{
	munmap(in->data, in->size);
}

static void input_free(struct cclerical_input *in)
{
	free(in->data);
}

static void open_input(FILE *f, struct cclerical_input *in)
{
	struct stat st;
	if (fstat(fileno(f), &st) == -1)
		DIE(1,"error stat'ing %s: %s\n", in->name, strerror(errno));
	if (S_ISREG(st.st_mode)) {
		in->size = st.st_size;
		in->data = mmap(NULL, st.st_size, PROT_READ | PROT_WRITE,
		                MAP_PRIVATE, fileno(f), 0);
		in->fini = input_munmap;
		if (in->data != MAP_FAILED)
			return;
	}
	if (S_ISREG(st.st_mode) || S_ISFIFO(st.st_mode) || S_ISCHR(st.st_mode)) {
		static char buf[1U << 12];
		in->size = 0;
		in->data = NULL;
		in->fini = input_free;
		size_t sz = 0;
		for (size_t rd; (rd = fread(buf, 1, sizeof(buf), f)) > 0;) {
			in->size += rd;
			if (in->size+rd > sz)
				in->data = realloc(in->data,
				                   sz = MAX(in->size+rd, 2*sz));
			memcpy((char *)in->data + in->size, buf, rd);
			in->size += rd;
			if (rd < sizeof(buf))
				break;
		}
		if (feof(f))
			return;
		in->fini(in);
		int r = ferror(f);
		if (r)
			DIE(1,"error reading %s: %s\n", in->name, strerror(r));
	}
	DIE(1,"error reading %s: not a regular file or FIFO\n",in->name);
}

static void parse_feat(struct cc_opts *opts, const char *f)
{
	const char *eq = strchr(f, '=');
	if (!eq)
		eq = f + strlen(f);
	for (size_t i=0; i<ARRAY_SIZE(FEAT_OPTS); i++) {
		const struct feat_value *v = FEAT_OPTS[i].values;
		int disabled = !strncmp(f, "no-", 3);
		const char *g = disabled ? f + 3 : f;
		if (strncmp(g, FEAT_OPTS[i].feat, eq-g))
			continue;
		if (!eq && !v) {
			opts->feat[i] = !disabled;
			return;
		}
		if (eq && v) {
			for (size_t j=0; v->value; j++, v++)
				if (!strcmp(eq+1, v->value)) {
					if (disabled)
						opts->feat[i] &= ~(1U << j);
					else
						opts->feat[i] |= 1U << j;
					return;
				}
		}
		if (v) {
			fprintf(stderr, "error parsing option '-f%s': "
			                "must be = one of ", f);
			v = FEAT_OPTS[i].values;
			for (size_t j=0; v->value; j++, v++)
				fprintf(stderr, "%s'%s'", j ? ", " : "",
				        v->value);
			DIE(1,"\n");
		} else
			DIE(1,"error parsing option '-f%s': "
			      "can only be set or unset", f);
	}
	fprintf(stderr, "unknown feature option '-f%s', available are:\n", f);
	for (size_t i=0; i<ARRAY_SIZE(FEAT_OPTS); i++) {
		int n = fprintf(stderr, "\n  -f%s=", FEAT_OPTS[i].feat) - 1;
		const struct feat_value *v = FEAT_OPTS[i].values;
		fprintf(stderr, "%*s  %s", 24-n, "", FEAT_OPTS[i].help_msg);
		if (!v)
			continue;
		for (; v->value; v++) {
			n = fprintf(stderr, "      %s", v->value);
			fprintf(stderr, "%*s  %s", 26-n, "", v->help_msg);
		}
	}
	exit(1);
}

CCLERICAL_VECTOR_DEF(ccl_vec_input,struct cclerical_input)

int main(int argc, char **argv)
{
	struct cc_opts opts = CC_OPTS_INIT;
	const struct compiler *cc = NULL;
	const char *output = NULL;

	for (int opt; (opt = getopt(argc, argv, ":b:df:ho:vx:")) != -1;)
		switch (opt) {
		case 'b':
			for (cc = COMPILERS; cc->id; cc++)
				if (!strcmp(cc->id, optarg))
					break;
			if (cc->id)
				break;
			DIE(1,"error: TGT '%s' not supported for option '-b'\n",
			    optarg);
		case 'd': opts.dump_parse_tree = 1; break;
		case 'f':
			parse_feat(&opts, optarg);
			break;
		case 'h':
			printf("usage: %s [-OPTS] [--] [FILE...]\n", argv[0]);
			printf("\n\
Options [default]:\n\
  -b TGT     target backend TGT [iRRAM]; supported values for TGT: iRRAM\n\
  -d         dump parse tree to stderr for debugging purposes\n\
  -f FEAT    enable FEAT (to disable, prefix FEAT by 'no-')\n\
  -h         print this help message\n\
  -o OUTPUT  write compiled TGT source file to OUTPUT [stdout]\n\
  -v         increase verbosity\n\
  -x DIALECT choose cclerical DIALECT [T17]; supported: T17\n\
\n\
This program is distributed under BSD-3 license.\n\
Author: Franz Brausse <brausse@informatik.uni-trier.de>\n");
			exit(0);
		case 'o': output = optarg; break;
		case 'v': opts.verbosity++; break;
		case 'x':
			if (!strcmp(optarg, "T17")) break;
			DIE(1,"error: just TGT 'T17' supported for option "
			      "'-x'\n");
		case '?': DIE(1,"error: unknown option '-%c'\n",optopt);
		case ':': DIE(1,"error: option '-%c' requires a parameter\n",
		              optopt);
		}
	/* defaults */
	if (!cc)
		cc = COMPILERS;
	if (output && argc - optind >= 2)
		DIE(1,"error: -o can not be used with multiple input files\n");
	if (output && !(opts.output = fopen(output, "wb")))
		DIE(1,"error opening output file '%s': %s\n",
		    output, strerror(errno));

	if (opts.verbosity > 0)
		fprintf(stderr, "ccl v%s\n", CCL_VERSION_STR);

	/* of type struct cclerical_input * */
	struct ccl_vec_input inputs = CCLERICAL_VECTOR_INIT;
	if (argc == optind) {
		struct cclerical_input in = { .name = "<stdin>" };
		open_input(stdin, &in);
		ccl_vec_input_add(&inputs, in);
	}
	for (; optind < argc; optind++) {
		struct cclerical_input in = { .name = argv[optind] };
		FILE *f = fopen(in.name, "rb");
		if (!f)
			DIE(1,"error opening input file '%s': %s\n",
			    in.name, strerror(errno));
		open_input(f, &in);
		fclose(f);
		ccl_vec_input_add(&inputs, in);
	}

	int r = 0;
	for (size_t i=0; !r && i<inputs.valid; i++) {
		struct cclerical_input *in = &inputs.data[i];
		r = compile_t17(in, cc, &opts);
		in->fini(in);
	}
	cclerical_vector_fini(&inputs);
	fclose(opts.output);
	return r;
}
