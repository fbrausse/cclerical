/* SPDX short identifier: BSD-3-Clause */

#if _POSIX_C_SOURCE < 1
# undef _POSIX_C_SOURCE
# define _POSIX_C_SOURCE	1 /* isatty(3p), fileno(3p) */
#endif

#include <unistd.h>	/* isatty(3p) */

#include "cclerical.h"

#define MAX(a,b)	((a) < (b) ? (b) : (a))

#define CCL_VT100_CSI		"\x1b["
#define CCL_VT100_MODE(str)	CCL_VT100_CSI str "m"

#define CCL_VT100_RESET		"0"
#define CCL_VT100_BRIGHT	"1"
#define CCL_VT100_DIM		"2"
#define CCL_VT100_UNDERL	"4"
#define CCL_VT100_BLINK		"5"
#define CCL_VT100_REVERSE	"7"
#define CCL_VT100_HIDDEN	"8"
#define CCL_VT100_UNDERL_OFF	"24"
#define CCL_VT100_BLINK_OFF	"25"
#define CCL_VT100_REVERSE_OFF	"27"

#define CCL_VT100_COL_FG(c)	"3" c
#define CCL_VT100_COL_BG(c)	"4" c

#define CCL_VT100_BLACK		"0"
#define CCL_VT100_RED		"1"
#define CCL_VT100_GREEN		"2"
#define CCL_VT100_YELLOW	"3"
#define CCL_VT100_BLUE		"4"
#define CCL_VT100_MAGENTA	"5"
#define CCL_VT100_CYAN		"6"
#define CCL_VT100_WHITE		"7"

#define CCL_VT100_ERROR	\
	CCL_VT100_MODE(CCL_VT100_BRIGHT ";" CCL_VT100_COL_FG(CCL_VT100_RED))
#define CCL_VT100_MODE_DEFAULTS	\
	CCL_VT100_MODE(CCL_VT100_RESET)

static void highlight_ascii(FILE *out,
                            const char *begin_line, const char *begin_col,
                            const char *end_line, const char *end_col)
{
	for (; begin_line < begin_col; begin_line++)
		switch (*begin_line) {
		case '\r':
		case '\n':
		case '\t': fputc(*begin_line, out); break;
		default: fputc(' ', out); break;
		}
	/* TODO: underline with carets */
	fprintf(out, "%.*s", (int)(end_col - begin_col), begin_col);
	for (; end_col < end_line; end_col++)
		switch (*end_col) {
		case '\r':
		case '\n':
		case '\t': fputc(*end_col, out); break;
		default: fputc(' ', out); break;
		}
}

static void highlight_vt100(FILE *out,
                            const char *begin_line, const char *begin_col,
                            const char *end_line, const char *end_col)
{
	fprintf(out, "%.*s%s%.*s%s%.*s",
	        (int)(begin_col - begin_line), begin_line, CCL_VT100_ERROR,
	        (int)(end_col - begin_col), begin_col, CCL_VT100_MODE_DEFAULTS,
	        (int)(end_line - end_col), end_col);
}

void update_last_loc1(struct cclerical_source_loc *loc, char c);

void cclerical_highlight(FILE *out, enum cclerical_highlight_mode mode,
                         const struct cclerical_input *input,
                         const struct cclerical_source_loc *locp)
{
	struct cclerical_source_loc c = { 1, 1, 1, 1 };
	const char *s = input->data, *fini = s + input->size;
	while (c.last_line < locp->first_line && s < fini)
		update_last_loc1(&c, *s++);
	if (s >= fini)
		return;
	const char *begin_line = s;
	const char *begin_col = NULL, *end_col = NULL;
	while (c.last_line <= locp->last_line && s < fini) {
		if (c.last_line == locp->first_line &&
		    c.last_column == locp->first_column)
			begin_col = s;
		if (c.last_line == locp->last_line &&
		    c.last_column == locp->last_column)
			end_col = s;
		update_last_loc1(&c, *s++);
	}
	if (!begin_col)
		begin_col = s;
	if (!end_col)
		end_col = s;
	const char *end_line = s;

	if (mode == CCLERICAL_HIGHLIGHT_AUTO && isatty(fileno(out)))
		mode = CCLERICAL_HIGHLIGHT_VT100;
	switch (mode) {
	case CCLERICAL_HIGHLIGHT_AUTO: /* fall through */
	case CCLERICAL_HIGHLIGHT_ASCII:
		highlight_ascii(out, begin_line, begin_col, end_line, end_col);
		break;
	case CCLERICAL_HIGHLIGHT_VT100:
		highlight_vt100(out, begin_line, begin_col, end_line, end_col);
		break;
	}
}

const char *const CCLERICAL_TYPE_STR[] = {
	[CCLERICAL_TYPE_UNIT] = "Unit",
	[CCLERICAL_TYPE_BOOL] = "Bool",
	[CCLERICAL_TYPE_INT ] = "Int",
	[CCLERICAL_TYPE_REAL] = "Real"
};

int cclerical_op_is_unary(enum cclerical_op op)
{
	return op == CCLERICAL_OP_NEG || op == CCLERICAL_OP_NOT;
}

unsigned cclerical_op_arity(enum cclerical_op op)
{
	return cclerical_op_is_unary(op) ? 1 : 2;
}

struct cclerical_expr * cclerical_expr_create(enum cclerical_expr_type type)
{
	struct cclerical_expr *e = malloc(sizeof(struct cclerical_expr));
	e->type = type;
	e->hoare_conds.pre = NULL;
	e->hoare_conds.post = NULL;
	return e;
}

struct cclerical_expr * cclerical_expr_create_op(enum cclerical_op op,
                                                 struct cclerical_expr *a,
                                                 struct cclerical_expr *b)
{
	struct cclerical_expr *e = cclerical_expr_create(CCLERICAL_EXPR_OP);
	e->op.args[0] = a;
	e->op.args[1] = b;
	e->op.op = op;
	return e;
}

enum cclerical_type cclerical_prog_type(const struct cclerical_prog *p)
{
	struct cclerical_expr *last = cclerical_vec_expr_ptr_last(&p->exprs);
	return last->result_type;
}

void cclerical_constant_fini(struct cclerical_constant *c)
{
	switch (c->type) {
	case CCLERICAL_TYPE_UNIT:
	case CCLERICAL_TYPE_BOOL:
		break;
	case CCLERICAL_TYPE_INT:
	case CCLERICAL_TYPE_REAL:
		free(c->numeric.str);
		break;
	}
}

void cclerical_expr_destroy(struct cclerical_expr *e)
{
	switch (e->type) {
	case CCLERICAL_EXPR_VAR:
		break;
	case CCLERICAL_EXPR_FUN_CALL:
		for (size_t i=0; i<e->fun_call.params.valid; i++) {
			struct cclerical_expr *f = e->fun_call.params.data[i];
			cclerical_expr_destroy(f);
		}
		cclerical_vector_fini(&e->fun_call.params);
		break;
	case CCLERICAL_EXPR_CNST:
		cclerical_constant_fini(&e->cnst);
		break;
	case CCLERICAL_EXPR_CASE:
		cclerical_cases_fini(&e->cases);
		break;
	case CCLERICAL_EXPR_IF:
		cclerical_expr_destroy(e->branch.cond);
		cclerical_expr_destroy(e->branch.if_true);
		if (e->branch.if_false)
			cclerical_expr_destroy(e->branch.if_false);
		break;
	case CCLERICAL_EXPR_DECL_ASGN:
		for (size_t i=0; i<e->decl_asgn.inits.valid; i++) {
			struct cclerical_expr *f = e->decl_asgn.inits.data[i].init;
			cclerical_expr_destroy(f);
		}
		cclerical_vector_fini(&e->decl_asgn.inits);
		cclerical_expr_destroy(e->decl_asgn.body);
		break;
	case CCLERICAL_EXPR_LIM:
		cclerical_expr_destroy(e->lim.seq);
		cclerical_scope_fini(&e->lim.local);
		break;
	case CCLERICAL_EXPR_OP:
		for (unsigned i=0; i<cclerical_op_arity(e->op.op); i++)
			cclerical_expr_destroy(e->op.args[i]);
		break;
	case CCLERICAL_EXPR_SKIP:
		break;
	case CCLERICAL_EXPR_ASGN:
		cclerical_expr_destroy(e->asgn.expr);
		break;
	case CCLERICAL_EXPR_WHILE:
		cclerical_expr_destroy(e->loop.cond);
		cclerical_expr_destroy(e->loop.body);
		break;
	case CCLERICAL_EXPR_SEQ:
		cclerical_prog_destroy(e->seq);
		break;
	}
	free(e->hoare_conds.pre);
	free(e->hoare_conds.post);
	free(e);
}

struct cclerical_prog * cclerical_prog_create(void)
{
	struct cclerical_prog *p = calloc(1, sizeof(struct cclerical_prog));
	return p;
}

void cclerical_prog_destroy(struct cclerical_prog *p)
{
	for (size_t i=0; i<p->exprs.valid; i++) {
		struct cclerical_expr *e = p->exprs.data[i];
		cclerical_expr_destroy(e);
	}
	cclerical_vector_fini(&p->exprs);
	free(p);
}

void cclerical_cases_fini(const struct cclerical_vec_case *c)
{
	for (size_t i=0; i<c->valid; i++) {
		const struct cclerical_case *cc = &c->data[i];
		cclerical_expr_destroy(cc->cond);
		cclerical_expr_destroy(cc->body);
	}
	cclerical_vector_fini(c);
}

void cclerical_scope_fini(const struct cclerical_scope *s)
{
	cclerical_vector_fini(&s->var_idcs);
}

/* -------------------------------------------------------------------------- */

void cclerical_parser_init(struct cclerical_parser *p,
                           const struct cclerical_input *input)
{
	memset(p, 0, sizeof(*p));
	cclerical_parser_open_scope(p, 0, 0);
	p->input = input;
}

static int lookup(const struct cclerical_parser *p,
                  size_t scope_idx, const char *id,
                  cclerical_id_t *ridx, size_t *rscope, int rw)
{
	const struct cclerical_parser_scope *s = p->scopes.data[scope_idx];
	if (!rw || !s->this_read_only) {
		const struct cclerical_vec_id_t *vi = &s->scope.var_idcs;
		for (size_t i=0; i<vi->valid; i++) {
			size_t idx = (uintptr_t)vi->data[i];
			const struct cclerical_decl *v = p->decls.data[idx];
			if (!strcmp(v->id, id)) {
				if (ridx)
					*ridx = idx;
				if (rscope)
					*rscope = scope_idx;
				return 1;
			}
		}
	}
	return (!scope_idx || (rw && s->prev_read_only))
	       ? 0 : lookup(p, scope_idx-1, id, ridx, rscope, rw);
}

int cclerical_parser_var_lookup(struct cclerical_parser *p, const char *id,
                                cclerical_id_t *v, size_t *scope_idx, int rw)
{
	return p->scopes.valid && lookup(p, p->scopes.valid-1, id, v, scope_idx, rw);
}

int cclerical_parser_new_decl(struct cclerical_parser *p,
                              const struct cclerical_decl *decl,
                              cclerical_id_t *v)
{
	if (cclerical_parser_var_lookup(p, decl->id, v, NULL, 0))
		return 0;
	size_t idx = p->decls.valid;
	cclerical_vec_decl_ptr_add(&p->decls, memdup(decl, sizeof(*decl)));
	struct cclerical_parser_scope *s = cclerical_vec_scope_ptr_last(&p->scopes);
	cclerical_vec_id_t_add(&s->scope.var_idcs, idx);
	if (v)
		*v = idx;
	return 1;
}

void cclerical_parser_open_scope(struct cclerical_parser *p, int ro,
                                 int parents_ro)
{
	struct cclerical_parser_scope ns = {
		.scope = { .var_idcs = CCLERICAL_VECTOR_INIT },
		.this_read_only = ro,
		.prev_read_only = parents_ro,
	};
	cclerical_vec_scope_ptr_add(&p->scopes, memdup(&ns, sizeof(ns)));
}

struct cclerical_scope cclerical_parser_close_scope(struct cclerical_parser *p)
{
	struct cclerical_parser_scope *s = p->scopes.data[--p->scopes.valid];
	struct cclerical_scope scope = s->scope;
	free(s);
	return scope;
}

void cclerical_decl_fini(const struct cclerical_decl *d)
{
	free(d->id);
	switch (d->type) {
	case CCLERICAL_DECL_VAR:
		break;
	case CCLERICAL_DECL_FUN:
		if (cclerical_decl_fun_is_external(d))
			cclerical_vector_fini(&d->fun.arguments.types);
		else
			cclerical_vector_fini(&d->fun.arguments.ids);
		if (d->fun.body)
			cclerical_prog_destroy(d->fun.body);
		break;
	}
}

void cclerical_decl_destroy(struct cclerical_decl *d)
{
	cclerical_decl_fini(d);
	free(d);
}

void cclerical_parser_fini(struct cclerical_parser *p)
{
	while (p->scopes.valid) {
		struct cclerical_scope sc = cclerical_parser_close_scope(p);
		cclerical_scope_fini(&sc);
	}
	cclerical_vector_fini(&p->scopes);
	for (size_t i=0; i<p->decls.valid; i++) {
		struct cclerical_decl *d = p->decls.data[i];
		cclerical_decl_destroy(d);
	}
	cclerical_vector_fini(&p->decls);
	if (p->prog)
		cclerical_prog_destroy(p->prog);
}
