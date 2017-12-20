/* SPDX short identifier: BSD-3-Clause */
#include "cclerical.h"

#define MAX(a,b)	((a) < (b) ? (b) : (a))

const char *const CCLERICAL_TYPE_STR[] = {
	[CCLERICAL_TYPE_UNIT] = "Unit",
	[CCLERICAL_TYPE_BOOL] = "Bool",
	[CCLERICAL_TYPE_INT ] = "Int",
	[CCLERICAL_TYPE_REAL] = "Real"
};

static inline void cclerical_vector_ensure_size(struct cclerical_vector *v, size_t n)
{
	if (v->size < n &&
	    !(v->data = realloc(v->data, sizeof(*v->data)*(v->size = MAX(n, v->valid*2)))))
		abort();
}

void cclerical_vector_add(struct cclerical_vector *v, void *it)
{
	cclerical_vector_ensure_size(v, v->valid+1);
	v->data[v->valid++] = it;
}

void cclerical_vector_fini(const struct cclerical_vector *v)
{
	free(v->data);
}

int cclerical_op_is_unary(enum cclerical_op op)
{
	return op == CCLERICAL_OP_NEG || op == CCLERICAL_OP_NOT;
}

struct cclerical_expr * cclerical_expr_create(enum cclerical_expr_type type)
{
	struct cclerical_expr *e = malloc(sizeof(struct cclerical_expr));
	e->type = type;
	return e;
}

struct cclerical_expr * cclerical_expr_create_op(enum cclerical_op op,
                                               struct cclerical_expr *a,
                                               struct cclerical_expr *b)
{
	struct cclerical_expr *e = cclerical_expr_create(CCLERICAL_EXPR_OP);
	e->op.arg1 = a;
	e->op.arg2 = b;
	e->op.op = op;
	return e;
}

enum cclerical_type cclerical_prog_type(const struct cclerical_prog *p)
{
	struct cclerical_stmt *last = cclerical_vector_last(&p->stmts);
	if (last->type != CCLERICAL_STMT_EXPR)
		return CCLERICAL_TYPE_UNIT;
	return last->expr->result_type;
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
		for (size_t i=0; i<e->decl_asgn.inits.valid; i+=2) {
			struct cclerical_expr *f = e->decl_asgn.inits.data[i+1];
			cclerical_expr_destroy(f);
		}
		cclerical_expr_destroy(e->decl_asgn.prog);
		break;
	case CCLERICAL_EXPR_LIM:
		cclerical_expr_destroy(e->lim.seq);
		cclerical_scope_fini(&e->lim.local);
		break;
	case CCLERICAL_EXPR_OP:
		cclerical_expr_destroy(e->op.arg1);
		if (!cclerical_op_is_unary(e->op.op))
			cclerical_expr_destroy(e->op.arg2);
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
	free(e);
}

struct cclerical_stmt * cclerical_stmt_create(enum cclerical_stmt_type type)
{
	struct cclerical_stmt *e = malloc(sizeof(struct cclerical_stmt));
	e->type = type;
	return e;
}

void cclerical_stmt_destroy(struct cclerical_stmt *s)
{
	switch (s->type) {
	case CCLERICAL_STMT_EXPR: cclerical_expr_destroy(s->expr); break;
	}
	free(s);
}

struct cclerical_prog * cclerical_prog_create(void)
{
	struct cclerical_prog *p = calloc(1, sizeof(struct cclerical_prog));
	return p;
}

void cclerical_prog_destroy(struct cclerical_prog *p)
{
	for (size_t i=0; i<p->stmts.valid; i++) {
		struct cclerical_stmt *s = p->stmts.data[i];
		cclerical_stmt_destroy(s);
	}
	cclerical_vector_fini(&p->stmts);
	free(p);
}

void cclerical_cases_fini(const struct cclerical_vector *c)
{
	for (size_t i=0; i<c->valid; i+=2) {
		cclerical_expr_destroy(c->data[i]);
		cclerical_expr_destroy(c->data[i+1]);
	}
	cclerical_vector_fini(c);
}

void cclerical_scope_fini(const struct cclerical_scope *s)
{
	cclerical_vector_fini(&s->var_idcs);
}

/* -------------------------------------------------------------------------- */

void cclerical_parser_init(struct cclerical_parser *p)
{
	memset(p, 0, sizeof(*p));
	cclerical_parser_open_scope(p, 0, 0);
}

static int lookup(const struct cclerical_parser *p,
                  size_t scope_idx, const char *id,
                  cclerical_id_t *ridx, size_t *rscope, int rw)
{
	const struct cclerical_parser_scope *s = p->scopes.data[scope_idx];
	if (!rw || !s->this_read_only) {
		const struct cclerical_vector *vi = &s->scope.var_idcs;
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
	cclerical_vector_add(&p->decls, memdup(decl, sizeof(*decl)));
	struct cclerical_parser_scope *s = cclerical_vector_last(&p->scopes);
	cclerical_vector_add(&s->scope.var_idcs, (void *)(uintptr_t)idx);
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
	cclerical_vector_add(&p->scopes, memdup(&ns, sizeof(ns)));
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
		cclerical_vector_fini(&d->fun.arguments);
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
	for (size_t i=0; i<p->decls.valid; i++) {
		struct cclerical_decl *d = p->decls.data[i];
		cclerical_decl_destroy(d);
	}
	cclerical_vector_fini(&p->decls);
	if (p->prog)
		cclerical_prog_destroy(p->prog);
}
