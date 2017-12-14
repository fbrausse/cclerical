
#include "cclerical.h"

#define MAX(a,b)	((a) < (b) ? (b) : (a))

const char *const CCLERICAL_TYPE_STR[] = {
	[CCLERICAL_TYPE_UNIT] = "Unit",
	[CCLERICAL_TYPE_BOOL] = "Bool",
	[CCLERICAL_TYPE_INT ] = "Int",
	[CCLERICAL_TYPE_REAL] = "Real"
};

static inline void * memdup(const void *src, size_t n)
{
	return memcpy(malloc(n), src, n);
}

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

static void * cclerical_vector_last(const struct cclerical_vector *v)
{
	return v->valid ? v->data[v->valid-1] : NULL;
}

void cclerical_vector_fini(const struct cclerical_vector *v)
{
	free(v->data);
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
	if (!last || last->type != CCLERICAL_STMT_EXPR)
		return CCLERICAL_TYPE_UNIT;
	return last->expr->result_type;
}

void cclerical_constant_fini(struct cclerical_constant *c)
{
	switch (c->lower_type) {
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
	case CCLERICAL_EXPR_VAR: break;
	case CCLERICAL_EXPR_FUN_CALL:
		for (size_t i=0; i<e->fun_call.params.valid; i++) {
			struct cclerical_expr *f = e->fun_call.params.data[i];
			cclerical_expr_destroy(f);
		}
		break;
	case CCLERICAL_EXPR_CNST:
		cclerical_constant_fini(&e->cnst);
		break;
	case CCLERICAL_EXPR_CASE: cclerical_cases_fini(&e->cases); break;
	case CCLERICAL_EXPR_DECL_ASGN:
		cclerical_expr_destroy(e->decl_asgn.expr);
		cclerical_prog_destroy(e->decl_asgn.prog);
		break;
	case CCLERICAL_EXPR_LIM:
		cclerical_prog_destroy(e->lim.seq);
		cclerical_scope_fini(&e->lim.local);
		break;
	case CCLERICAL_EXPR_OP:
		cclerical_expr_destroy(e->op.arg1);
		cclerical_expr_destroy(e->op.arg2);
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
	case CCLERICAL_STMT_SKIP: break;
	case CCLERICAL_STMT_ASGN: cclerical_expr_destroy(s->asgn.expr); break;
	case CCLERICAL_STMT_IF:
		cclerical_expr_destroy(s->branch.cond);
		cclerical_prog_destroy(s->branch.if_true);
		cclerical_prog_destroy(s->branch.if_false);
		break;
	case CCLERICAL_STMT_WHILE:
		cclerical_expr_destroy(s->loop.cond);
		cclerical_prog_destroy(s->loop.body);
		break;
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
		cclerical_prog_destroy(c->data[i+1]);
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
}

static
const int cclerical_parser_var_lookup0(const struct cclerical_parser *p,
                                      const struct cclerical_parser_scope *s,
                                      const char *id, cclerical_id_t *ridx,
                                      int rw)
{
	if (!s)
		return 0;
	const struct cclerical_vector *vi = &s->scope.var_idcs;
	for (size_t i=0; i<vi->valid; i++) {
		size_t idx = (uintptr_t)vi->data[i];
		const struct cclerical_decl *v = p->decls.data[idx];
		if (!strcmp(v->id, id)) {
			*ridx = idx;
			return 1;
		}
	}
	return rw ? 0 : cclerical_parser_var_lookup0(p, s->parent, id, ridx, rw);
}

int cclerical_parser_var_lookup(struct cclerical_parser *p, const char *id,
                               cclerical_id_t *v, int rw)
{
	return cclerical_parser_var_lookup0(p, &p->scope, id, v, rw);
}

static int cclerical_parser_new_decl(struct cclerical_parser *p,
                                     const struct cclerical_decl *decl,
                                     cclerical_id_t *v)
{
	cclerical_id_t exists;
	if (cclerical_parser_var_lookup(p, decl->id, &exists, 0))
		return EEXIST;
	size_t idx = p->decls.valid;
	cclerical_vector_add(&p->decls, memdup(decl, sizeof(*decl)));
	cclerical_vector_add(&p->scope.scope.var_idcs, (void *)(uintptr_t)idx);
	if (v)
		*v = idx;
	return 0;
}

int cclerical_parser_new_var(struct cclerical_parser *p, char *id,
                             enum cclerical_type type, cclerical_id_t *v)
{
	struct cclerical_decl d = {
		.type = CCLERICAL_DECL_VAR,
		.id = id,
		.var = {
			.type = type,
		},
	};
	return cclerical_parser_new_decl(p, &d, v);
}

int cclerical_parser_new_fun(struct cclerical_parser *p,
                             char *id, struct cclerical_vector arguments,
                             struct cclerical_prog *body, cclerical_id_t *v)
{
	struct cclerical_decl d = {
		.type = CCLERICAL_DECL_FUN,
		.id = id,
		.fun = {
			.arguments = arguments,
			.body = body,
		},
	};
	return cclerical_parser_new_decl(p, &d, v);
}

void cclerical_parser_open_scope(struct cclerical_parser *p)
{
	struct cclerical_parser_scope *s;
	s = malloc(sizeof(struct cclerical_parser_scope));
	*s = p->scope;
	p->scope.parent = s;
	memset(&p->scope.scope, 0, sizeof(p->scope.scope));
}

struct cclerical_scope cclerical_parser_close_scope(struct cclerical_parser *p)
{
	struct cclerical_scope scope = p->scope.scope;
	struct cclerical_parser_scope *s = p->scope.parent;
	p->scope = *s;
	free(s);
	return scope;
}

void cclerical_decl_destroy(struct cclerical_decl *d)
{
	free(d->id);
	switch (d->type) {
	case CCLERICAL_DECL_VAR:
		break;
	case CCLERICAL_DECL_FUN:
		cclerical_vector_fini(&d->fun.arguments);
		cclerical_prog_destroy(d->fun.body);
		break;
	}
	free(d);
}

void cclerical_parser_fini(struct cclerical_parser *p)
{
	while (p->scope.parent) {
		struct cclerical_scope sc = cclerical_parser_close_scope(p);
		cclerical_scope_fini(&sc);
	}
	cclerical_scope_fini(&p->scope.scope);
	for (size_t i=0; i<p->decls.valid; i++) {
		struct cclerical_decl *d = p->decls.data[i];
		cclerical_decl_destroy(d);
	}
	cclerical_vector_fini(&p->decls);
	if (p->prog)
		cclerical_prog_destroy(p->prog);
}
