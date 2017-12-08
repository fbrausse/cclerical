
#include "clerical.h"

#define MAX(a,b)	((a) < (b) ? (b) : (a))

static inline void * memdup(const void *src, size_t n)
{
	return memcpy(malloc(n), src, n);
}

static inline void clerical_vector_ensure_size(struct clerical_vector *v, size_t n)
{
	if (v->size < n &&
	    !(v->data = realloc(v->data, sizeof(*v->data)*(v->size = MAX(n, v->valid*2)))))
		abort();
}

void clerical_vector_add(struct clerical_vector *v, void *it)
{
	clerical_vector_ensure_size(v, v->valid+1);
	v->data[v->valid++] = it;
}

void clerical_vector_fini(const struct clerical_vector *v)
{
	free(v->data);
}

struct clerical_expr * clerical_expr_create(enum clerical_expr_type type)
{
	struct clerical_expr *e = malloc(sizeof(struct clerical_expr));
	e->type = type;
	return e;
}

struct clerical_expr * clerical_expr_create_op(enum clerical_op op,
                                               struct clerical_expr *a,
                                               struct clerical_expr *b)
{
	struct clerical_expr *e = clerical_expr_create(CLERICAL_EXPR_OP);
	e->op.arg1 = a;
	e->op.arg2 = b;
	e->op.op = op;
	return e;
}

void clerical_expr_destroy(struct clerical_expr *e)
{
	switch (e->type) {
	case CLERICAL_EXPR_VAR: break;
	case CLERICAL_EXPR_CNST: free(e->cnst.str); break;
	case CLERICAL_EXPR_CASE: clerical_cases_fini(&e->cases); break;
	case CLERICAL_EXPR_DECL_ASGN:
		clerical_expr_destroy(e->decl_asgn.expr);
		clerical_prog_destroy(e->decl_asgn.prog);
		break;
	case CLERICAL_EXPR_LIM:
		clerical_expr_destroy(e->lim.seq);
		clerical_scope_fini(&e->lim.local);
		break;
	case CLERICAL_EXPR_OP:
		clerical_expr_destroy(e->op.arg1);
		clerical_expr_destroy(e->op.arg2);
		break;
	}
	free(e);
}

struct clerical_stmt * clerical_stmt_create(enum clerical_stmt_type type)
{
	struct clerical_stmt *e = malloc(sizeof(struct clerical_stmt));
	e->type = type;
	return e;
}

void clerical_stmt_destroy(struct clerical_stmt *s)
{
	switch (s->type) {
	case CLERICAL_STMT_SKIP: break;
	case CLERICAL_STMT_ASGN: clerical_expr_destroy(s->asgn.expr); break;
	case CLERICAL_STMT_IF:
		clerical_expr_destroy(s->branch.cond);
		clerical_prog_destroy(s->branch.if_true);
		clerical_prog_destroy(s->branch.if_false);
		break;
	case CLERICAL_STMT_WHILE:
		clerical_expr_destroy(s->loop.cond);
		clerical_prog_destroy(s->loop.body);
		break;
	case CLERICAL_STMT_EXPR: clerical_expr_destroy(s->expr); break;
	}
	free(s);
}

struct clerical_prog * clerical_prog_create(void)
{
	struct clerical_prog *p = calloc(1, sizeof(struct clerical_prog));
	return p;
}

void clerical_prog_destroy(struct clerical_prog *p)
{
	for (size_t i=0; i<p->stmts.valid; i++) {
		struct clerical_stmt *s = p->stmts.data[i];
		clerical_stmt_destroy(s);
	}
	clerical_vector_fini(&p->stmts);
	free(p);
}

void clerical_cases_fini(const struct clerical_vector *c)
{
	for (size_t i=0; i<c->valid; i+=2) {
		clerical_expr_destroy(c->data[i]);
		clerical_prog_destroy(c->data[i+1]);
	}
	clerical_vector_fini(c);
}

struct clerical_var * clerical_var_create(char *id, enum clerical_type type)
{
	struct clerical_var v = { id, type };
	return memdup(&v, sizeof(v));
}

void clerical_var_destroy(struct clerical_var *v)
{
	free(v->id);
	free(v);
}

void clerical_scope_fini(const struct clerical_scope *s)
{
	clerical_vector_fini(&s->var_idcs);
}

/* -------------------------------------------------------------------------- */

void clerical_parser_init(struct clerical_parser *p)
{
	memset(p, 0, sizeof(*p));
}

static
const int clerical_parser_var_lookup0(const struct clerical_parser *p,
                                      const struct clerical_parser_scope *s,
                                      const char *id, clerical_var_t *ridx,
                                      int rw)
{
	if (!s)
		return 0;
	const struct clerical_vector *vi = &s->scope.var_idcs;
	for (size_t i=0; i<vi->valid; i++) {
		size_t idx = (uintptr_t)vi->data[i];
		const struct clerical_var *v = p->vars.data[idx];
		if (!strcmp(v->id, id)) {
			*ridx = idx;
			return 1;
		}
	}
	return rw ? 0 : clerical_parser_var_lookup0(p, s->parent, id, ridx, rw);
}

int clerical_parser_var_lookup(struct clerical_parser *p, const char *id,
                               clerical_var_t *v, int rw)
{
	return clerical_parser_var_lookup0(p, &p->scope, id, v, rw);
}

int clerical_parser_new_var(struct clerical_parser *p, char *id,
                            enum clerical_type type, clerical_var_t *v)
{
	clerical_var_t exists;
	if (clerical_parser_var_lookup(p, id, &exists, 0))
		return EEXIST;
	size_t idx = p->vars.valid;
	clerical_vector_add(&p->vars, clerical_var_create(id, type));
	clerical_vector_add(&p->scope.scope.var_idcs, (void *)(uintptr_t)idx);
	*v = idx;
	return 0;
}

void clerical_parser_open_scope(struct clerical_parser *p)
{
	struct clerical_parser_scope *s;
	s = malloc(sizeof(struct clerical_parser_scope));
	*s = p->scope;
	p->scope.parent = s;
	memset(&p->scope, 0, sizeof(p->scope));
}

struct clerical_scope clerical_parser_close_scope(struct clerical_parser *p)
{
	struct clerical_scope scope = p->scope.scope;
	struct clerical_parser_scope *s = p->scope.parent;
	p->scope = *s;
	free(s);
	return scope;
}

void clerical_parser_fini(struct clerical_parser *p)
{
	while (p->scope.parent) {
		clerical_scope_fini(&p->scope.scope);
		p->scope.scope = clerical_parser_close_scope(p);
	}
	clerical_scope_fini(&p->scope.scope);
	for (size_t i=0; i<p->vars.valid; i++) {
		struct clerical_var *v = p->vars.data[i];
		clerical_var_destroy(v);
	}
	clerical_vector_fini(&p->vars);
	if (p->prog)
		clerical_prog_destroy(p->prog);
}
