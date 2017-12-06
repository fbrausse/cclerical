
#ifndef CLERICAL_H
#define CLERICAL_H

#include <stdint.h>
#include <stdlib.h>	/* malloc(), free(), abort() */
#include <string.h>
#include <errno.h>

#ifdef __cplusplus
extern "C" {
#endif

#define MAX(a,b)	((a) < (b) ? (b) : (a))

static inline void * memdup(const void *src, size_t n)
{
	return memcpy(malloc(n), src, n);
}

struct clerical_vector {
	void **data;
	size_t valid, size;
};

#define CLERICAL_VECTOR_INIT	{ NULL, 0, 0, }

static inline void clerical_vector_ensure_size(struct clerical_vector *v, size_t n)
{
	if (v->size < n &&
	    !(v->data = realloc(v->data, sizeof(*v->data)*(v->size = MAX(n, v->valid*2)))))
		abort();
}

static inline void clerical_vector_add(struct clerical_vector *v, void *it)
{
	clerical_vector_ensure_size(v, v->valid+1);
	v->data[v->valid++] = it;
}

static inline void clerical_vector_fini(const struct clerical_vector *v)
{
	free(v->data);
}

enum clerical_type {
	CLERICAL_TYPE_UNDEF = -1,
	CLERICAL_TYPE_BOOL,
	CLERICAL_TYPE_INT,
	CLERICAL_TYPE_REAL,
};

struct clerical_constant {
	enum clerical_type lower_type;
	char *str;
	unsigned base;
};

enum clerical_op {
	CLERICAL_OP_PLUS = '+',
	CLERICAL_OP_MINUS = '-',
	CLERICAL_OP_MUL = '*',
	CLERICAL_OP_DIV = '/',
	CLERICAL_OP_EXP = '^',
	CLERICAL_OP_LT = '<',
	CLERICAL_OP_GT = '>',
	CLERICAL_OP_UMINUS = 128,
	CLERICAL_OP_NE,
};

enum clerical_expr_type {
	CLERICAL_EXPR_DECL_ASGN,
	CLERICAL_EXPR_CNST,
	CLERICAL_EXPR_VAR,
	CLERICAL_EXPR_CASE,
	CLERICAL_EXPR_LIM,
	CLERICAL_EXPR_OP,
};

struct clerical_var {
	char *id;
	enum clerical_type type;
};

static inline struct clerical_var * clerical_var_create(char *id, enum clerical_type type)
{
	struct clerical_var v = { id, type };
	return memdup(&v, sizeof(v));
}

static inline void clerical_var_destroy(struct clerical_var *v)
{
	free(v->id);
	free(v);
}

typedef size_t clerical_var_t;

struct clerical_prog;

struct clerical_scope {
	struct clerical_vector var_idcs; /* of (void *)(uintptr_t)clerical_var_t */
};

static inline void clerical_scope_fini(const struct clerical_scope *s)
{
	clerical_vector_fini(&s->var_idcs);
}

struct clerical_expr {
	enum clerical_expr_type type;
	union {
		struct clerical_constant cnst;
		clerical_var_t var;
		struct {
			struct clerical_expr *arg1, *arg2;
			enum clerical_op op;
		} op;
		struct clerical_vector cases; /* of clerical_case */
		struct {
			clerical_var_t seq_idx;
			struct clerical_expr *seq;
			struct clerical_scope local;
		} lim;
		struct clerical_stmt_decl_asgn {
			clerical_var_t var;
			struct clerical_expr *expr;
			struct clerical_prog *prog;
		} decl_asgn;
	};
};

static inline
struct clerical_expr * clerical_expr_create(enum clerical_expr_type type)
{
	struct clerical_expr *e = malloc(sizeof(struct clerical_expr));
	e->type = type;
	return e;
}

static inline
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

static void clerical_cases_fini(const struct clerical_vector *c);
static void clerical_prog_destroy(struct clerical_prog *p);

static inline void clerical_expr_destroy(struct clerical_expr *e)
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

enum clerical_stmt_type {
	CLERICAL_STMT_SKIP,
	CLERICAL_STMT_WHILE,
	CLERICAL_STMT_IF,
	CLERICAL_STMT_ASGN,
};

struct clerical_case {
	struct clerical_expr *cond;
	struct clerical_prog *prog;
};

struct clerical_stmt {
	enum clerical_stmt_type type;
	union {
		struct {} skip; /* skip */
		struct {
			struct clerical_expr *cond;
			struct clerical_prog *body;
		} loop;
		struct {
			struct clerical_expr *cond;
			struct clerical_prog *if_true;
			struct clerical_prog *if_false;
		} branch;
		struct {
			clerical_var_t var;
			struct clerical_expr *expr;
		} asgn;
	};
};

static inline
struct clerical_stmt * clerical_stmt_create(enum clerical_stmt_type type)
{
	struct clerical_stmt *e = malloc(sizeof(struct clerical_stmt));
	e->type = type;
	return e;
}

static void clerical_stmt_destroy(struct clerical_stmt *s)
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
	}
	free(s);
}

struct clerical_prog {
	struct clerical_vector stmts; /* of struct clerical_stmt * */
};

static inline struct clerical_prog * clerical_prog_create(void)
{
	struct clerical_prog *p = calloc(1, sizeof(struct clerical_prog));
	return p;
}

static void clerical_prog_destroy(struct clerical_prog *p)
{
	for (size_t i=0; i<p->stmts.valid; i++) {
		struct clerical_stmt *s = p->stmts.data[i];
		clerical_stmt_destroy(s);
	}
	clerical_vector_fini(&p->stmts);
}

static inline void clerical_cases_fini(const struct clerical_vector *c)
{
	for (size_t i=0; i<c->valid; i+=2) {
		clerical_expr_destroy(c->data[i]);
		clerical_prog_destroy(c->data[i+1]);
	}
	clerical_vector_fini(c);
}

/* -------------------------------------------------------------------------- */

struct clerical_parser_scope {
	struct clerical_parser_scope *parent;
	struct clerical_scope scope;
};

struct clerical_parser {
	struct clerical_parser_scope scope;
	struct clerical_vector vars; /* of struct clerical_var *; TODO: inefficient... */
};

static inline void clerical_parser_init(struct clerical_parser *p)
{
	memset(p, 0, sizeof(*p));
}

static inline
const int lookup_var(const struct clerical_parser *p,
                     const struct clerical_parser_scope *s,
                     const char *id, clerical_var_t *ridx)
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
	return lookup_var(p, s->parent, id, ridx);
}

static inline int clerical_parser_var_lookup(struct clerical_parser *p, const char *id, clerical_var_t *v)
{
	return lookup_var(p, &p->scope, id, v);
}

static inline int clerical_parser_new_var(struct clerical_parser *p, char *id, enum clerical_type type, clerical_var_t *v)
{
	clerical_var_t exists;
	if (clerical_parser_var_lookup(p, id, &exists))
		return EEXIST;
	size_t idx = p->vars.valid;
	clerical_vector_add(&p->vars, clerical_var_create(id, type));
	*v = idx;
	return 0;
}

static inline void clerical_parser_open_scope(struct clerical_parser *p)
{
	struct clerical_parser_scope *s = malloc(sizeof(struct clerical_parser_scope));
	*s = p->scope;
	p->scope.parent = s;
	memset(&p->scope, 0, sizeof(p->scope));
}

static inline struct clerical_scope clerical_parser_close_scope(struct clerical_parser *p)
{
	struct clerical_scope scope = p->scope.scope;
	struct clerical_parser_scope *s = p->scope.parent;
	p->scope = *s;
	free(s);
	return scope;
}

static inline void clerical_parser_fini(struct clerical_parser *p)
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
}

/*
int clerical_parse_expr(struct clerical_parser *p, struct clerical_expr **ret);
int clerical_parse_stmt(struct clerical_parser *p, struct clerical_stmt **ret);
int clerical_parse_fun(struct clerical_parser *p, struct clerical_fun **ret);
int clerical_parse_prog(struct clerical_parser *p, struct clerical_prog **ret);
*/

#ifdef __cplusplus
}
#endif

#endif
