/* SPDX short identifier: BSD-3-Clause */
#ifndef CCLERICAL_H
#define CCLERICAL_H

#include <stdint.h>
#include <stdlib.h>	/* malloc(), free(), abort() */
#include <string.h>
#include <errno.h>

#ifdef __cplusplus
extern "C" {
#endif

struct cclerical_source_loc {
	int first_line, first_column;
	int last_line, last_column;
};

static inline void * memdup(const void *src, size_t n)
{
	return memcpy(malloc(n), src, n);
}

struct cclerical_vector {
	void **data;
	size_t valid, size;
};

#define CCLERICAL_VECTOR_INIT	{ NULL, 0, 0, }

static inline void cclerical_vector_init(struct cclerical_vector *v)
{
	memset(v, 0, sizeof(*v));
}

static inline void * cclerical_vector_last(const struct cclerical_vector *v)
{
	return v->data[v->valid-1];
}

void cclerical_vector_add(struct cclerical_vector *v, void *it);
void cclerical_vector_fini(const struct cclerical_vector *v);

enum cclerical_type {
	CCLERICAL_TYPE_UNIT,
	CCLERICAL_TYPE_BOOL,
	CCLERICAL_TYPE_INT,
	CCLERICAL_TYPE_REAL,
};

extern const char *const CCLERICAL_TYPE_STR[];

typedef unsigned cclerical_type_set_t;

struct cclerical_constant {
	enum cclerical_type type;
	union {
		int boolean;
		struct {
			char *str;
			unsigned base;
		} numeric;
	};
};

void cclerical_constant_fini(struct cclerical_constant *c);

typedef size_t cclerical_id_t;

struct cclerical_scope {
	struct cclerical_vector var_idcs; /* of (void *)(uintptr_t)cclerical_id_t */
};

void cclerical_scope_fini(const struct cclerical_scope *s);

struct cclerical_prog;

enum cclerical_op {
	CCLERICAL_OP_NEG,
	CCLERICAL_OP_NOT,
	CCLERICAL_OP_AND,
	CCLERICAL_OP_OR,
	CCLERICAL_OP_ADD,
	CCLERICAL_OP_SUB,
	CCLERICAL_OP_MUL,
	CCLERICAL_OP_DIV,
	CCLERICAL_OP_EXP,
	CCLERICAL_OP_LT,
	CCLERICAL_OP_GT,
	CCLERICAL_OP_NE,
};

int cclerical_op_is_unary(enum cclerical_op);

enum cclerical_expr_type {
	CCLERICAL_EXPR_DECL_ASGN,
	CCLERICAL_EXPR_CNST,
	CCLERICAL_EXPR_VAR,
	CCLERICAL_EXPR_FUN_CALL,
	CCLERICAL_EXPR_CASE,
	CCLERICAL_EXPR_IF, /* syntactic sugar for CCLERICAL_EXPR_CASE */
	CCLERICAL_EXPR_LIM,
	CCLERICAL_EXPR_OP,
	CCLERICAL_EXPR_SKIP,
	CCLERICAL_EXPR_WHILE,
	CCLERICAL_EXPR_ASGN,
	CCLERICAL_EXPR_SEQ,
};

struct cclerical_expr {
	enum cclerical_expr_type type;
	enum cclerical_type result_type;
	struct cclerical_source_loc source_loc;
	size_t min_scope_asgn;
	union {
		struct cclerical_constant cnst;
		cclerical_id_t var;
		struct {
			cclerical_id_t fun;
			struct cclerical_vector params; /* of type struct cclerical_expr * */
		} fun_call;
		struct {
			struct cclerical_expr *arg1, *arg2;
			enum cclerical_op op;
		} op;
		struct cclerical_vector cases; /* of cclerical_case */
		struct {
			struct cclerical_expr *cond;
			struct cclerical_expr *if_true;
			struct cclerical_expr *if_false; /* may be NULL */
		} branch;
		struct {
			cclerical_id_t seq_idx;
			struct cclerical_expr *seq;
			struct cclerical_scope local;
		} lim;
		struct cclerical_stmt_decl_asgn {
			/* of type { (void *)(uintptr_t)cclerical_id_t;
			 *           struct cclerical_expr * } */
			struct cclerical_vector inits;
			struct cclerical_expr *prog;
		} decl_asgn;
		struct {} skip; /* skip */
		struct {
			struct cclerical_expr *cond;
			struct cclerical_expr *body;
		} loop;
		struct {
			cclerical_id_t var;
			struct cclerical_expr *expr;
		} asgn;
		struct cclerical_prog *seq;
	};
};

struct cclerical_expr * cclerical_expr_create(enum cclerical_expr_type type);
struct cclerical_expr * cclerical_expr_create_op(enum cclerical_op op,
                                                 struct cclerical_expr *a,
                                                 struct cclerical_expr *b);
void                    cclerical_expr_destroy(struct cclerical_expr *e);

static inline int cclerical_expr_is_pure(const struct cclerical_expr *e,
                                         size_t rel_to_scope_idx)
{
	return e->min_scope_asgn >= rel_to_scope_idx;
}

enum cclerical_stmt_type {
	CCLERICAL_STMT_EXPR,
};

struct cclerical_stmt {
	enum cclerical_stmt_type type;
	union {
		struct cclerical_expr *expr;
	};
};

struct cclerical_stmt * cclerical_stmt_create(enum cclerical_stmt_type type);
void                    cclerical_stmt_destroy(struct cclerical_stmt *s);

struct cclerical_prog {
	struct cclerical_vector stmts; /* of struct cclerical_stmt *, size >= 1 */
};

struct cclerical_prog * cclerical_prog_create(void);
void                    cclerical_prog_destroy(struct cclerical_prog *p);
enum cclerical_type     cclerical_prog_type(const struct cclerical_prog *p);

struct cclerical_case {
	struct cclerical_expr *cond;
	struct cclerical_prog *prog;
};

void cclerical_cases_fini(const struct cclerical_vector *c);

/* -------------------------------------------------------------------------- */

struct cclerical_parser_scope {
	struct cclerical_scope scope;
	unsigned this_read_only : 1;
	unsigned prev_read_only : 1;
};

struct cclerical_decl {
	enum {
		CCLERICAL_DECL_VAR,
		CCLERICAL_DECL_FUN,
	} type;
	enum cclerical_type value_type;
	char *id;
	struct cclerical_source_loc source_loc;
	union {
		struct {} var;
		struct {
			/* if body: of type (void *)(uintptr_t)cclerical_id_t
			 * else   : of type (void *)(uintptr_t)cclerical_type */
			struct cclerical_vector arguments;
			/* may be NULL for declarations of "external" functions */
			struct cclerical_prog *body;
		} fun;
	};
};

#define CCLERICAL_DECL_INIT_VAR(value_type,id,loc) \
	{ CCLERICAL_DECL_VAR, value_type, id, loc, .var = {}, }

#define CCLERICAL_DECL_INIT_FUN(value_type,id,loc,args,body) \
	{ CCLERICAL_DECL_FUN, value_type, id, loc, .fun = { args, body }, }

void cclerical_decl_fini(const struct cclerical_decl *d);

struct cclerical_parser {
	struct cclerical_vector scopes; /* of struct cclerical_parser_scope */
	struct cclerical_prog *prog;
	struct cclerical_vector decls; /* of struct cclerical_decl * */
};

void cclerical_parser_init(struct cclerical_parser *p);

int  cclerical_parser_var_lookup(struct cclerical_parser *p, const char *id,
                                 cclerical_id_t *v, size_t *scope_idx, int rw);

int cclerical_parser_new_decl(struct cclerical_parser *p,
                              const struct cclerical_decl *decl,
                              cclerical_id_t *v);

void cclerical_parser_open_scope(struct cclerical_parser *p, int ro,
                                 int parents_ro);

struct cclerical_scope cclerical_parser_close_scope(struct cclerical_parser *p);

void cclerical_parser_fini(struct cclerical_parser *p);

#ifdef __cplusplus
}
#endif

#endif
