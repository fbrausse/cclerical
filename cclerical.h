
#ifndef CCLERICAL_H
#define CCLERICAL_H

#include <stdint.h>
#include <stdlib.h>	/* malloc(), free(), abort() */
#include <string.h>
#include <errno.h>

#ifdef __cplusplus
extern "C" {
#endif

struct cclerical_vector {
	void **data;
	size_t valid, size;
};

#define CCLERICAL_VECTOR_INIT	{ NULL, 0, 0, }

static inline void cclerical_vector_init(struct cclerical_vector *v)
{
	memset(v, 0, sizeof(*v));
}

void cclerical_vector_add(struct cclerical_vector *v, void *it);
void cclerical_vector_fini(const struct cclerical_vector *v);

enum cclerical_type {
	CCLERICAL_TYPE_UNIT,
	CCLERICAL_TYPE_BOOL,
	CCLERICAL_TYPE_INT,
	CCLERICAL_TYPE_REAL,
};

typedef unsigned cclerical_type_set_t;

struct cclerical_constant {
	enum cclerical_type lower_type;
	char *str;
	unsigned base;
};

struct cclerical_var {
	char *id;
	enum cclerical_type type;
};

struct cclerical_var * cclerical_var_create(char *id, enum cclerical_type type);
void                  cclerical_var_destroy(struct cclerical_var *v);

typedef size_t cclerical_var_t;

struct cclerical_scope {
	struct cclerical_vector var_idcs; /* of (void *)(uintptr_t)cclerical_var_t */
};

void cclerical_scope_fini(const struct cclerical_scope *s);

struct cclerical_prog;

enum cclerical_op {
	CCLERICAL_OP_PLUS = '+',
	CCLERICAL_OP_MINUS = '-',
	CCLERICAL_OP_MUL = '*',
	CCLERICAL_OP_DIV = '/',
	CCLERICAL_OP_EXP = '^',
	CCLERICAL_OP_LT = '<',
	CCLERICAL_OP_GT = '>',
	CCLERICAL_OP_UMINUS = 128,
	CCLERICAL_OP_NE,
};

enum cclerical_expr_type {
	CCLERICAL_EXPR_DECL_ASGN,
	CCLERICAL_EXPR_CNST,
	CCLERICAL_EXPR_VAR,
	CCLERICAL_EXPR_CASE,
	CCLERICAL_EXPR_LIM,
	CCLERICAL_EXPR_OP,
};

struct cclerical_expr {
	enum cclerical_expr_type type;
	enum cclerical_type result_type;
	union {
		struct cclerical_constant cnst;
		cclerical_var_t var;
		struct {
			struct cclerical_expr *arg1, *arg2;
			enum cclerical_op op;
		} op;
		struct cclerical_vector cases; /* of cclerical_case */
		struct {
			cclerical_var_t seq_idx;
			struct cclerical_prog *seq;
			struct cclerical_scope local;
		} lim;
		struct cclerical_stmt_decl_asgn {
			cclerical_var_t var;
			struct cclerical_expr *expr;
			struct cclerical_prog *prog;
		} decl_asgn;
	};
};

struct cclerical_expr * cclerical_expr_create(enum cclerical_expr_type type);
struct cclerical_expr * cclerical_expr_create_op(enum cclerical_op op,
                                               struct cclerical_expr *a,
                                               struct cclerical_expr *b);
void                   cclerical_expr_destroy(struct cclerical_expr *e);

enum cclerical_stmt_type {
	CCLERICAL_STMT_SKIP,
	CCLERICAL_STMT_WHILE,
	CCLERICAL_STMT_IF,
	CCLERICAL_STMT_ASGN,
	CCLERICAL_STMT_EXPR,
};

struct cclerical_stmt {
	enum cclerical_stmt_type type;
	union {
		struct {} skip; /* skip */
		struct {
			struct cclerical_expr *cond;
			struct cclerical_prog *body;
		} loop;
		struct {
			struct cclerical_expr *cond;
			struct cclerical_prog *if_true;
			struct cclerical_prog *if_false;
		} branch;
		struct {
			cclerical_var_t var;
			struct cclerical_expr *expr;
		} asgn;
		struct cclerical_expr *expr;
	};
};

struct cclerical_stmt * cclerical_stmt_create(enum cclerical_stmt_type type);
void                   cclerical_stmt_destroy(struct cclerical_stmt *s);

struct cclerical_prog {
	struct cclerical_vector stmts; /* of struct cclerical_stmt * */
};

struct cclerical_prog * cclerical_prog_create(void);
void                   cclerical_prog_destroy(struct cclerical_prog *p);
enum cclerical_type cclerical_prog_type(const struct cclerical_prog *p);

struct cclerical_case {
	struct cclerical_expr *cond;
	struct cclerical_prog *prog;
};

void cclerical_cases_fini(const struct cclerical_vector *c);

struct cclerical_fun {
	char *id;
	struct cclerical_vector arguments; /* of type (void *)(uintptr_t)cclerical_var_t */
	struct cclerical_prog *body;
};

struct cclerical_fun * cclerical_fun_create(char *id,
                                            struct cclerical_vector *arguments,
                                            struct cclerical_prog *body);

/* -------------------------------------------------------------------------- */

struct cclerical_parser_scope {
	struct cclerical_parser_scope *parent;
	struct cclerical_scope scope;
};

struct cclerical_parser {
	struct cclerical_parser_scope scope;
	struct cclerical_prog *prog;
	struct cclerical_vector vars; /* of struct cclerical_var * */
	struct cclerical_vector funs; /* of struct cclerical_fun * */
};

void cclerical_parser_init(struct cclerical_parser *p);

int  cclerical_parser_var_lookup(struct cclerical_parser *p, const char *id,
                                cclerical_var_t *v, int rw);

int  cclerical_parser_new_var(struct cclerical_parser *p, char *id,
                             enum cclerical_type type, cclerical_var_t *v);

void cclerical_parser_open_scope(struct cclerical_parser *p);

struct cclerical_scope cclerical_parser_close_scope(struct cclerical_parser *p);

void cclerical_parser_fini(struct cclerical_parser *p);

#ifdef __cplusplus
}
#endif

#endif
