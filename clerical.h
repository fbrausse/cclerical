
#ifndef CLERICAL_H
#define CLERICAL_H

#include <stdint.h>
#include <stdlib.h>	/* malloc(), free(), abort() */
#include <string.h>
#include <errno.h>

#ifdef __cplusplus
extern "C" {
#endif

struct clerical_vector {
	void **data;
	size_t valid, size;
};

#define CLERICAL_VECTOR_INIT	{ NULL, 0, 0, }

void clerical_vector_add(struct clerical_vector *v, void *it);
void clerical_vector_fini(const struct clerical_vector *v);

enum clerical_type {
	CLERICAL_TYPE_UNIT,
	CLERICAL_TYPE_BOOL,
	CLERICAL_TYPE_INT,
	CLERICAL_TYPE_REAL,
};

struct clerical_constant {
	enum clerical_type lower_type;
	char *str;
	unsigned base;
};

struct clerical_var {
	char *id;
	enum clerical_type type;
};

struct clerical_var * clerical_var_create(char *id, enum clerical_type type);
void                  clerical_var_destroy(struct clerical_var *v);

typedef size_t clerical_var_t;

struct clerical_scope {
	struct clerical_vector var_idcs; /* of (void *)(uintptr_t)clerical_var_t */
};

void clerical_scope_fini(const struct clerical_scope *s);

struct clerical_prog;

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
			struct clerical_prog *seq;
			struct clerical_scope local;
		} lim;
		struct clerical_stmt_decl_asgn {
			clerical_var_t var;
			struct clerical_expr *expr;
			struct clerical_prog *prog;
		} decl_asgn;
	};
};

struct clerical_expr * clerical_expr_create(enum clerical_expr_type type);
struct clerical_expr * clerical_expr_create_op(enum clerical_op op,
                                               struct clerical_expr *a,
                                               struct clerical_expr *b);
void                   clerical_expr_destroy(struct clerical_expr *e);

enum clerical_stmt_type {
	CLERICAL_STMT_SKIP,
	CLERICAL_STMT_WHILE,
	CLERICAL_STMT_IF,
	CLERICAL_STMT_ASGN,
	CLERICAL_STMT_EXPR,
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
		struct clerical_expr *expr;
	};
};

struct clerical_stmt * clerical_stmt_create(enum clerical_stmt_type type);
void                   clerical_stmt_destroy(struct clerical_stmt *s);

struct clerical_prog {
	struct clerical_vector stmts; /* of struct clerical_stmt * */
};

struct clerical_prog * clerical_prog_create(void);
void                   clerical_prog_destroy(struct clerical_prog *p);

struct clerical_case {
	struct clerical_expr *cond;
	struct clerical_prog *prog;
};

void clerical_cases_fini(const struct clerical_vector *c);

/* -------------------------------------------------------------------------- */

struct clerical_parser_scope {
	struct clerical_parser_scope *parent;
	struct clerical_scope scope;
};

struct clerical_parser {
	struct clerical_parser_scope scope;
	struct clerical_prog *prog;
	struct clerical_vector vars; /* of struct clerical_var * */
};

void clerical_parser_init(struct clerical_parser *p);

int  clerical_parser_var_lookup(struct clerical_parser *p, const char *id,
                                clerical_var_t *v, int rw);

int  clerical_parser_new_var(struct clerical_parser *p, char *id,
                             enum clerical_type type, clerical_var_t *v);

void clerical_parser_open_scope(struct clerical_parser *p);

struct clerical_scope clerical_parser_close_scope(struct clerical_parser *p);

void clerical_parser_fini(struct clerical_parser *p);

#ifdef __cplusplus
}
#endif

#endif
