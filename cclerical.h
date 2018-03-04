/* SPDX short identifier: BSD-3-Clause */
#ifndef CCLERICAL_H
#define CCLERICAL_H

#include <stdint.h>
#include <stdio.h>	/* FILE */
#include <stdlib.h>	/* malloc(), free(), abort() */
#include <stddef.h>	/* max_align_t */
#include <string.h>
#include <errno.h>

#ifdef __cplusplus
extern "C" {
#endif

/* count trailing zeroes */

/* Returns the number of trailing 0-bits in x, starting at the least significant
 * bit position. If x is 0, the result is undefined. */
#ifdef __GNUC__
# define CCLERICAL_CTZ(x) \
	_Generic(+(x) \
	        ,unsigned int: __builtin_ctz(x) \
	        ,unsigned long: __builtin_ctzl(x) \
	        ,unsigned long long: __builtin_ctzll(x) \
	        )
#else
# define CCLERICAL_CTZ_FN(name,type)        \
	static inline unsigned name(type x) \
	{                                   \
		unsigned n = 0;             \
		for (; !(x & 1U); n++)      \
			x >>= 1;            \
		return n;                   \
	}
CCLERICAL_CTZ_FN(cclerical_ctz,unsigned)
CCLERICAL_CTZ_FN(cclerical_ctzl,unsigned long)
CCLERICAL_CTZ_FN(cclerical_ctzll,unsigned long long)
# undef CCLERICAL_CTX_FN
# define CCLERICAL_CTZ(x) \
	_Generic(+(x) \
	        ,unsigned int: cclerical_ctz(x) \
	        ,unsigned long: cclerical_ctzl(x) \
	        ,unsigned long long: cclerical_ctzll(x) \
	        )
#endif

/* utility functions */

static inline void * memdup(const void *src, size_t n)
{
	return memcpy(malloc(n), src, n);
}

/* typed vectors */

#define CCLERICAL_VECTOR_STRUCT(name,of_type) \
	struct name { \
		of_type *data; \
		size_t valid, size; \
	}

CCLERICAL_VECTOR_STRUCT(cclerical_vector_tmpl,void);

static inline void
cclerical_vector_tmpl_ensure_size(struct cclerical_vector_tmpl *v,
                                  size_t n, size_t elem_sz)
{
	if (v->size >= n)
		return;
	v->size = n > v->valid * 2 ? n : v->valid * 2;
	if (!(v->data = realloc(v->data, elem_sz * v->size)))
		abort();
}

#define cclerical_vector_init(v)	memset((v), 0, sizeof(*(v)))
#define cclerical_vector_fini(v)	free((v)->data)
#define cclerical_vector_ensure_size(v,n) \
	cclerical_vector_tmpl_ensure_size((struct cclerical_vector_tmpl *)(v), \
	                                  (n), sizeof(*(v)->data))
/* no generic version of _add(v,it) since it would either have to evaluate v or
 * it multiple times, take a ptr of it or require __typeof__() support */
#if 0
static inline void *
cclerical_vector_tmpl_prep_add(struct cclerical_vector_tmpl *v, size_t elem_sz)
{
	cclerical_vector_tmpl_ensure_size(v, v->valid+1, elem_sz);
	return (char *)v->data + elem_sz * v->valid++;
}

#define cclerical_vector_add(v,it) \
	(void)(*(__typeof__((v)->data))cclerical_vector_tmpl_prep_add( \
		(struct cclerical_vector_tmpl *)(v), \
		sizeof(*(v)->data) \
	      ) = (it))
#endif

#define CCLERICAL_VECTOR_INIT	{ NULL, 0, 0, }

#define CCLERICAL_VECTOR_DEF(name,of_type) \
	/* there is no realloc_aligned() */ \
	_Static_assert(_Alignof(of_type) <= _Alignof(max_align_t), \
	               "vectors with alignment > max_align_t unsupported"); \
	CCLERICAL_VECTOR_STRUCT(name,of_type); \
	static inline void name ## _add(struct name *v, of_type it) \
	{ \
		cclerical_vector_ensure_size(v, v->size+1); \
		v->data[v->valid++] = it; \
	} \
	static inline of_type name ## _last(const struct name *v) \
	{ \
		return v->data[v->valid-1]; \
	}

struct cclerical_input {
	const char *name;
	void *data;
	size_t size;
	void (*fini)(struct cclerical_input *);
};

struct cclerical_source_loc {
	int first_line, first_column;
	int last_line, last_column;
};

enum cclerical_highlight_mode {
	CCLERICAL_HIGHLIGHT_AUTO,
	CCLERICAL_HIGHLIGHT_ASCII,
	CCLERICAL_HIGHLIGHT_VT100,
};

void cclerical_highlight(FILE *out, enum cclerical_highlight_mode mode,
                         const struct cclerical_input *input,
                         const struct cclerical_source_loc *locp);

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

CCLERICAL_VECTOR_DEF(cclerical_vec_id_t,cclerical_id_t)

struct cclerical_scope {
	struct cclerical_vec_id_t var_idcs;
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
	CCLERICAL_OP_LE,
	CCLERICAL_OP_GT,
	CCLERICAL_OP_GE,
	CCLERICAL_OP_NE,
	CCLERICAL_OP_EQ,
};

#define CCLERICAL_OP_MAX_ARITY	2

int cclerical_op_is_unary(enum cclerical_op);
unsigned cclerical_op_arity(enum cclerical_op);

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

struct cclerical_expr;

CCLERICAL_VECTOR_DEF(cclerical_vec_expr_ptr,struct cclerical_expr *)

struct cclerical_decl_asgn {
	cclerical_id_t id;
	struct cclerical_expr *init;
};

CCLERICAL_VECTOR_DEF(cclerical_vec_decl_asgn,struct cclerical_decl_asgn)

struct cclerical_case {
	struct cclerical_expr *cond;
	struct cclerical_expr *body;
};

CCLERICAL_VECTOR_DEF(cclerical_vec_case,struct cclerical_case)

void cclerical_cases_fini(const struct cclerical_vec_case *c);

struct cclerical_prog;

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
			struct cclerical_vec_expr_ptr params;
		} fun_call;
		struct {
			struct cclerical_expr *args[CCLERICAL_OP_MAX_ARITY];
			enum cclerical_op op;
		} op;
		struct cclerical_vec_case cases;
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
			struct cclerical_vec_decl_asgn inits;
			struct cclerical_expr *body;
		} decl_asgn;
		/* struct {} skip; */
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

struct cclerical_prog {
	struct cclerical_vec_expr_ptr exprs; /* size >= 1 */
};

struct cclerical_prog * cclerical_prog_create(void);
void                    cclerical_prog_destroy(struct cclerical_prog *p);
enum cclerical_type     cclerical_prog_type(const struct cclerical_prog *p);

/* -------------------------------------------------------------------------- */

struct cclerical_parser_scope {
	struct cclerical_scope scope;
	unsigned this_read_only : 1;
	unsigned prev_read_only : 1;
};

CCLERICAL_VECTOR_DEF(cclerical_vec_type,enum cclerical_type)

struct cclerical_decl {
	enum {
		CCLERICAL_DECL_VAR,
		CCLERICAL_DECL_FUN,
	} type;
	enum cclerical_type value_type;
	char *id;
	struct cclerical_source_loc source_loc;
	union {
	//	struct {} var;
		struct {
			union {
				struct cclerical_vec_type types; /* if !body */
				struct cclerical_vec_id_t ids;   /* if  body */
			} arguments;
			/* may be NULL for declarations of "external" functions */
			struct cclerical_prog *body;
		} fun;
	};
};

#define CCLERICAL_DECL_INIT_VAR(value_type,id,loc) \
	{ CCLERICAL_DECL_VAR, value_type, id, loc, \
	  .fun = { { CCLERICAL_VECTOR_INIT }, NULL } /*.var = {}*/, }

#define CCLERICAL_DECL_INIT_FUN(value_type,id,loc,args,body) \
	{ CCLERICAL_DECL_FUN, value_type, id, loc, .fun = { { .ids = args }, body }, }

#define CCLERICAL_DECL_INIT_EXT_FUN(value_type,id,loc,args,body) \
	{ CCLERICAL_DECL_FUN, value_type, id, loc, .fun = { { .types = args }, body }, }

static inline int cclerical_decl_fun_is_external(const struct cclerical_decl *d)
{
	return !d->fun.body;
}

void cclerical_decl_fini(const struct cclerical_decl *d);

CCLERICAL_VECTOR_DEF(cclerical_vec_scope_ptr,struct cclerical_parser_scope *)
CCLERICAL_VECTOR_DEF(cclerical_vec_decl_ptr,struct cclerical_decl *)

struct cclerical_parser {
	struct cclerical_vec_scope_ptr scopes;
	struct cclerical_prog *prog;
	struct cclerical_vec_decl_ptr decls; /* of struct cclerical_decl * */
	const struct cclerical_input *input;
};

void cclerical_parser_init(struct cclerical_parser *p,
                           const struct cclerical_input *input);

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
