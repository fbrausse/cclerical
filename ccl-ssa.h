
#ifndef CCL_SSA_H
#define CCL_SSA_H

#include "cclerical.h"

typedef struct cclerical_vector ccl_vec_t;

typedef union { cclerical_id_t id; } ccl_decl_id_t; /* index into ccl_tu::decl_storage */
typedef union { cclerical_id_t id; } ccl_bb_id_t;   /* index into ccl_tu::bb_storage */
typedef union { cclerical_id_t id; } ccl_insn_id_t; /* index into ccl_tu::insn_storage */
typedef union { cclerical_id_t id; } ccl_fun_id_t;  /* index into ccl_tu::insn_storage */

enum ccl_insn_asgn_type {
	CCL_INSN_ASGN_ALIAS, /* incorporates VAR, CNST and FUN_CALL; the latter are pure! */
	CCL_INSN_ASGN_OP,
	CCL_INSN_ASGN_LIM,
};

struct ccl_insn_asgn {
	enum ccl_insn_asgn_type type;
	ccl_decl_id_t lhs; /* can never ref a function (i.e.
	                    *  !ccl_decl::is_constant
	                    *  && (ccl_decl::origin_is_artificial ||
	                    *      cclerical_decl::type != CCLERICAL_DECL_FUN))) */
	union {
		struct {
			ccl_decl_id_t decl;
			ccl_vec_t args; /* of (void *)(uintptr_t)ccl_decl_id_t */
		} alias;
		struct {
			enum cclerical_op op;
			ccl_decl_id_t args[CCLERICAL_OP_MAX_ARITY];
		} op;
		struct {
			/* TODO: lim needs knowledge about continuous parameters
			 *       used in body */
			ccl_decl_id_t seq_idx;
			ccl_insn_id_t body;
		} lim;
	};
	ccl_insn_id_t next;
};

enum ccl_insn_type {
	CCL_INSN_ASGN,
	CCL_INSN_CASE,
	CCL_INSN_IF,
	CCL_INSN_WHILE,
	CCL_INSN_RETURN,
};

struct ccl_insn {
	enum ccl_insn_type type;
	struct cclerical_source_loc source_loc;
	union {
		struct ccl_insn_asgn asgn;
		struct {
			ccl_vec_t conds;  /* of (void *)(uintptr_t)ccl_decl_id_t */
			ccl_vec_t bodies; /* of (void *)(uintptr_t)ccl_insn_id_t */
			/* if_false doesn't exist since no case being true
			 * during runtime is undefined behaviour */
		} cases;
		struct {
			ccl_decl_id_t cond;
			ccl_insn_id_t if_true;
			ccl_insn_id_t if_false;
		} branch;
		struct {
			ccl_decl_id_t cond;
			ccl_insn_id_t body; /* if_true */
			ccl_insn_id_t next; /* if_false */
		} loop;
		ccl_decl_id_t retval;
	};
};

void ccl_insn_destroy(struct ccl_insn *in);

struct ccl_basic_block {
	ccl_insn_id_t begin, end;
};

/*  type | cnst | art
 * ------+------+-----
 *   U   |  y   | y/n
 *   K   | y/n  |  y
 * B/Z/R | y/n  | y/n
 */

enum ccl_type {
	CCL_TYPE_UNIT, /* always constant */
	CCL_TYPE_BOOL,
	CCL_TYPE_INT,
	CCL_TYPE_REAL,
	CCL_TYPE_KLEENEAN, /* always artificial */
};

struct ccl_decl {
	enum ccl_type value_type;
	unsigned is_constant          : 1;
	unsigned origin_is_artificial : 1;
	union {
		const struct cclerical_constant *cnst;
		const struct cclerical_decl *org; /* can be both, FUN or VAR */
		ccl_insn_id_t art;
	} origin;
	ccl_fun_id_t fun_id; /* valid when valid(org)
	                                && org->type == CCLERICAL_DECL_FUN
	                                && !cclerical_decl_fun_is_external(org) */
	union {
		enum ccl_unit {
			CCL_UNIT_STAR,
		} unit;
		enum ccl_kleenean {
			CCL_KLEENEAN_FALSE,
			CCL_KLEENEAN_TRUE,
			CCL_KLEENEAN_BOT,
		} kleenean;
	} art_cnst_value;
};

struct ccl_fun {
	ccl_decl_id_t retval;
	ccl_insn_id_t body;
};

struct ccl_tu {
	ccl_vec_t decl_storage; /* of struct ccl_decl * */
	ccl_vec_t insn_storage; /* of struct ccl_insn * */

	ccl_vec_t fun_storage;  /* of struct ccl_fun * */

	ccl_vec_t bb_storage;   /* of struct ccl_basic_block * */

	ccl_vec_t compute;      /* of (void *)(uintptr_t)ccl_fun_id_t */
};

#define CCL_TU_INIT { \
	CCLERICAL_VECTOR_INIT, \
	CCLERICAL_VECTOR_INIT, \
	CCLERICAL_VECTOR_INIT, \
	CCLERICAL_VECTOR_INIT, \
	CCLERICAL_VECTOR_INIT, \
}

void ccl_tu_init(struct ccl_tu *tu, const ccl_vec_t *decls);
void ccl_tu_fini(const struct ccl_tu *tu);

ccl_fun_id_t ccl_cfg_add(struct ccl_tu *tu, const struct cclerical_prog *p, struct cclerical_source_loc source_loc);

#endif
