
#ifndef CCL_SSA_H
#define CCL_SSA_H

#include "cclerical.h"

typedef struct cclerical_vector ccl_vec_t;

typedef union { cclerical_id_t id; } ccl_decl_id_t; /* index into ccl_tu::decl_storage */
typedef union { cclerical_id_t id; } ccl_bb_id_t;   /* index into ccl_tu::bb_storage */
typedef union { cclerical_id_t id; } ccl_insn_id_t; /* index into ccl_tu::insn_storage */
typedef union { cclerical_id_t id; } ccl_fun_id_t;  /* index into ccl_tu::insn_storage */

enum ccl_insn_type {
	CCL_INSN_ASGN,
	CCL_INSN_CASE,
	CCL_INSN_IF,
	CCL_INSN_WHILE,
};

enum ccl_insn_asgn_type {
	CCL_INSN_ASGN_ALIAS,
	CCL_INSN_ASGN_OP,
	CCL_INSN_ASGN_FUN_CALL,
	CCL_INSN_ASGN_LIM,
};

struct ccl_insn {
	enum ccl_insn_type type;
	union {
		struct ccl_insn_asgn {
			enum ccl_insn_asgn_type type;
			ccl_decl_id_t lhs;
			union {
				ccl_decl_id_t alias;
				struct {
					enum cclerical_op op;
					ccl_decl_id_t args[CCLERICAL_OP_MAX_ARITY];
				} op;
				struct {
					ccl_fun_id_t fun;
					ccl_vec_t args; /* of (void *)(uintptr_t)(ccl_decl_id_t) */
				} fun_call;
				struct {
					ccl_decl_id_t seq_idx;
					ccl_bb_id_t body;
				} lim;
			};
		} asgn;
		struct {
			ccl_vec_t conds; /* of (void *)(uintptr_t)(ccl_decl_id_t) */
			ccl_vec_t bodies; /* of (void *)(uintptr_t)(ccl_bb_id_t) */
		} cases;
		struct {
			ccl_decl_id_t cond;
			ccl_bb_id_t if_true;
			ccl_bb_id_t if_false;
		} branch;
		struct {
			ccl_decl_id_t cond;
			ccl_bb_id_t body;
		} loop;
	};
};

struct ccl_decl {
	enum ccl_type {
		CCL_TYPE_UNIT, /* never cnst; org/art are NULL */
		CCL_TYPE_BOOL,
		CCL_TYPE_INT,
		CCL_TYPE_REAL,
		CCL_TYPE_KLEENEAN, /* always artificial */
	} value_type;
	unsigned is_constant          : 1;
	unsigned origin_is_artificial : 1;
	union {
		const struct cclerical_constant *cnst;
		const struct cclerical_decl *org;
		ccl_insn_id_t art;
	} origin;
	union {
		enum ccl_unit {
			CCL_UNIT_STAR = 0,
		} unit;
		enum ccl_kleenean {
			CCL_KLEENEAN_BOT   = -1,
			CCL_KLEENEAN_FALSE =  0,
			CCL_KLEENEAN_TRUE  =  1,
		} kleenean;
	} art_cnst_value;
};

struct ccl_basic_block {
	ccl_insn_id_t begin, end;
};

struct ccl_fun {
	ccl_decl_id_t retval;
	ccl_bb_id_t body;
};

struct ccl_tu {
	ccl_vec_t insn_storage; /* of struct ccl_insn * */
	ccl_vec_t bb_storage;   /* of struct ccl_basic_block * */
	ccl_vec_t decl_storage; /* of struct ccl_decl * */
	ccl_vec_t fun_storage;  /* of struct ccl_fun * */
	ccl_vec_t compute;      /* of (void *)(uintptr_t)ccl_fun_id_t */
};

void ccl_cfg_add(const struct cclerical_prog *p, const ccl_vec_t *decls,
                 struct ccl_tu *tu);

#endif
