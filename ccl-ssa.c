
#include <assert.h>
#include <stdio.h>

#include "ccl-ssa.h"

/* warning: ccl_{tu,cfg}_*_add*() invalidate the returned pointer */
static struct ccl_decl * get_decl(const struct ccl_tu *tu, ccl_decl_id_t id)
{
	return tu->decl_storage.data[id.id];
}

/* warning: ccl_{tu,cfg}_*_add*() invalidate the returned pointer */
static struct ccl_insn * get_insn(const struct ccl_tu *tu, ccl_insn_id_t id)
{
	return tu->insn_storage.data[id.id];
}

static struct ccl_insn_asgn * get_asgn(const struct ccl_tu *tu,
                                       ccl_insn_id_t id)
{
	struct ccl_insn *in = get_insn(tu, id);
	assert(in->type == CCL_INSN_ASGN);
	return &in->asgn;
}

static struct ccl_decl * ccl_decl_create(const struct cclerical_decl *d)
{
	struct ccl_decl r = {
		.value_type           = d->value_type,
		.is_constant          = d->value_type == CCLERICAL_TYPE_UNIT,
		.origin_is_artificial = 0,
		.origin               = { .org = d, },
		.art_cnst_value       = { .unit = CCL_UNIT_STAR, }
	};
	return memdup(&r, sizeof(r));
}

void ccl_tu_init(struct ccl_tu *tu, const ccl_vec_t *decls)
{
	memset(tu, 0, sizeof(*tu));
	cclerical_vector_ensure_size(&tu->decl_storage, decls->valid);
	tu->decl_storage.valid = decls->valid;
	for (size_t i=0; i<decls->valid; i++) {
		const struct cclerical_decl *d = decls->data[i];
		tu->decl_storage.data[i] = ccl_decl_create(d);
	}
	for (size_t i=0; i<decls->valid; i++) {
		const struct cclerical_decl *d = decls->data[i];
		if (d->type != CCLERICAL_DECL_FUN ||
		    cclerical_decl_fun_is_external(d))
			continue;
		ccl_fun_id_t fun_id = ccl_cfg_add(tu, d->fun.body, d->source_loc);
		struct ccl_decl *dd = get_decl(tu, (ccl_decl_id_t){ .id = i });
		dd->fun_id = fun_id;
	}
}

ccl_decl_id_t ccl_tu_decl_add(struct ccl_tu *tu)
{
	ccl_decl_id_t r = { .id = tu->decl_storage.valid };
	struct ccl_decl *d = calloc(1, sizeof(struct ccl_decl));
	cclerical_vector_add(&tu->decl_storage, d);
	return r;
}

ccl_decl_id_t ccl_tu_decl_add_art(struct ccl_tu *tu, enum cclerical_type t,
                                  ccl_insn_id_t origin_id)
{
	ccl_decl_id_t r = ccl_tu_decl_add(tu);
	struct ccl_decl *lhs = get_decl(tu, r);
	lhs->value_type           = t;
	lhs->is_constant          = t == CCLERICAL_TYPE_UNIT;
	lhs->origin_is_artificial = 1;
	lhs->origin.art           = origin_id;
	lhs->art_cnst_value.unit  = CCL_UNIT_STAR; /* TODO */
	return r;
}

ccl_insn_id_t ccl_tu_insn_add(struct ccl_tu *tu, enum ccl_insn_type type,
                              struct cclerical_source_loc source_loc)
{
	ccl_insn_id_t r = { .id = tu->insn_storage.valid };
	struct ccl_insn *in = calloc(1, sizeof(struct ccl_insn));
	in->type = type;
	in->source_loc = source_loc;
	cclerical_vector_add(&tu->insn_storage, in);
	return r;
}

static ccl_insn_id_t ccl_tu_insn_add_asgn(struct ccl_tu *tu,
                                          struct cclerical_source_loc source_loc,
                                          enum ccl_insn_asgn_type type,
                                          ccl_decl_id_t lhs, ccl_insn_id_t next)
{
	ccl_insn_id_t id = ccl_tu_insn_add(tu, CCL_INSN_ASGN, source_loc);
	struct ccl_insn *r = get_insn(tu, id);
	r->asgn.type = type;
	r->asgn.lhs  = lhs;
	r->asgn.next = next;
	return id;
}

/* if (asgn_to) { assign the result of this sequence to *asgn_to } */
ccl_insn_id_t ccl_cfg_add_prog(struct ccl_tu *tu,
                               const struct cclerical_prog *p,
                               ccl_insn_id_t next,
                               const ccl_decl_id_t *asgn_to);

/* if (asgn_to) { assign the result of this expression to *asgn_to } */
ccl_insn_id_t ccl_cfg_add_expr(struct ccl_tu *tu,
                               const struct cclerical_expr *e,
                               ccl_insn_id_t next,
                               const ccl_decl_id_t *asgn_to);

static ccl_insn_id_t ssa_asgn_new(struct ccl_tu *tu,
                                  const struct cclerical_expr *e,
                                  ccl_insn_id_t next,
                                  enum ccl_insn_asgn_type asgn_type,
                                  const ccl_decl_id_t *asgn_to)
{
	ccl_decl_id_t cnst_id;
	if (e->type == CCLERICAL_EXPR_CNST) {
		/* TODO: clarify semantics of art_cnst and origin */
		/* cnsts have not been added to decls yet, do so now */
		cnst_id = ccl_tu_decl_add(tu); /* new variable */
		struct ccl_decl *cnst = get_decl(tu, cnst_id);
		cnst->value_type = e->result_type;
		cnst->is_constant = 1;
		cnst->origin_is_artificial = 0;
		cnst->origin.cnst = &e->cnst;
	}

	/* this function handles pure expressions */
	if (!asgn_to) {
		fprintf(stderr, "warning: dropping unused pure expression\n");
		return next;
	}

	ccl_decl_id_t lhs_id = asgn_to ? *asgn_to : ccl_tu_decl_add(tu); /* new variable */
	ccl_insn_id_t in_asgn_id = ccl_tu_insn_add_asgn(tu, e->source_loc, asgn_type, lhs_id, next);
	if (asgn_to) {
		struct ccl_decl *lhs = get_decl(tu, lhs_id);
		assert(lhs->value_type == e->result_type);
		assert(lhs->is_constant || (e->result_type != CCLERICAL_TYPE_UNIT));
	}

	ccl_insn_id_t chain_id = in_asgn_id;
	switch (asgn_type) {
	case CCL_INSN_ASGN_ALIAS:
		if (e->type == CCLERICAL_EXPR_VAR) {
			struct ccl_insn_asgn *asgn = get_asgn(tu, in_asgn_id);
			asgn->alias.decl.id = e->var;
			break;
		}
		if (e->type == CCLERICAL_EXPR_CNST) {
			struct ccl_insn_asgn *asgn = get_asgn(tu, in_asgn_id);
			asgn->alias.decl = cnst_id;
			break;
		}
		{
			struct ccl_insn_asgn *asgn = get_asgn(tu, in_asgn_id);
			asgn->alias.decl.id = e->fun_call.fun;
			cclerical_vector_ensure_size(&asgn->alias.args,
			                             e->fun_call.params.valid);
			asgn->alias.args.valid = e->fun_call.params.valid;
		}
		/* fun_call args pure exprs */
		for (size_t i=e->fun_call.params.valid; i; i--) {
			const struct cclerical_expr *p = e->fun_call.params.data[i-1];
			ccl_decl_id_t par_id = ccl_tu_decl_add_art(tu, p->result_type, in_asgn_id);
			chain_id = ccl_cfg_add_expr(tu, p, chain_id, &par_id);
			struct ccl_insn_asgn *asgn = get_asgn(tu, in_asgn_id);
			asgn->alias.args.data[i-1] = (void *)(uintptr_t)par_id.id;
		}
		break;
	case CCL_INSN_ASGN_LIM: {
		/* lim-bodies have not been defined yet, do so now */
		/* TODO: is next == chain_id wrong?
		 *       make body a new ccl_fun? -> no, it's like an anon fun_call */
		ccl_insn_id_t body = ccl_cfg_add_expr(tu, e->lim.seq, chain_id, asgn_to);
		struct ccl_insn_asgn *asgn = get_asgn(tu, in_asgn_id);
		asgn->lim.seq_idx.id = e->lim.seq_idx; /* already exists (as a variable) */
		asgn->lim.body = body;
		break;
	}
	case CCL_INSN_ASGN_OP: {
		{
			struct ccl_insn_asgn *asgn = get_asgn(tu, in_asgn_id);
			asgn->op.op = e->op.op;
		}
		for (unsigned i=cclerical_op_arity(e->op.op); i; i--) {
			const struct cclerical_expr *f = e->op.args[i-1];
			ccl_decl_id_t op_p = ccl_tu_decl_add_art(tu, f->result_type, in_asgn_id);
			chain_id = ccl_cfg_add_expr(tu, e->op.args[i-1], chain_id, &op_p);
			struct ccl_insn_asgn *asgn = get_asgn(tu, in_asgn_id);
			asgn->op.args[i-1] = op_p;
		}
		break;
	}
	}
	return chain_id;
}

ccl_insn_id_t ccl_cfg_add_expr(struct ccl_tu *tu,
                               const struct cclerical_expr *e,
                               ccl_insn_id_t next,
                               const ccl_decl_id_t *asgn_to)
{
	assert(!asgn_to || get_decl(tu, *asgn_to)->value_type == e->result_type);
	switch (e->type) {
	/* return value is of type CCL_INSN_ASGN, next will be copied */
	case CCLERICAL_EXPR_ASGN: {
		/* already exists (as a variable) */
		ccl_decl_id_t lhs = { .id = e->var };
		assert(!asgn_to || asgn_to->id == e->var);
		return ccl_cfg_add_expr(tu, e->asgn.expr, next, &lhs);
	}
	case CCLERICAL_EXPR_VAR: /* fall through */
	case CCLERICAL_EXPR_CNST: /* fall through */
	case CCLERICAL_EXPR_FUN_CALL:
		return ssa_asgn_new(tu, e, next, CCL_INSN_ASGN_ALIAS, asgn_to);
	case CCLERICAL_EXPR_LIM:
		return ssa_asgn_new(tu, e, next, CCL_INSN_ASGN_LIM, asgn_to);
	case CCLERICAL_EXPR_OP:
		return ssa_asgn_new(tu, e, next, CCL_INSN_ASGN_OP, asgn_to);
	case CCLERICAL_EXPR_SEQ:
		return ccl_cfg_add_prog(tu, e->seq, next, asgn_to);
	case CCLERICAL_EXPR_SKIP:
		assert(!asgn_to || get_decl(tu, *asgn_to)->value_type == CCL_TYPE_UNIT);
		return next;
	case CCLERICAL_EXPR_DECL_ASGN: {
		/* treat as sequence of assignments --
		 * we care about scope / declarations of variables after
		 * liveliness analysis */
		ccl_insn_id_t chain_id = ccl_cfg_add_expr(tu, e->decl_asgn.body, next, asgn_to);
		const struct cclerical_vector *inits = &e->decl_asgn.inits;
		for (size_t i=inits->valid; i; i-=2) {
			/* already exists (as a variable) */
			ccl_decl_id_t lhs = {
				.id = (uintptr_t)inits->data[i-2]
			};
			const struct cclerical_expr *rhs = inits->data[i-1];
			chain_id = ccl_cfg_add_expr(tu, rhs, chain_id, &lhs);
		}
		return chain_id;
	}
	case CCLERICAL_EXPR_CASE: {
		ccl_insn_id_t cases_id = ccl_tu_insn_add(tu, CCL_INSN_CASE, e->source_loc);
		{
			struct ccl_insn *cases = get_insn(tu, cases_id);
			size_t n = e->cases.valid / 2;
			cclerical_vector_ensure_size(&cases->cases.conds, n);
			cclerical_vector_ensure_size(&cases->cases.bodies, n);
			cases->cases.conds.valid = n;
			cases->cases.bodies.valid = n;
		}
		ccl_insn_id_t chain_id = cases_id;
		for (size_t i=e->cases.valid; i; i-=2) {
			const struct cclerical_expr *c = e->cases.data[i-2];
			const struct cclerical_expr *b = e->cases.data[i-1];
			ccl_decl_id_t cond_var = ccl_tu_decl_add_art(tu, c->result_type, cases_id);
			chain_id = ccl_cfg_add_expr(tu, c, chain_id, &cond_var);
			ccl_insn_id_t body_id = ccl_cfg_add_expr(tu, b, next, asgn_to);
			struct ccl_insn *cases = get_insn(tu, cases_id);
			cases->cases.conds.data[(i-2)/2] = (void *)(uintptr_t)cond_var.id;
			cases->cases.bodies.data[(i-2)/2] = (void *)(uintptr_t)body_id.id;
		}
		return chain_id;
	}
	case CCLERICAL_EXPR_IF: {
		ccl_insn_id_t branch_id, cond_id, if0, if1;
		branch_id = ccl_tu_insn_add(tu, CCL_INSN_IF, e->source_loc);
		ccl_decl_id_t c = ccl_tu_decl_add_art(tu, e->branch.cond->result_type, branch_id);
		cond_id = ccl_cfg_add_expr(tu, e->branch.cond, branch_id, &c);
		if1 = ccl_cfg_add_expr(tu, e->branch.if_true, next, asgn_to);
		if0 = e->branch.if_false
		    ? ccl_cfg_add_expr(tu, e->branch.if_false, next, asgn_to)
		    : next;
		struct ccl_insn *branch = get_insn(tu, branch_id);
		branch->branch.cond = c;
		branch->branch.if_true = if1;
		branch->branch.if_false = if0;
		return cond_id;
	}
	case CCLERICAL_EXPR_WHILE: {
		assert(!asgn_to);
		ccl_insn_id_t loop_id = ccl_tu_insn_add(tu, CCL_INSN_WHILE, e->source_loc);
		ccl_decl_id_t c = ccl_tu_decl_add_art(tu, e->loop.cond->result_type, loop_id);
		ccl_insn_id_t cond_id = ccl_cfg_add_expr(tu, e->loop.cond,
		                                         loop_id, &c);
		ccl_insn_id_t body_id = ccl_cfg_add_expr(tu, e->loop.body,
		                                         cond_id, NULL);
		struct ccl_insn *loop = get_insn(tu, loop_id);
		loop->loop.cond = c;
		loop->loop.body = body_id;
		loop->loop.next = next;
		return cond_id;
	}
	}
	abort();
}

ccl_insn_id_t ccl_cfg_add_prog(struct ccl_tu *tu,
                               const struct cclerical_prog *p,
                               ccl_insn_id_t next,
                               const ccl_decl_id_t *asgn_to)
{
	ccl_insn_id_t ret = next;
	for (size_t i=p->exprs.valid; i; i--) {
		const struct cclerical_expr *e = p->exprs.data[i-1];
		ret = ccl_cfg_add_expr(tu, e, ret, asgn_to);
		asgn_to = NULL;
	}
	return ret;
}

ccl_fun_id_t ccl_cfg_add(struct ccl_tu *tu, const struct cclerical_prog *p, struct cclerical_source_loc source_loc)
{
	ccl_insn_id_t endfun = ccl_tu_insn_add(tu, CCL_INSN_RETURN, source_loc);
	ccl_decl_id_t ret = ccl_tu_decl_add_art(tu, cclerical_prog_type(p), endfun);
	get_insn(tu, endfun)->retval = ret;

	ccl_insn_id_t start = ccl_cfg_add_prog(tu, p, endfun, &ret);

	ccl_fun_id_t fun = { .id = tu->fun_storage.valid };
	struct ccl_fun fundat = {
		.retval = ret,
		.body   = start,
	};
	cclerical_vector_add(&tu->fun_storage, memdup(&fundat, sizeof(fundat)));
	return fun;
}
