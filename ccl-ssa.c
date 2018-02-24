
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
		.value_type           = (enum ccl_type)d->value_type,
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
	lhs->value_type           = (enum ccl_type)t;
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
		cnst->value_type = (enum ccl_type)e->result_type;
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
		assert(lhs->value_type == (enum ccl_type)e->result_type);
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
	assert(!asgn_to || (get_decl(tu, *asgn_to)->value_type
	                    == (enum ccl_type)e->result_type));
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

static void ccl_vec_init2(ccl_vec_t *v, size_t n)
{
	cclerical_vector_ensure_size(v, n);
	memset(v->data, 0, sizeof(*v->data) * n);
	v->valid = n;
}

static void ccl_vec_add_id(ccl_vec_t *v, size_t id)
{
	cclerical_vector_add(v, (void *)(uintptr_t)id);
}

static size_t ccl_vec_get_id(const ccl_vec_t *v, size_t i)
{
	return (uintptr_t)v->data[i];
}

static void ccl_vec_set_id(const ccl_vec_t *v, size_t i, size_t id)
{
	v->data[i] = (void *)(uintptr_t)id;
}

static void ccl_vec_addn(ccl_vec_t *v, void **entries, size_t n)
{
	cclerical_vector_ensure_size(v, v->valid + n);
	memcpy(v->data + v->valid, entries, sizeof(*entries) * n);
	v->valid += n;
}

static void ccl_vec_swap(ccl_vec_t *a, ccl_vec_t *b)
{
	ccl_vec_t c = *a;
	*a = *b;
	*b = c;
}

static void ccl_vec_reset(ccl_vec_t *v)
{
	cclerical_vector_fini(v);
	memset(v, 0, sizeof(*v));
}

/* --------------------------------------------------------------------------
 * ccl_cfg_bb
 * -------------------------------------------------------------------------- */

static struct ccl_basic_block * get_bb(const struct ccl_cfg_bb *cbb,
                                       ccl_bb_id_t id)
{
	return cbb->bb_storage.data[id.id];
}

static void bb_add_edge(struct ccl_cfg_bb *cbb, ccl_bb_id_t src, ccl_bb_id_t tgt)
{
	ccl_vec_add_id(&get_bb(cbb, src)->out, tgt.id);
	ccl_vec_add_id(&get_bb(cbb, tgt)->in, src.id);
}

static void bb_add_edges(struct ccl_cfg_bb *cbb, ccl_bb_id_t src, const ccl_vec_t *tgts)
{
	size_t n = tgts->valid;
	struct ccl_basic_block *a = get_bb(cbb, src);
	ccl_vec_addn(&a->out, tgts->data, n);
	for (size_t i=0; i<n; i++) {
		ccl_bb_id_t tgt = { .id = (uintptr_t)tgts->data[i] };
		ccl_vec_add_id(&get_bb(cbb, tgt)->in, src.id);
	}
}

static void make_unique(ccl_vec_t *vec, int (*cmp)(const void *, const void *))
{
	qsort(vec->data, vec->valid, sizeof(*vec->data), cmp);
	size_t j = vec->valid != 0;
	for (size_t i=1; i<vec->valid; i++) {
		if (cmp(vec->data + (j-1), vec->data + i))
			vec->data[j++] = vec->data[i];
	}
	vec->valid = j;
}

static int cmp_uptr(const void *a, const void *b)
{
	const uintptr_t *va = a, *vb = b;
	return *va < *vb ? -1 : *va > *vb ? +1 : 0;
}

static void bb_fin_edges(struct ccl_cfg_bb *cbb, ccl_bb_id_t vid)
{
	struct ccl_basic_block *v = get_bb(cbb, vid);
	make_unique(&v->out, cmp_uptr);
	make_unique(&v->in, cmp_uptr);
}

static void bb_contract(struct ccl_cfg_bb *cbb, ccl_bb_id_t a, ccl_bb_id_t b)
{
	struct ccl_basic_block *v = get_bb(cbb, a);
	struct ccl_basic_block *w = get_bb(cbb, b);

	/* TODO: this is bad... */
	for (size_t i=0; i<w->out.valid; i++) {
		ccl_bb_id_t succ = { .id = ccl_vec_get_id(&w->out, i) };
		struct ccl_basic_block *bb = get_bb(cbb, succ);
		for (size_t j=0; j<bb->in.valid; j++)
			if (ccl_vec_get_id(&bb->in, j) == b.id)
				ccl_vec_set_id(&bb->in, j, a.id);
		make_unique(&bb->in, cmp_uptr);
	}
	ccl_vec_swap(&v->out, &w->out);
	ccl_vec_reset(&w->out);
	ccl_vec_reset(&w->in);
	ccl_vec_addn(&v->insns, w->insns.data, w->insns.valid);
	for (size_t i=0; i<w->insns.valid; i++) {
		ccl_insn_id_t in = { .id = ccl_vec_get_id(&w->insns, i) };
		assert(ccl_vec_get_id(&cbb->entries, in.id) == b.id);
		ccl_vec_set_id(&cbb->entries, in.id, a.id);
	}
	ccl_vec_reset(&w->insns);
}

void ccl_cfg_bb_init(struct ccl_cfg_bb *cbb, const struct ccl_tu *cfg)
{
	size_t n = cfg->insn_storage.valid;
	memset(cbb, 0, sizeof(*cbb));

	/* make nodes */
	ccl_vec_init2(&cbb->bb_storage, n);
	ccl_vec_init2(&cbb->entries, n);
	for (size_t i=0; i<n; i++) {
		struct ccl_basic_block bb = {
			.insns = CCLERICAL_VECTOR_INIT,
			.in = CCLERICAL_VECTOR_INIT,
			.out = CCLERICAL_VECTOR_INIT,
		};
		ccl_vec_add_id(&bb.insns, i);
		cbb->bb_storage.data[i] = memdup(&bb, sizeof(bb));
		ccl_vec_set_id(&cbb->entries, i, i);
	}

	/* connect nodes */
	for (size_t i=0; i<n; i++) {
		const struct ccl_insn *in = get_insn(cfg, (ccl_insn_id_t){ .id = i });
		ccl_bb_id_t a = { .id = i };
		switch (in->type) {
		case CCL_INSN_ASGN:
			bb_add_edge(cbb, a, (ccl_bb_id_t){ .id = in->asgn.next.id });
			break;
		case CCL_INSN_CASE:
			bb_add_edges(cbb, a, &in->cases.bodies);
			break;
		case CCL_INSN_IF:
			bb_add_edge(cbb, a, (ccl_bb_id_t){ .id = in->branch.if_true.id });
			bb_add_edge(cbb, a, (ccl_bb_id_t){ .id = in->branch.if_false.id });
			break;
		case CCL_INSN_RETURN:
			break;
		case CCL_INSN_WHILE:
			bb_add_edge(cbb, a, (ccl_bb_id_t){ .id = in->loop.body.id });
			bb_add_edge(cbb, a, (ccl_bb_id_t){ .id = in->loop.next.id });
			break;
		}
	}

	for (size_t i=0; i<n; i++) {
		ccl_bb_id_t a = { .id = i };
		bb_fin_edges(cbb, a);
	}

	/* contract paths */
	int changes;
	do {
		changes = 0;
		for (ccl_bb_id_t vid = { .id = 0 }; vid.id < n; vid.id++) {
			struct ccl_basic_block *v = get_bb(cbb, vid);
			if (v->out.valid != 1)
				continue;
			ccl_bb_id_t wid = { .id = ccl_vec_get_id(&v->out, 0) };
			struct ccl_basic_block *w = get_bb(cbb, wid);
			if (w->in.valid != 1)
				continue;
			/* merge v and w */
			bb_contract(cbb, vid, wid);
			changes = 1;
			vid.id--;
		}
	} while (changes);
}
