
#ifndef CCL_H
#define CCL_H

#include <stdio.h>

#include "cclerical.h"

#define ARRAY_SIZE(...)		(sizeof(__VA_ARGS__)/sizeof(*(__VA_ARGS__)))
#define DIE(code,...) do { fprintf(stderr, __VA_ARGS__); exit(code); } while (0)
#define CCL_PREFIX		"ccl_"

CCLERICAL_VECTOR_DEF(ccl_vec_t,void *)

typedef struct ccl_vec_t ccl_vec_t;

enum {
	OPT_FEAT_CFG_COMPACT_ALIAS,
};

enum {
	OPT_FEAT_CFG_COMPACT,
	OPT_FEAT_N,
};

struct cc_opts {
	int dump_parse_tree;
	int verbosity;
	enum cclerical_highlight_mode highlight;
	FILE *output;
	unsigned feat[OPT_FEAT_N]; /* bitmask */
};

#define CC_OPTS_INIT { 0, 0, CCLERICAL_HIGHLIGHT_AUTO, stdout, {0}, }

void cclprintf(FILE *f, int lvl, const char *fmt, ...);

struct cclerical_input;

typedef void backend_f(FILE *, const struct cc_opts *opts,
                       const struct cclerical_input *,
                       const struct cclerical_prog *,
                       const struct cclerical_vec_decl_ptr *);

void export_irram(FILE *out,
                  const struct cc_opts *opts, const struct cclerical_input *,
                  const struct cclerical_prog *p,
                  const struct cclerical_vec_decl_ptr *decls);

#endif
