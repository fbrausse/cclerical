
#ifndef CCL_H
#define CCL_H

#include <stdio.h>

#include "cclerical.h"

#define ARRAY_SIZE(...)		(sizeof(__VA_ARGS__)/sizeof(*(__VA_ARGS__)))
#define DIE(code,...) do { fprintf(stderr, __VA_ARGS__); exit(code); } while (0)
#define CCL_PREFIX		"ccl_"

typedef struct cclerical_vector ccl_vec_t;

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

#define CC_OPTS_INIT { 0, 0, CCLERICAL_HIGHLIGHT_AUTO, stdout }

void cclprintf(FILE *f, int lvl, const char *fmt, ...);

struct cclerical_input;

typedef void backend_f(FILE *, const struct cc_opts *opts,
                       const struct cclerical_input *,
                       const struct cclerical_prog *, const ccl_vec_t *);

void export_irram(FILE *out,
                  const struct cc_opts *opts, const struct cclerical_input *,
                  const struct cclerical_prog *p, const ccl_vec_t *decls);

#endif
