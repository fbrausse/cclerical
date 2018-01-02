
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
	FILE *output;
	unsigned feat[OPT_FEAT_N]; /* bitmask */
};

void cclprintf(FILE *f, int lvl, const char *fmt, ...);

typedef void backend_f(FILE *,
                       const struct cclerical_prog *, const ccl_vec_t *);

void export_irram(FILE *out,
                  const struct cclerical_prog *p, const ccl_vec_t *decls);

#endif
