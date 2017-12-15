/* SPDX short identifier: BSD-3-Clause */
%{
#include <limits.h>	/* LONG_MAX */
#include <stdarg.h>	/* va_*() */
#include <stdio.h>	/* FILE */

#include "cclerical.h"
#include "cclerical.tab.h"
#include "cclerical.lex.h"

#define YYMAXDEPTH LONG_MAX
#define YYLTYPE_IS_TRIVIAL 1

static void cclerical_error0(YYLTYPE *locp, const char *file, int lineno,
                             const char *fmt, ...);

#define ERROR(locp,...) cclerical_error0(locp, __FILE__, __LINE__, __VA_ARGS__)
#define cclerical_error(locp,p,scanner,...) ERROR(locp, __VA_ARGS__)

static int lookup_var(struct cclerical_parser *p, char *id,
                      cclerical_id_t *v, YYLTYPE *locp, int rw);

#define TYPES_ALL (1U << CCLERICAL_TYPE_UNIT | \
                   1U << CCLERICAL_TYPE_BOOL | \
                   1U << CCLERICAL_TYPE_INT  | \
                   1U << CCLERICAL_TYPE_REAL)

static struct cclerical_expr * expr(struct cclerical_parser *p,
                                   struct cclerical_expr *e,
                                   cclerical_type_set_t forced,
                                   YYLTYPE *locp);
static struct cclerical_expr * expr_new(struct cclerical_parser *p,
                                       struct cclerical_expr *e,
                                       YYLTYPE *locp);

static struct cclerical_expr * fun_call(struct cclerical_parser *p,
                                        YYLTYPE *locp, char *id,
                                        struct cclerical_vector params);

#define EXPR_NEW(e)	do { if (!expr_new(p, e, &yylloc)) YYERROR; } while (0)
#define EXPR(e,forced)	\
	do { if (!expr(p, e, forced, &yylloc)) YYERROR; } while (0)

%}

%parse-param { struct cclerical_parser *p } { void *yyscanner }
%lex-param { void *yyscanner }

%union {
	struct cclerical_expr *expr;
	struct cclerical_stmt *stmt;
	struct cclerical_prog *prog;
	enum cclerical_type type;
	cclerical_id_t varref;
	char *ident;
	struct cclerical_vector cases;
	struct cclerical_constant cnst;
	struct cclerical_vector params;
};

%locations
%define api.pure full
%define parse.error verbose
%define parse.lac full
%defines
%name-prefix "cclerical_"
//%debug
//%verbose

%token-table

%token TK_IF		"if"
%token TK_THEN		"then"
%token TK_ELSE		"else"
%token TK_CASE		"case"
%token TK_SKIP		"skip"
%token TK_LIM		"lim"
%token TK_VAR		"var"
%token TK_IN		"in"
%token TK_WHILE		"while"
%token TK_END		"end"
%token TK_DO		"do"
%token TK_FUN		"function"
%token TK_EXTERNAL	"external"

%token TK_ASGN		":="
%token TK_RSARROW	"->"
%token TK_RDARROW	"=>"
%token TK_NE		"/="
%token TK_BARS		"||"

%token TK_UNIT		"Unit"
%token TK_BOOL		"Bool"
%token TK_INT		"Int"
%token TK_REAL		"Real"

%token <ident> IDENT
%token <cnst> CONSTANT

%precedence ';'
%precedence TK_THEN
%precedence TK_ELSE

%nonassoc '<' '>' TK_NE
%left  '+' '-'
%left  '*' '/'
%precedence UMINUS
%right '^'

%type <prog> prog else_branch
%type <stmt> stmt
%type <expr> expr
%type <type> type utype
%type <cases> cases
%type <varref> fun_decl_param fun_decl
%type <params> extfun_decl_params extfun_decl_params_spec
%type <params> fun_decl_params fun_decl_params_spec
%type <params> fun_call_params fun_call_param_spec

%start tu

%%

tu
  : %empty
  | tu toplevel

toplevel
  : fun_decl
  | TK_DO prog { p->prog = $2; }

fun_decl
  : TK_FUN IDENT { cclerical_parser_open_scope(p); }
    '(' fun_decl_params_spec ')' ':' prog
    {
	free(cclerical_parser_close_scope(p).var_idcs.data);
	cclerical_parser_new_fun(p, $2, cclerical_prog_type($8), $5, $8, &$$);
    }
  | TK_EXTERNAL IDENT '(' extfun_decl_params_spec ')' TK_RSARROW utype
    {
	cclerical_parser_new_fun(p, $2, $7, $4, NULL, &$$);
    }

extfun_decl_params_spec
  : %empty { cclerical_vector_init(&$$); }
  | extfun_decl_params

extfun_decl_params
  : type
    {
	cclerical_vector_init(&$$);
	cclerical_vector_add(&$$, (void *)(uintptr_t)$1);
    }
  | extfun_decl_params ',' type
    {
	$$ = $1;
	cclerical_vector_add(&$$, (void *)(uintptr_t)$3);
    }

fun_decl_params_spec
  : %empty      { cclerical_vector_init(&$$); }
  | fun_decl_params

fun_decl_params
  : fun_decl_param
    {
	cclerical_vector_init(&$$);
	cclerical_vector_add(&$$, (void *)(uintptr_t)$1);
    }
  | fun_decl_params ',' fun_decl_param
    {
	$$ = $1;
	cclerical_vector_add(&$$, (void *)(uintptr_t)$3);
    }

fun_decl_param
  : type IDENT
    {
	int r = cclerical_parser_new_var(p, $2, $1, &$$);
	if (r) {
		ERROR(&yylloc, "error declaring function parameter '%s': %s\n",
		      $2, strerror(r));
		free($2);
		YYERROR;
	}
    }

prog
  : prog ';' stmt
    {
	$$ = $1;
	cclerical_vector_add(&$$->stmts, $3);
    }
  | stmt
    {
	$$ = cclerical_prog_create();
	cclerical_vector_add(&$$->stmts, $1);
    }

stmt
  : IDENT TK_ASGN expr
    {
	cclerical_id_t v;
	if (!lookup_var(p, $1, &v, &yylloc, 1))
		YYERROR;
	struct cclerical_decl *d = p->decls.data[v];
	EXPR($3, 1U << d->value_type);
	$$ = cclerical_stmt_create(CCLERICAL_STMT_ASGN);
	$$->asgn.var = v;
	$$->asgn.expr = $3;
    }
  | TK_SKIP { $$ = cclerical_stmt_create(CCLERICAL_STMT_SKIP); }
  | TK_IF expr TK_THEN prog else_branch TK_END
    {
	EXPR($2, 1U << CCLERICAL_TYPE_BOOL);
	$$ = cclerical_stmt_create(CCLERICAL_STMT_IF);
	$$->branch.cond = $2;
	$$->branch.if_true = $4;
	$$->branch.if_false = $5;
    }
  | TK_WHILE expr TK_DO prog TK_END
    {
	EXPR($2, 1U << CCLERICAL_TYPE_BOOL);
	$$ = cclerical_stmt_create(CCLERICAL_STMT_WHILE);
	$$->loop.cond = $2;
	$$->loop.body = $4;
    }
  | expr
    {
	$$ = cclerical_stmt_create(CCLERICAL_STMT_EXPR);
	$$->expr = $1;
    }

else_branch
  : %empty { $$ = NULL; }
  | TK_ELSE prog { $$ = $2; }

expr
  : expr '+' expr { EXPR_NEW($$ = cclerical_expr_create_op(CCLERICAL_OP_ADD, $1, $3)); }
  | expr '-' expr { EXPR_NEW($$ = cclerical_expr_create_op(CCLERICAL_OP_SUB, $1, $3)); }
  | expr '*' expr { EXPR_NEW($$ = cclerical_expr_create_op(CCLERICAL_OP_MUL, $1, $3)); }
  | expr '/' expr { EXPR_NEW($$ = cclerical_expr_create_op(CCLERICAL_OP_DIV, $1, $3)); }
  | expr '^' expr { EXPR_NEW($$ = cclerical_expr_create_op(CCLERICAL_OP_EXP, $1, $3)); }
  | expr '<' expr { EXPR_NEW($$ = cclerical_expr_create_op(CCLERICAL_OP_LT, $1, $3)); }
  | expr '>' expr { EXPR_NEW($$ = cclerical_expr_create_op(CCLERICAL_OP_GT, $1, $3)); }
  | expr TK_NE expr { EXPR_NEW($$ = cclerical_expr_create_op(CCLERICAL_OP_NE, $1, $3)); }
  | '-' expr %prec UMINUS
    {
	EXPR_NEW($$ = cclerical_expr_create_op(CCLERICAL_OP_NEG, $2, NULL));
    }
  | '(' expr ')'  { $$ = $2; }
  | TK_CASE cases TK_END
    {
	$$ = cclerical_expr_create(CCLERICAL_EXPR_CASE);
	$$->cases = $2;
	EXPR_NEW($$);
    }
  | TK_LIM IDENT
    {
	cclerical_parser_open_scope(p);
	int r = cclerical_parser_new_var(p, $2, CCLERICAL_TYPE_INT, &$<varref>$);
	if (r) {
		ERROR(&yylloc, "error declaring variable '%s' in lim: %s\n",
		      $2, strerror(r));
		free($2);
		YYERROR;
	}
    }
    TK_RDARROW prog TK_END
    {
	$$ = cclerical_expr_create(CCLERICAL_EXPR_LIM);
	$$->lim.seq_idx = $<varref>3;
	$$->lim.seq = $5;
	$$->lim.local = cclerical_parser_close_scope(p);
	EXPR_NEW($$);
    }
  | TK_VAR IDENT TK_ASGN expr ':' type
    {
	EXPR($4, 1U << $6);
	cclerical_parser_open_scope(p);
	int r = cclerical_parser_new_var(p, $2, $6, &$<varref>$);
	if (r) {
		ERROR(&yylloc, "error defining variable '%s': %s\n",
		      $2, strerror(r));
		free($2);
		YYERROR;
	}
    }
    TK_IN prog TK_END
    {
	$$ = cclerical_expr_create(CCLERICAL_EXPR_DECL_ASGN);
	$$->decl_asgn.var  = $<varref>7;
	$$->decl_asgn.expr = $4;
	$$->decl_asgn.prog = $9;
	EXPR_NEW($$);
	free(cclerical_parser_close_scope(p).var_idcs.data);
    }
  | IDENT '(' fun_call_param_spec ')'
    {
	if (!($$ = fun_call(p, &yyloc, $1, $3)))
		YYERROR;
	EXPR_NEW($$);
    }
  | IDENT
    {
	cclerical_id_t v;
	if (!lookup_var(p, $1, &v, &yylloc, 0))
		YYERROR;
	$$ = cclerical_expr_create(CCLERICAL_EXPR_VAR);
	$$->var = v;
	EXPR_NEW($$);
    }
  | CONSTANT
    {
	$$ = cclerical_expr_create(CCLERICAL_EXPR_CNST);
	$$->cnst = $1;
	EXPR_NEW($$);
    }

fun_call_param_spec
  : %empty           { cclerical_vector_init(&$$); }
  | fun_call_params

fun_call_params
  : expr
    {
	cclerical_vector_init(&$$);
	cclerical_vector_add(&$$, $1);
    }
  | fun_call_params ',' expr
    {
	$$ = $1;
	cclerical_vector_add(&$$, $3);
    }

cases
  : expr TK_RDARROW prog
    {
	cclerical_vector_init(&$$);
	cclerical_vector_add(&$$, $1);
	cclerical_vector_add(&$$, $3);
    }
  | cases TK_BARS expr TK_RDARROW prog
    {
	$$ = $1;
	cclerical_vector_add(&$$, $3);
	cclerical_vector_add(&$$, $5);
    }

type
  : TK_BOOL { $$ = CCLERICAL_TYPE_BOOL; }
  | TK_INT  { $$ = CCLERICAL_TYPE_INT; }
  | TK_REAL { $$ = CCLERICAL_TYPE_REAL; }

utype
  : type
  | TK_UNIT { $$ = CCLERICAL_TYPE_UNIT; }

%%

static void print_range(FILE *f, int a, int b)
{
	if (a == b)
		fprintf(f, "%d", a);
	else
		fprintf(f, "%d-%d", a, b);
}

static void cclerical_error0(YYLTYPE *locp, const char *file, int lineno,
                             const char *fmt, ...)
{
#ifndef NDEBUG
	fprintf(stderr, "%s:%d: ", file, lineno);
#else
	(void)file;
	(void)lineno;
#endif
	fprintf(stderr, "error at ");
	print_range(stderr, locp->first_line, locp->last_line);
	fprintf(stderr, ".");
	print_range(stderr, locp->first_column, locp->last_column);
	fprintf(stderr, ": ");
	va_list ap;
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
}

static int lookup_var(struct cclerical_parser *p, char *id,
                      cclerical_id_t *v, YYLTYPE *locp, int rw)
{
	int r = cclerical_parser_var_lookup(p, id, v, rw);
	if (!r) {
		ERROR(locp, "variable '%s' is %s in this context", id,
		      rw && cclerical_parser_var_lookup(p, id, v, 0)
		      ? "read-only" : "not defined");
		free(id);
	}
	return r;
}

static int max_sub_type(cclerical_type_set_t t, enum cclerical_type *res)
{
	if (!t)
		return 0;
	if (t & (1U << CCLERICAL_TYPE_UNIT)) {
		if (t & ~(1U << CCLERICAL_TYPE_UNIT))
			return 0;
		*res = CCLERICAL_TYPE_UNIT;
	} else if (t & (1U << CCLERICAL_TYPE_BOOL)) {
		if (t & ~(1U << CCLERICAL_TYPE_BOOL))
			return 0;
		*res = CCLERICAL_TYPE_BOOL;
	} else
		*res = (t & (1U << CCLERICAL_TYPE_INT)) ? CCLERICAL_TYPE_INT
		                                        : CCLERICAL_TYPE_REAL;
	return 1;
}

static cclerical_type_set_t super_types(enum cclerical_type t)
{
	cclerical_type_set_t res = 1U << t;
	if (t == CCLERICAL_TYPE_INT)
		res |= 1U << CCLERICAL_TYPE_REAL;
	return res;
}

static int unique_t(cclerical_type_set_t s, enum cclerical_type *res)
{
	switch (s) {
	case 1U << CCLERICAL_TYPE_UNIT: *res = CCLERICAL_TYPE_UNIT; return 1;
	case 1U << CCLERICAL_TYPE_BOOL: *res = CCLERICAL_TYPE_BOOL; return 1;
	case 1U << CCLERICAL_TYPE_INT : *res = CCLERICAL_TYPE_INT ; return 1;
	case 1U << CCLERICAL_TYPE_REAL: *res = CCLERICAL_TYPE_REAL; return 1;
	default: return 0;
	}
}

static int is_binary_op(enum cclerical_op op)
{
	return op != CCLERICAL_OP_NEG;
}

static int is_arith_op(enum cclerical_op op)
{
	switch (op) {
	case CCLERICAL_OP_NEG:
	case CCLERICAL_OP_ADD:
	case CCLERICAL_OP_SUB:
	case CCLERICAL_OP_MUL:
	case CCLERICAL_OP_DIV:
	case CCLERICAL_OP_EXP:
		return 1;
	case CCLERICAL_OP_LT:
	case CCLERICAL_OP_GT:
	case CCLERICAL_OP_NE:
		return 0;
	}
	return -1;
}

static int expr_super_types(const struct cclerical_parser *p,
                            const struct cclerical_expr *e, YYLTYPE *locp,
                            cclerical_type_set_t *res)
{
	enum cclerical_type expr_t = CCLERICAL_TYPE_UNIT; /* silence uninit-use warning */
	switch (e->type) {
	case CCLERICAL_EXPR_OP: {
		cclerical_type_set_t arg_t = 1U << e->op.arg1->result_type;
		if (!is_binary_op(e->op.op))
			arg_t |= 1U << e->op.arg2->result_type;
		if (arg_t & (1U << CCLERICAL_TYPE_UNIT)) {
			ERROR(locp, "operand of Unit type");
			return 0;
		}
		if ((e->op.op == CCLERICAL_OP_LT || e->op.op == CCLERICAL_OP_GT)
		    && (arg_t & (1U << CCLERICAL_TYPE_BOOL))) {
			ERROR(locp, "comparison with Boolean type");
			return 0;
		}
		if (!is_arith_op(e->op.op)) {
			expr_t = CCLERICAL_TYPE_BOOL;
		} else if (!unique_t(arg_t, &expr_t)) {
			ERROR(locp, "mixed-type op expression");
			return 0;
		}
		break;
	}
	case CCLERICAL_EXPR_LIM:
		expr_t = cclerical_prog_type(e->lim.seq);
		break;
	case CCLERICAL_EXPR_CASE: {
		cclerical_type_set_t arg_t = 0;
		for (size_t i=0; i<e->cases.valid; i+=2)
			arg_t |= 1U << cclerical_prog_type(e->cases.data[i+1]);
		if (!unique_t(arg_t, &expr_t)) {
			ERROR(locp, "mixed-type case expression");
			return 0;
		}
		break;
	}
	case CCLERICAL_EXPR_CNST:
		expr_t = e->cnst.lower_type;
		break;
	case CCLERICAL_EXPR_DECL_ASGN:
		expr_t = cclerical_prog_type(e->decl_asgn.prog);
		break;
	case CCLERICAL_EXPR_VAR: {
		const struct cclerical_decl *v = p->decls.data[e->var];
		expr_t = v->value_type;
		break;
	}
	case CCLERICAL_EXPR_FUN_CALL: {
		const struct cclerical_decl *f = p->decls.data[e->fun_call.fun];
		expr_t = f->value_type;
		break;
	}
	}
	*res = super_types(expr_t);
	return 1;
}

static struct cclerical_expr * expr(struct cclerical_parser *p,
                                    struct cclerical_expr *e,
                                    cclerical_type_set_t forced,
                                    YYLTYPE *locp)
{
	cclerical_type_set_t convertible_to;
	if (!expr_super_types(p, e, locp, &convertible_to))
		return NULL;
	cclerical_type_set_t common = convertible_to & forced;
	if (!common) {
		ERROR(locp, "no common type in expr-super-types 0x%x and "
		            "forced types 0x%x", convertible_to, forced);
		return NULL;
	}
	if (!max_sub_type(common, &e->result_type)) {
		ERROR(locp, "no common sub-type in 0x%x for complete expr",
		      common);
		return NULL;
	}
	return e;
}

static struct cclerical_expr * expr_new(struct cclerical_parser *p,
                                       struct cclerical_expr *e,
                                       YYLTYPE *locp)
{
	if (expr(p, e, TYPES_ALL, locp))
		return e;
	free(e);
	return NULL;
}

static struct cclerical_expr * fun_call(struct cclerical_parser *p,
                                        YYLTYPE *locp, char *id,
                                        struct cclerical_vector params)
{
	cclerical_id_t v;
	if (!lookup_var(p, id, &v, locp, 0)) {
		ERROR(locp, "call to undeclared function identifier '%s'", id);
		return NULL;
	}
	struct cclerical_decl *d = p->decls.data[v];
	if (d->type != CCLERICAL_DECL_FUN) {
		ERROR(locp, "'%s' does not identify a function", d->id);
		return NULL;
	}
	if (d->fun.arguments.valid != params.valid) {
		ERROR(locp, "in function-call to %s: number of arguments "
		            "mismatch: passed: %zu, declared with: %zu",
		      d->id, params.valid, d->fun.arguments.valid);
		return NULL;
	}
	for (size_t i=0; i<params.valid; i++) {
		struct cclerical_expr *e = params.data[i];
		cclerical_id_t param = (uintptr_t)d->fun.arguments.data[i];
		struct cclerical_decl *dparam = p->decls.data[param];
		if ((1U << dparam->value_type) & super_types(e->result_type))
			continue;
		ERROR(locp, "in function-call to %s: type mismatch of argument "
		            "%zu: exprected %s, expression is of type %s",
		     d->id, CCLERICAL_TYPE_STR[dparam->value_type],
		     CCLERICAL_TYPE_STR[e->result_type]);
		return NULL;
	}
	struct cclerical_expr *e = cclerical_expr_create(CCLERICAL_EXPR_FUN_CALL);
	e->fun_call.fun = v;
	e->fun_call.params = params;
	return e;
}
