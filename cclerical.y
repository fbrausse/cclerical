
%{
#include <limits.h>	/* LONG_MAX */
#include <stdarg.h>	/* va_*() */
#include <stdio.h>	/* FILE */

#include "cclerical.h"
#include "cclerical.tab.h"
#include "cclerical.lex.h"

#define YYMAXDEPTH LONG_MAX
#define YYLTYPE_IS_TRIVIAL 1

static void cclerical_error0(YYLTYPE *locp, const struct cclerical_parser *p,
                             void *scanner, const char *file, int lineno,
                             const char *fmt, ...);

#define cclerical_error(locp,p,scanner,...) \
	cclerical_error0(locp, p, scanner, __FILE__, __LINE__, __VA_ARGS__)

//int cclerical_lex(YYSTYPE *lvalp, YYLTYPE *llocp);

static int lookup_var(struct cclerical_parser *p, char *id,
                      cclerical_id_t *v, YYLTYPE *locp, int rw);

static const unsigned TYPES_ALL = 1U << CCLERICAL_TYPE_UNIT
                                | 1U << CCLERICAL_TYPE_BOOL
                                | 1U << CCLERICAL_TYPE_INT
                                | 1U << CCLERICAL_TYPE_REAL;

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

%token TK_ASGN		":="
%token TK_RARROW	"=>"
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

%type <prog> prog
%type <stmt> stmt
%type <expr> expr var_init
%type <type> type
%type <cases> cases
%type <varref> lim_init fun_decl_param fun_decl
%type <params> fun_decl_params fun_decl_params_spec
%type <params> fun_call_params fun_call_param_spec
%type <ident> fun_decl_init

%start tu

%%

tu
  : %empty
  | tu toplevel

toplevel
  : fun_decl
  | TK_DO prog { p->prog = $2; }

fun_decl
  : fun_decl_init '(' fun_decl_params_spec ')' ':' prog
    {
	free(cclerical_parser_close_scope(p).var_idcs.data);
	cclerical_parser_new_fun(p, $1, $3, $6, &$$);
    }

fun_decl_init
  : TK_FUN IDENT { cclerical_parser_open_scope(p); $$ = $2; }

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
	cclerical_vector_add(&$$, (void *)(uintptr_t)$3);
    }

fun_decl_param
  : type IDENT
    {
	int r = cclerical_parser_new_var(p, $2, $1, &$$);
	if (r) {
		cclerical_error(&yylloc, p, yyscanner,
		                "error declaring function parameter '%s': %s\n",
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
	EXPR($3, 1U << d->var.type);
	$$ = cclerical_stmt_create(CCLERICAL_STMT_ASGN);
	$$->asgn.var = v;
	$$->asgn.expr = $3;
    }
  | TK_SKIP { $$ = cclerical_stmt_create(CCLERICAL_STMT_SKIP); }
  | TK_IF expr TK_THEN prog TK_ELSE prog TK_END
    {
	EXPR($2, 1U << CCLERICAL_TYPE_BOOL);
	$$ = cclerical_stmt_create(CCLERICAL_STMT_IF);
	$$->branch.cond = $2;
	$$->branch.if_true = $4;
	$$->branch.if_false = $6;
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

expr
  : expr '+' expr { EXPR_NEW($$ = cclerical_expr_create_op(CCLERICAL_OP_PLUS, $1, $3)); }
  | expr '-' expr { EXPR_NEW($$ = cclerical_expr_create_op(CCLERICAL_OP_MINUS, $1, $3)); }
  | expr '*' expr { EXPR_NEW($$ = cclerical_expr_create_op(CCLERICAL_OP_MUL, $1, $3)); }
  | expr '/' expr { EXPR_NEW($$ = cclerical_expr_create_op(CCLERICAL_OP_DIV, $1, $3)); }
  | expr '^' expr { EXPR_NEW($$ = cclerical_expr_create_op(CCLERICAL_OP_EXP, $1, $3)); }
  | expr '<' expr { EXPR_NEW($$ = cclerical_expr_create_op(CCLERICAL_OP_LT, $1, $3)); }
  | expr '>' expr { EXPR_NEW($$ = cclerical_expr_create_op(CCLERICAL_OP_GT, $1, $3)); }
  | expr TK_NE expr { EXPR_NEW($$ = cclerical_expr_create_op(CCLERICAL_OP_NE, $1, $3)); }
  | '-' expr %prec UMINUS
    {
	EXPR_NEW($$ = cclerical_expr_create_op(CCLERICAL_OP_UMINUS, $2, NULL));
    }
  | '(' expr ')'  { $$ = $2; }
  | TK_CASE cases TK_END
    {
	$$ = cclerical_expr_create(CCLERICAL_EXPR_CASE);
	$$->cases = $2;
	EXPR_NEW($$);
    }
  | lim_init TK_RARROW prog TK_END
    {
	$$ = cclerical_expr_create(CCLERICAL_EXPR_LIM);
	$$->lim.seq_idx = $1;
	$$->lim.seq = $3;
	$$->lim.local = cclerical_parser_close_scope(p);
	EXPR_NEW($$);
    }
  | var_init TK_IN prog TK_END
    {
	$$ = $1;
	$$->decl_asgn.prog = $3;
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

var_init
  : TK_VAR IDENT TK_ASGN expr ':' type
    {
	EXPR($4, 1U << $6);
	cclerical_parser_open_scope(p);
	cclerical_id_t v;
	int r = cclerical_parser_new_var(p, $2, $6, &v);
	if (r) {
		cclerical_error(&yylloc, p, yyscanner,
		                "error defining variable '%s': %s\n", $2,
		                strerror(r));
		free($2);
		YYERROR;
	}
	$$ = cclerical_expr_create(CCLERICAL_EXPR_DECL_ASGN);
	$$->decl_asgn.var  = v;
	$$->decl_asgn.expr = $4;
    }

lim_init
  : TK_LIM IDENT
    {
	cclerical_parser_open_scope(p);
	int r = cclerical_parser_new_var(p, $2, CCLERICAL_TYPE_INT, &$$);
	if (r) {
		cclerical_error(&yylloc, p, yyscanner,
		                "error declaring variable '%s' in lim: %s\n",
		                $2, strerror(r));
		free($2);
		YYERROR;
	}
    }

cases
  : expr TK_RARROW prog
    {
	memset(&$$, 0, sizeof($$));
	cclerical_vector_add(&$$, $1);
	cclerical_vector_add(&$$, $3);
    }
  | cases TK_BARS expr TK_RARROW prog
    {
	$$ = $1;
	cclerical_vector_add(&$$, $3);
	cclerical_vector_add(&$$, $5);
    }

type
  : TK_BOOL { $$ = CCLERICAL_TYPE_BOOL; }
  | TK_INT  { $$ = CCLERICAL_TYPE_INT; }
  | TK_REAL { $$ = CCLERICAL_TYPE_REAL; }

%%

static inline void cclerical_error0(YYLTYPE *locp,
                                    const struct cclerical_parser *p,
                                    void *scanner, const char *file, int lineno,
                                    const char *fmt, ...)
{
	fprintf(stderr,
#ifndef NDEBUG
	        "%s:%d: error at %d-%d:%d-%d: ",
	        file, lineno,
#else
	        "error at %d-%d:%d-%d: ",
#endif
	        locp->first_line, locp->last_line,
	        locp->first_column, locp->last_column);
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
		cclerical_error(locp, p, NULL,
		                "variable '%s' is %s in this context", id,
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
	return op != CCLERICAL_OP_UMINUS;
}

static int is_arith_op(enum cclerical_op op)
{
	switch (op) {
	case CCLERICAL_OP_PLUS:
	case CCLERICAL_OP_MINUS:
	case CCLERICAL_OP_MUL:
	case CCLERICAL_OP_DIV:
	case CCLERICAL_OP_EXP:
	case CCLERICAL_OP_UMINUS:
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
			cclerical_error(locp, p, NULL, "operand of Unit type");
			return 0;
		}
		if (!is_arith_op(e->op.op)) {
			if ((e->op.op == CCLERICAL_OP_LT ||
			     e->op.op == CCLERICAL_OP_GT) &&
			    (arg_t & (1U << CCLERICAL_TYPE_BOOL))) {
				cclerical_error(locp, p, NULL,
						"comparison with Boolean type");
				return 0;
			}
			expr_t = CCLERICAL_TYPE_BOOL;
		} else if (!unique_t(arg_t, &expr_t)) {
			cclerical_error(locp, p, NULL,
			                "mixed-type op expression");
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
			cclerical_error(locp, p, NULL,
			                "mixed-type case expression");
			return 0;
		}
		break;
	}
	case CCLERICAL_EXPR_CNST:
		expr_t = e->cnst.lower_type;
		break;
	case CCLERICAL_EXPR_DECL_ASGN:
		expr_t = e->decl_asgn.expr->result_type;
		break;
	case CCLERICAL_EXPR_VAR: {
		const struct cclerical_decl *v = p->decls.data[e->var];
		expr_t = v->var.type;
		break;
	}
	case CCLERICAL_EXPR_FUN_CALL: {
		const struct cclerical_decl *f = p->decls.data[e->fun_call.fun];
		expr_t = cclerical_prog_type(f->fun.body);
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
		cclerical_error(locp, p, NULL,
		                "no common type in expr-super-types 0x%x and "
		                "forced types 0x%x\n", convertible_to, forced);
		return NULL;
	}
	if (!max_sub_type(common, &e->result_type)) {
		cclerical_error(locp, p, NULL,
		                "no common sub-type in 0x%x for complete expr",
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
		cclerical_error(locp, p, NULL,
		                "call to undeclared function identifier '%s'",
		                id);
		return NULL;
	}
	struct cclerical_decl *d = p->decls.data[v];
	if (d->type != CCLERICAL_DECL_FUN) {
		cclerical_error(locp, p, NULL,
		                "'%s' does not identify a function", d->id);
		return NULL;
	}
	if (d->fun.arguments.valid != params.valid) {
		cclerical_error(locp, p, NULL,
		                "in function-call to %s: number of arguments "
		                "mismatch: passed: %zu, declared with: %zu",
		                d->id, params.valid, d->fun.arguments.valid);
		return NULL;
	}
	for (size_t i=0; i<params.valid; i++) {
		struct cclerical_expr *e = params.data[i];
		cclerical_id_t param = (uintptr_t)d->fun.arguments.data[i];
		struct cclerical_decl *dparam = p->decls.data[param];
		if ((1U << dparam->var.type) & super_types(e->result_type))
			continue;
		cclerical_error(locp, p, NULL,
		                "in function-call to %s: type mismatch of "
		                "argument %zu: exprected %s, expression is of "
		                "type %s", d->id,
		                CCLERICAL_TYPE_STR[dparam->var.type],
		                CCLERICAL_TYPE_STR[e->result_type]);
		return NULL;
	}
	struct cclerical_expr *e = cclerical_expr_create(CCLERICAL_EXPR_FUN_CALL);
	e->fun_call.fun = v;
	e->fun_call.params = params;
	return e;
}
