
%{
#include <limits.h>	/* LONG_MAX */
#include <stdarg.h>	/* va_*() */
#include <stdio.h>	/* FILE */

#include "cclerical.h"
#include "cclerical.tab.h"
#include "cclerical.lex.h"

#define YYMAXDEPTH LONG_MAX
#define YYLTYPE_IS_TRIVIAL 1

static void cclerical_error0(YYLTYPE *locp, struct cclerical_parser *p,
                             void *scanner, const char *file, int lineno,
                             const char *fmt, ...);

#define cclerical_error(locp,p,scanner,...) \
	cclerical_error0(locp, p, scanner, __FILE__, __LINE__, __VA_ARGS__)

//int cclerical_lex(YYSTYPE *lvalp, YYLTYPE *llocp);

static int lookup_var(struct cclerical_parser *p, char *id,
                      cclerical_var_t *v, YYLTYPE *locp, int rw);

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
	cclerical_var_t varref;
	char *ident;
	struct cclerical_vector cases;
	struct cclerical_constant cnst;
	struct cclerical_vector params;
	struct cclerical_fun *fun;
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
%type <varref> lim_init fun_decl_param
%type <params> fun_decl_params fun_decl_params_spec
%type <fun> fun_decl
%type <ident> fun_decl_init

%start tu

%%

tu
  : %empty
  | tu toplevel

toplevel
  : fun_decl { cclerical_vector_add(&p->funs, $1); }
  | TK_DO prog { p->prog = $2; }

fun_decl
  : fun_decl_init '(' fun_decl_params_spec ')' ':' prog
    {
	$$ = cclerical_fun_create($1, &$3, $6);
	cclerical_parser_close_scope(p);
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
	cclerical_var_t v;
	if (!lookup_var(p, $1, &v, &yylloc, 1))
		YYERROR;
	$$ = cclerical_stmt_create(CCLERICAL_STMT_ASGN);
	$$->asgn.var = v;
	$$->asgn.expr = $3;
    }
  | TK_SKIP { $$ = cclerical_stmt_create(CCLERICAL_STMT_SKIP); }
  | TK_IF expr TK_THEN prog TK_ELSE prog TK_END
    {
	$$ = cclerical_stmt_create(CCLERICAL_STMT_IF);
	$$->branch.cond = $2;
	$$->branch.if_true = $4;
	$$->branch.if_false = $6;
    }
  | TK_WHILE expr TK_DO prog TK_END
    {
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
	EXPR($$, 1U << $$->result_type);
	free(cclerical_parser_close_scope(p).var_idcs.data);
    }
  | IDENT
    {
	cclerical_var_t v;
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

var_init
  : TK_VAR IDENT TK_ASGN expr ':' type
    {
	cclerical_parser_open_scope(p);
	cclerical_var_t v;
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
	$$->result_type = $6;
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

static inline void cclerical_error0(YYLTYPE *locp, struct cclerical_parser *p,
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
                      cclerical_var_t *v, YYLTYPE *locp, int rw)
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

static struct cclerical_expr * expr(struct cclerical_parser *p,
                                    struct cclerical_expr *e,
                                    cclerical_type_set_t forced,
                                    YYLTYPE *locp)
{
	enum cclerical_type expr_t;
	switch (e->type) {
	case CCLERICAL_EXPR_OP: {
		cclerical_type_set_t arg_t = 1U << e->op.arg1->result_type;
		if (!is_binary_op(e->op.op))
			arg_t |= 1U << e->op.arg2->result_type;
		if (arg_t & (1U << CCLERICAL_TYPE_UNIT)) {
			cclerical_error(locp, p, NULL, "operand of Unit type");
			return NULL;
		}
		if (!is_arith_op(e->op.op)) {
			if ((e->op.op == CCLERICAL_OP_LT ||
			     e->op.op == CCLERICAL_OP_GT) &&
			    (arg_t & (1U << CCLERICAL_TYPE_BOOL))) {
				cclerical_error(locp, p, NULL,
						"comparison with Boolean type");
				return NULL;
			}
			expr_t = CCLERICAL_TYPE_BOOL;
		} else if (!unique_t(arg_t, &expr_t)) {
			cclerical_error(locp, p, NULL,
			                "mixed-type op expression");
			return NULL;
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
			return NULL;
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
		const struct cclerical_var *v = p->vars.data[e->var];
		expr_t = v->type;
		break;
	default:
		abort();
	}
	}
	cclerical_type_set_t convertible_to = super_types(expr_t);
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
