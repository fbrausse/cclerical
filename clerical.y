
%{
#include <limits.h>	/* LONG_MAX */
#include <stdarg.h>	/* va_*() */
#include <stdio.h>	/* FILE */

#include "clerical.h"
#include "clerical.tab.h"
#include "clerical.lex.h"

#define YYMAXDEPTH LONG_MAX
#define YYLTYPE_IS_TRIVIAL 1

static void clerical_error(YYLTYPE *locp, struct clerical_parser *p,
                           void *scanner, const char *fmt, ...);
//int clerical_lex(YYSTYPE *lvalp, YYLTYPE *llocp);

static int lookup_var(struct clerical_parser *p, char *id,
                      clerical_var_t *v, YYLTYPE *locp, int rw);

static const unsigned TYPES_ALL = 1U << CLERICAL_TYPE_UNIT
                                | 1U << CLERICAL_TYPE_BOOL
                                | 1U << CLERICAL_TYPE_INT
                                | 1U << CLERICAL_TYPE_REAL;

static struct clerical_expr * expr(struct clerical_parser *p,
                                   struct clerical_expr *e,
                                   clerical_type_set_t forced,
                                   YYLTYPE *locp);
static struct clerical_expr * expr_new(struct clerical_parser *p,
                                       struct clerical_expr *e,
                                       YYLTYPE *locp);

#define EXPR_NEW(e)	do { if (!expr_new(p, e, &yylloc)) YYERROR; } while (0)
#define EXPR(e,forced)	\
	do { if (!expr(p, e, forced, &yylloc)) YYERROR; } while (0)

%}

%parse-param { struct clerical_parser *p } { void *yyscanner }
%lex-param { void *yyscanner }

%union {
	struct clerical_expr *expr;
	struct clerical_stmt *stmt;
	struct clerical_prog *prog;
	enum clerical_type type;
	clerical_var_t varref;
	char *ident;
	struct clerical_vector cases;
	struct clerical_constant cnst;
};

%locations
%define api.pure full
%define parse.error verbose
%define parse.lac full
%defines
%name-prefix "clerical_"
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

%precedence TK_THEN
%precedence TK_ELSE

%left  '<' '>' "/="
%left  '+' '-'
%left  '*' '/'
%precedence TK_NEG
%right '^'

%type <prog> prog
%type <stmt> stmt
%type <expr> expr var_init
%type <type> type
%type <cases> cases
%type <varref> lim_init

%start tu

%%

tu
  : prog { p->prog = $1; }

prog
  : prog ';' stmt
    {
	$$ = $1;
	clerical_vector_add(&$$->stmts, $3);
    }
  | stmt
    {
	$$ = clerical_prog_create();
	clerical_vector_add(&$$->stmts, $1);
    }

stmt
  : IDENT TK_ASGN expr
    {
	clerical_var_t v;
	if (!lookup_var(p, $1, &v, &yylloc, 1))
		YYERROR;
	$$ = clerical_stmt_create(CLERICAL_STMT_ASGN);
	$$->asgn.var = v;
	$$->asgn.expr = $3;
    }
  | TK_SKIP { $$ = clerical_stmt_create(CLERICAL_STMT_SKIP); }
  | TK_IF expr TK_THEN prog TK_ELSE prog
    {
	$$ = clerical_stmt_create(CLERICAL_STMT_IF);
	$$->branch.cond = $2;
	$$->branch.if_true = $4;
	$$->branch.if_false = $6;
    }
  | TK_WHILE expr prog
    {
	$$ = clerical_stmt_create(CLERICAL_STMT_WHILE);
	$$->loop.cond = $2;
	$$->loop.body = $3;
    }
  | expr
    {
	$$ = clerical_stmt_create(CLERICAL_STMT_EXPR);
	$$->expr = $1;
    }

expr
  : expr '+' expr { EXPR_NEW($$ = clerical_expr_create_op(CLERICAL_OP_PLUS, $1, $3)); }
  | expr '-' expr { EXPR_NEW($$ = clerical_expr_create_op(CLERICAL_OP_MINUS, $1, $3)); }
  | expr '*' expr { EXPR_NEW($$ = clerical_expr_create_op(CLERICAL_OP_MUL, $1, $3)); }
  | expr '/' expr { EXPR_NEW($$ = clerical_expr_create_op(CLERICAL_OP_DIV, $1, $3)); }
  | expr '^' expr { EXPR_NEW($$ = clerical_expr_create_op(CLERICAL_OP_EXP, $1, $3)); }
  | expr '<' expr { EXPR_NEW($$ = clerical_expr_create_op(CLERICAL_OP_LT, $1, $3)); }
  | expr '>' expr { EXPR_NEW($$ = clerical_expr_create_op(CLERICAL_OP_GT, $1, $3)); }
  | expr TK_NE expr { EXPR_NEW($$ = clerical_expr_create_op(CLERICAL_OP_NE, $1, $3)); }
  | '-' expr %prec TK_NEG
    {
	EXPR_NEW($$ = clerical_expr_create_op(CLERICAL_OP_UMINUS, $2, NULL));
    }
  | '(' expr ')'  { $$ = $2; }
  | TK_CASE cases
    {
	$$ = clerical_expr_create(CLERICAL_EXPR_CASE);
	$$->cases = $2;
	EXPR_NEW($$);
    }
  | lim_init prog
    {
	$$ = clerical_expr_create(CLERICAL_EXPR_LIM);
	$$->lim.seq_idx = $1;
	$$->lim.seq = $2;
	$$->lim.local = clerical_parser_close_scope(p);
	EXPR_NEW($$);
    }
  | var_init TK_IN prog
    {
	$$ = $1;
	$$->decl_asgn.prog = $3;
	EXPR($$, TYPES_ALL);
	free(clerical_parser_close_scope(p).var_idcs.data);
    }
  | IDENT
    {
	clerical_var_t v;
	if (!lookup_var(p, $1, &v, &yylloc, 0))
		YYERROR;
	$$ = clerical_expr_create(CLERICAL_EXPR_VAR);
	$$->var = v;
	EXPR_NEW($$);
    }
  | CONSTANT
    {
	$$ = clerical_expr_create(CLERICAL_EXPR_CNST);
	$$->cnst = $1;
	EXPR_NEW($$);
    }

var_init
  : TK_VAR IDENT TK_ASGN expr ':' type
    {
	clerical_parser_open_scope(p);
	clerical_var_t v;
	int r = clerical_parser_new_var(p, $2, $6, &v);
	if (r) {
		clerical_error(&yylloc, p, yyscanner,
		               "error defining variable '%s': %s\n", $2,
		               strerror(r));
		free($2);
		YYERROR;
	}
	$$ = clerical_expr_create(CLERICAL_EXPR_DECL_ASGN);
	$$->decl_asgn.var  = v;
	EXPR($$->decl_asgn.expr = $4, 1U << $6);
    }

lim_init
  : TK_LIM IDENT TK_RARROW
    {
	clerical_parser_open_scope(p);
	int r = clerical_parser_new_var(p, $2, CLERICAL_TYPE_INT, &$$);
	if (r) {
		clerical_error(&yylloc, p, yyscanner,
		               "error declaring variable '%s' in lim: %s\n",
		               strerror(r));
		free($2);
		YYERROR;
	}
    }

cases
  : expr TK_RARROW prog
    {
	memset(&$$, 0, sizeof($$));
	clerical_vector_add(&$$, $1);
	clerical_vector_add(&$$, $3);
    }
  | cases TK_BARS expr TK_RARROW prog
    {
	$$ = $1;
	clerical_vector_add(&$$, $3);
	clerical_vector_add(&$$, $5);
    }

type
  : TK_BOOL { $$ = CLERICAL_TYPE_BOOL; }
  | TK_INT  { $$ = CLERICAL_TYPE_INT; }
  | TK_REAL { $$ = CLERICAL_TYPE_REAL; }

%%

static inline void clerical_error(YYLTYPE *locp, struct clerical_parser *p,
                                  void *scanner, const char *fmt, ...)
{
	fprintf(stderr, "error at %d-%d:%d-%d: ",
	        locp->first_line, locp->last_line,
	        locp->first_column, locp->last_column);
	va_list ap;
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
}

static int lookup_var(struct clerical_parser *p, char *id,
                      clerical_var_t *v, YYLTYPE *locp, int rw)
{
	int r = clerical_parser_var_lookup(p, id, v, rw);
	if (!r) {
		clerical_error(locp, p, NULL,
		               "variable '%s' is %s in this context", id,
		               rw && clerical_parser_var_lookup(p, id, v, 0)
		               ? "read-only" : "not defined");
		free(id);
	}
	return r;
}

static int min_super_type(clerical_type_set_t t, enum clerical_type *res)
{
	if (!t)
		return 0;
	if (t & (1U << CLERICAL_TYPE_UNIT)) {
		if (~t & ~(1U << CLERICAL_TYPE_UNIT))
			return 0;
		*res = CLERICAL_TYPE_UNIT;
		return 1;
	}
	if (t & (1U << CLERICAL_TYPE_BOOL)) {
		if (~t & ~(1U << CLERICAL_TYPE_BOOL))
			return 0;
		*res = CLERICAL_TYPE_BOOL;
		return 1;
	}
	*res = (t & (1U << CLERICAL_TYPE_REAL)) ? CLERICAL_TYPE_REAL
	                                        : CLERICAL_TYPE_INT;
	return 1;
}

static clerical_type_set_t super_types(enum clerical_type t)
{
	clerical_type_set_t res = 1U << t;
	if (t == CLERICAL_TYPE_INT)
		res |= 1U << CLERICAL_TYPE_REAL;
	return res;
}

static struct clerical_expr * expr(struct clerical_parser *p,
                                   struct clerical_expr *e,
                                   clerical_type_set_t forced,
                                   YYLTYPE *locp)
{
	clerical_type_set_t types, allowed;
	clerical_expr_compute_types(&p->vars, e, &types, &allowed);
	enum clerical_type type_min;
	if (!min_super_type(types, &type_min)) {
		clerical_error(locp, p, NULL,
		               "no common super type in expr combined of types "
		               "0x%x", types);
		goto err;
	}
	if (!(allowed & forced)) {
		clerical_error(locp, p, NULL,
		               "expr-allowed types 0x%x don't intersect forced "
		               "types 0x%x", allowed, forced);
		goto err;
	}
	allowed &= forced;
	clerical_type_set_t castable_to = super_types(type_min);
	if (!(allowed & castable_to)) {
		clerical_error(locp, p, NULL,
		               "allowed types 0x%x don't intersect castable "
		               "types 0x%x of expr", allowed, castable_to);
		goto err;
	}
	allowed &= castable_to;
	if (!min_super_type(allowed, &e->result_type)) {
		clerical_error(locp, p, NULL,
		               "no common super type in 0x%x for complete expr",
		               allowed);
		goto err;
	}
	return e;
err:
	return NULL;
}

static struct clerical_expr * expr_new(struct clerical_parser *p,
                                       struct clerical_expr *e,
                                       YYLTYPE *locp)
{
	if (expr(p, e, TYPES_ALL, locp))
		return e;
	free(e);
	return NULL;
}
