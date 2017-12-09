
%{
#include <limits.h>	/* LONG_MAX */
#include <stdarg.h>	/* va_*() */
#include <stdio.h>	/* FILE */

#include "clerical.h"
#include "clerical.tab.h"
#include "clerical.lex.h"

#define YYMAXDEPTH LONG_MAX
#define YYLTYPE_IS_TRIVIAL 1

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

//int clerical_lex(YYSTYPE *lvalp, YYLTYPE *llocp);

static int lookup_var(struct clerical_parser *p, char *id,
                      clerical_var_t *v, YYLTYPE *locp, int rw)
{
	int r = clerical_parser_var_lookup(p, id, v, rw);
	if (!r) {
		clerical_error(locp, p, NULL,
		               "error: variable '%s' is %s in this context\n",
		               id,
		               rw && !clerical_parser_var_lookup(p, id, v, 0)
		               ? "read-only" : "not defined");
		free(id);
	}
	return r;
}

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
  : expr '+' expr  { $$ = clerical_expr_create_op(CLERICAL_OP_PLUS, $1, $3); }
  | expr '-' expr  { $$ = clerical_expr_create_op(CLERICAL_OP_MINUS, $1, $3); }
  | expr '*' expr  { $$ = clerical_expr_create_op(CLERICAL_OP_MUL, $1, $3); }
  | expr '/' expr  { $$ = clerical_expr_create_op(CLERICAL_OP_DIV, $1, $3); }
  | expr '^' expr  { $$ = clerical_expr_create_op(CLERICAL_OP_EXP, $1, $3); }
  | expr '<' expr  { $$ = clerical_expr_create_op(CLERICAL_OP_LT, $1, $3); }
  | expr '>' expr  { $$ = clerical_expr_create_op(CLERICAL_OP_GT, $1, $3); }
  | expr TK_NE expr { $$ = clerical_expr_create_op(CLERICAL_OP_NE, $1, $3); }
  | '-' expr %prec TK_NEG
    {
	$$ = clerical_expr_create_op(CLERICAL_OP_UMINUS, $2, NULL);
    }
  | '(' expr ')'  { $$ = $2; }
  | TK_CASE cases
    {
	$$ = clerical_expr_create(CLERICAL_EXPR_CASE);
	$$->cases = $2;
    }
  | lim_init prog
    {
	$$ = clerical_expr_create(CLERICAL_EXPR_LIM);
	$$->lim.seq_idx = $1;
	$$->lim.seq = $2;
	$$->lim.local = clerical_parser_close_scope(p);
    }
  | var_init TK_IN prog
    {
	$$ = $1;
	$$->decl_asgn.prog = $3;
    }
  | IDENT
    {
	clerical_var_t v;
	if (!lookup_var(p, $1, &v, &yylloc, 0))
		YYERROR;
	$$ = clerical_expr_create(CLERICAL_EXPR_VAR);
	$$->var = v;
    }
  | CONSTANT
    {
	$$ = clerical_expr_create(CLERICAL_EXPR_CNST);
	$$->cnst = $1;
    }

var_init
  : TK_VAR IDENT TK_ASGN expr ':' type
    {
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
	$$->decl_asgn.expr = $4;
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
