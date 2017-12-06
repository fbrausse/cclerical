
%{
#include <limits.h>	/* LONG_MAX */
#include <stdarg.h>	/* va_*() */
#include <stdio.h>	/* FILE */

#include "clerical.h"
#include "clerical.tab.h"

#define YYMAXDEPTH LONG_MAX
#define YYLTYPE_IS_TRIVIAL 1

static inline void clerical_error(YYLTYPE *locp, struct clerical_parser *p,
                                  const char *fmt, ...)
{
	fprintf(stderr, "error at %d-%d:%d-%d: ",
	        locp->first_line, locp->last_line,
	        locp->first_column, locp->last_column);
	va_list ap;
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
}

int clerical_lex(YYSTYPE *lvalp, YYLTYPE *llocp);

%}

%parse-param {struct clerical_parser *p}

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

%token TK_THEN		"then"
%token TK_ELSE		"else"
%token TK_CASE		"case"

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
%type <expr> expr
%type <type> type
%type <cases> cases
%type <varref> lim_init

%destructor { free($$); } IDENT
%destructor { free($$.str); } CONSTANT
%destructor { clerical_expr_destroy($$); } expr
%destructor { clerical_stmt_destroy($$); } stmt
%destructor { clerical_prog_destroy($$); } prog
%destructor { clerical_cases_fini(&$$); } cases

%start prog

%%

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
  : IDENT ":=" expr
    {
	$$ = clerical_stmt_create(CLERICAL_STMT_ASGN);
	if (!clerical_parser_var_lookup(p, $1, &$$->asgn.var)) {
		clerical_error(&yylloc, p, "variable '%s' not defined\n", $1);
		free($1);
		free($$);
		YYERROR;
	} else
		free($1);
	$$->asgn.expr = $3;
    }
  | "skip" { $$ = clerical_stmt_create(CLERICAL_STMT_SKIP); }
  | "if" expr TK_THEN prog TK_ELSE prog
    {
	$$ = clerical_stmt_create(CLERICAL_STMT_IF);
	$$->branch.cond = $2;
	$$->branch.if_true = $4;
	$$->branch.if_false = $6;
    }

expr
  : expr '+' expr  { $$ = clerical_expr_create_op(CLERICAL_OP_PLUS, $1, $3); }
  | expr '-' expr  { $$ = clerical_expr_create_op(CLERICAL_OP_MINUS, $1, $3); }
  | expr '*' expr  { $$ = clerical_expr_create_op(CLERICAL_OP_MUL, $1, $3); }
  | expr '/' expr  { $$ = clerical_expr_create_op(CLERICAL_OP_DIV, $1, $3); }
  | expr '^' expr  { $$ = clerical_expr_create_op(CLERICAL_OP_EXP, $1, $3); }
  | expr '<' expr  { $$ = clerical_expr_create_op(CLERICAL_OP_LT, $1, $3); }
  | expr '>' expr  { $$ = clerical_expr_create_op(CLERICAL_OP_GT, $1, $3); }
  | expr "/=" expr { $$ = clerical_expr_create_op(CLERICAL_OP_NE, $1, $3); }
  | '-' expr %prec TK_NEG
    {
	$$ = clerical_expr_create_op(CLERICAL_OP_UMINUS, $2, NULL);
    }
  | '(' expr ')'  { $$ = $2; }
  | "case" cases
    {
	$$ = clerical_expr_create(CLERICAL_EXPR_CASE);
	$$->cases = $2;
    }
  | lim_init expr
    {
	$$ = clerical_expr_create(CLERICAL_EXPR_LIM);
	$$->lim.seq_idx = $1;
	$$->lim.seq = $2;
	$$->lim.local = clerical_parser_close_scope(p);
    }
  | "var" IDENT ":=" expr ':' type "in" prog
    {
	$$ = clerical_expr_create(CLERICAL_EXPR_DECL_ASGN);
	int r = clerical_parser_new_var(p, $2, $6, &$$->decl_asgn.var);
	if (r) {
		clerical_expr_destroy($$);
		clerical_error(&yylloc, p, "error defining variable '%s': %s\n", $2, strerror(r));
		free($2);
		YYERROR;
	}
	$$->decl_asgn.expr = $4;
	$$->decl_asgn.prog = $8;
    }
  | IDENT
    {
	$$ = clerical_expr_create(CLERICAL_EXPR_VAR);
	if (!clerical_parser_var_lookup(p, $1, &$$->var)) {
		clerical_error(&yylloc, p, "variable '%s' not defined\n", $1);
		free($1);
		free($$);
		YYERROR;
	} else
		free($1);
    }
  | CONSTANT
    {
	$$ = clerical_expr_create(CLERICAL_EXPR_CNST);
	$$->cnst = $1;
    }

lim_init
  : "lim" IDENT "=>"
    {
	clerical_parser_open_scope(p);
	int r = clerical_parser_new_var(p, $2, CLERICAL_TYPE_INT, &$$);
	if (r) {
		clerical_error(&yylloc, p,
		               "error declaring variable '%s' in lim: %s\n",
		               strerror(r));
		free($2);
		YYERROR;
	}
    }

cases
  : expr "=>" prog
    {
	memset(&$$, 0, sizeof($$));
	clerical_vector_add(&$$, $1);
	clerical_vector_add(&$$, $3);
    }
  | cases "||" expr "=>" prog
    {
	$$ = $1;
	clerical_vector_add(&$$, $3);
	clerical_vector_add(&$$, $5);
    }

type
  : "Bool" { $$ = CLERICAL_TYPE_BOOL; }
  | "Int"  { $$ = CLERICAL_TYPE_INT; }
  | "Real" { $$ = CLERICAL_TYPE_REAL; }

%%
