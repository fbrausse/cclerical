/* SPDX short identifier: BSD-3-Clause */
%{
#include "cclerical.h"
#include "cclerical.tab.h"

static char * ccl_strdup(const char *s)
{
	return memdup(s, strlen(s)+1);
}

static void update_loc(YYLTYPE *loc, const char *s)
{
	loc->first_line = loc->last_line;
	loc->first_column = loc->last_column;
	for (; *s; s++)
		switch (*s) {
		case '\n':
			loc->last_line++;
			loc->last_column = 0;
			break;
		case '\t':
			loc->last_column = (loc->last_column + 8) & ~7U;
			break;
		default:
			loc->last_column++;
			break;
		}
}

static void cnst_bool(struct cclerical_constant *c, int v)
{
	c->lower_type = CCLERICAL_TYPE_BOOL;
	c->boolean = v;
}

static void cnst_num(struct cclerical_constant *c, const char *s, int base,
                     enum cclerical_type ltype)
{
	c->lower_type = ltype;
	c->numeric.str = ccl_strdup(s);
	c->numeric.base = base;
}

#define YY_USER_ACTION update_loc(yylloc, yytext);
%}

%option reentrant
%option bison-bridge
%option bison-locations
%option noyywrap
%option prefix="cclerical_"
%option stack

IDENT		[a-z_][a-zA-Z0-9_]*'*
INT_DEC		(0|[1-9][0-9]*)
INT_HEX		0[xX][0-9a-fA-F]+
INT_BIN		0[bB][0-1]+
INT_OCT		0(0|[1-7][0-9]*)

FRAC_DEC	((0|[1-9][0-9]*)\.|\.[0-9])[0-9]*([eE][+-]?[0-9]+)?
FRAC_HEX	0[xX]([0-9a-fA-F]+\.|\.[0-9a-fA-F])[0-9a-fA-F]*([eE][+-]?[0-9a-fA-F]+)?

WHITE		[ \t\f\v\r\n]

%x comment

%%

		size_t comment_depth;

<INITIAL>\(\*	{ BEGIN(comment); comment_depth = 1; }
<comment>\(\*	{ comment_depth++; }
<comment>\*\)	{ if (!--comment_depth) BEGIN(INITIAL); }
<comment>[^(*]|\([^*]|\*[^)]	;

if		{ return TK_IF; }
then		{ return TK_THEN; }
else		{ return TK_ELSE; }
case		{ return TK_CASE; }
skip		{ return TK_SKIP; }
lim		{ return TK_LIM; }
var		{ return TK_VAR; }
in		{ return TK_IN; }
while		{ return TK_WHILE; }
end		{ return TK_END; }
do		{ return TK_DO; }
function	{ return TK_FUN; }
external	{ return TK_EXTERNAL; }
and		{ return TK_AND; }

false		{
			cnst_bool(&yylval->cnst, 0);
			return CONSTANT;
		}

true		{
			cnst_bool(&yylval->cnst, 1);
			return CONSTANT;
		}

:=		{ return TK_ASGN; }
->              { return TK_RSARROW; }
=>		{ return TK_RDARROW; }
\|\|		{ return TK_BARS; }

[+*/:;,^()=<>&|!-]	{ return yytext[0]; }

Unit		{ return TK_UNIT; }
Bool		{ return TK_BOOL; }
Int		{ return TK_INT; }
Real		{ return TK_REAL; }

{WHITE}+	;

{IDENT}		{ yylval->ident = ccl_strdup(yytext); return IDENT; }

{FRAC_DEC}	{
			cnst_num(&yylval->cnst, yytext, 10, CCLERICAL_TYPE_REAL);
			return CONSTANT;
		}

{FRAC_HEX}	{
			cnst_num(&yylval->cnst, yytext, 16, CCLERICAL_TYPE_REAL);
			return CONSTANT;
		}

{INT_DEC}	{
			cnst_num(&yylval->cnst, yytext, 10, CCLERICAL_TYPE_INT);
			return CONSTANT;
		}

{INT_HEX}	{
			cnst_num(&yylval->cnst, yytext, 16, CCLERICAL_TYPE_INT);
			return CONSTANT;
		}

{INT_BIN}	{
			cnst_num(&yylval->cnst, yytext, 2, CCLERICAL_TYPE_INT);
			return CONSTANT;
		}

{INT_OCT}	{
			cnst_num(&yylval->cnst, yytext, 8, CCLERICAL_TYPE_INT);
			return CONSTANT;
		}
