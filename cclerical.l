
%{
#include "cclerical.h"
#include "cclerical.tab.h"

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

#define YY_USER_ACTION update_loc(yylloc, yytext);
%}

%option reentrant
%option bison-bridge
%option bison-locations
%option noyywrap
%option prefix="cclerical_"
%option stack

IDENT		[a-z_][a-zA-Z0-9_]*'*
INT_DEC		[+-]?(0|[1-9][0-9]*)
INT_HEX		[+-]?0[xX][0-9a-fA-F]+
INT_BIN		[+-]?0[bB][0-1]+
INT_OCT		[+-]?0(0|[1-7][0-9]*)

WHITE		[ \t\f\v\r\n]

%%

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

:=		{ return TK_ASGN; }
=>		{ return TK_RARROW; }
\|\|		{ return TK_BARS; }

[+*/:;^()=<>-]	{ return yytext[0]; }

Unit		{ return TK_UNIT; }
Bool		{ return TK_BOOL; }
Int		{ return TK_INT; }
Real		{ return TK_REAL; }

{WHITE}+	;

{IDENT}		{ yylval->ident = strdup(yytext); return IDENT; }

{INT_DEC}	{
			yylval->cnst.lower_type = CCLERICAL_TYPE_INT;
			yylval->cnst.str        = strdup(yytext);
			yylval->cnst.base       = 10;
			return CONSTANT;
		}

{INT_HEX}	{
			yylval->cnst.lower_type = CCLERICAL_TYPE_INT;
			yylval->cnst.str        = strdup(yytext);
			yylval->cnst.base       = 16;
			return CONSTANT;
		}

{INT_BIN}	{
			yylval->cnst.lower_type = CCLERICAL_TYPE_INT;
			yylval->cnst.str        = strdup(yytext);
			yylval->cnst.base       = 2;
			return CONSTANT;
		}

{INT_OCT}	{
			yylval->cnst.lower_type = CCLERICAL_TYPE_INT;
			yylval->cnst.str        = strdup(yytext);
			yylval->cnst.base       = 8;
			return CONSTANT;
		}
