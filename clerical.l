
%{
#include "clerical.h"
#include "clerical.tab.h"
%}

%option reentrant
%option bison-bridge
%option bison-locations
%option noyywrap
%option prefix="clerical_"
%option stack
%option yylineno

IDENT		[a-zA-Z_][a-zA-Z0-9_]*
INT_DEC		[+-]?(0|[1-9][0-9]*)
INT_HEX		[+-]?0[xX][0-9a-fA-F]+
INT_BIN		[+-]?0[bB][0-1]+
INT_OCT		[+-]?0(0|[1-7][0-9]*)

%%

{IDENT}		{ yylval->ident = strdup(yytext); return IDENT; }

{INT_DEC}	{
			yylval->cnst.lower_type = CLERICAL_TYPE_INT;
			yylval->cnst.str        = strdup(yytext);
			yylval->cnst.base       = 10;
		}

{INT_HEX}	{
			yylval->cnst.lower_type = CLERICAL_TYPE_INT;
			yylval->cnst.str        = strdup(yytext);
			yylval->cnst.base       = 16;
		}

{INT_BIN}	{
			yylval->cnst.lower_type = CLERICAL_TYPE_INT;
			yylval->cnst.str        = strdup(yytext);
			yylval->cnst.base       = 2;
		}

{INT_OCT}	{
			yylval->cnst.lower_type = CLERICAL_TYPE_INT;
			yylval->cnst.str        = strdup(yytext);
			yylval->cnst.base       = 8;
		}
