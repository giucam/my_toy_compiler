%{
#include <string>
#include "node.h"
#include "parser.hpp"

#define SAVE_TOKEN  yylval.string = new std::string(yytext, yyleng)
#define TOKEN(t)    (yylval.token = t)

static std::string strbuf;
%}

%x stringliteral
%option noyywrap

%%
\/\/.*  ;
[ \t\n]					        ;
"extern"                        return TOKEN(TEXTERN);
"return"				        return TOKEN(TRETURN);
"let"                      return TOKEN(TLET);
":"                        return TOKEN(TCOLON);
";"                         return TOKEN(TSEMICOLON);
"struct"                    return TOKEN(TSTRUCT);
"func"                      return TOKEN(TFUNC);
"iface"                     return TOKEN(TIFACE);
"impl"                      return TOKEN(TIMPL);
\"                           { BEGIN stringliteral; strbuf.clear(); }
<stringliteral>[^\\"\n]*     { strbuf += yytext; }
<stringliteral>\\n           { strbuf += '\n'; }
<stringliteral>\\t           { strbuf += '\t'; }
<stringliteral>\"            { yylval.string = new std::string(strbuf); BEGIN 0; return TSTRING; }


[a-zA-Z_][a-zA-Z0-9_]*  SAVE_TOKEN; return TIDENTIFIER;
"..."                       SAVE_TOKEN; return TIDENTIFIER;
[0-9]+					        SAVE_TOKEN; return TINTEGER;

"="						          return TOKEN(TEQUAL);
"=="				          	return TOKEN(TCEQ);
"!="			          		return TOKEN(TCNE);
"<"				          		return TOKEN(TCLT);
"<="	          				return TOKEN(TCLE);
">"				          		return TOKEN(TCGT);
">="					          return TOKEN(TCGE);

"("	          					return TOKEN(TLPAREN);
")"					          	return TOKEN(TRPAREN);
"{"         						return TOKEN(TLBRACE);
"}"					          	return TOKEN(TRBRACE);
"["                         return TOKEN(TLBRACKET);
"]"                         return TOKEN(TRBRACKET);

"."         						return TOKEN(TDOT);
","				          		return TOKEN(TCOMMA);

"+"				          		return TOKEN(TPLUS);
"-"		          				return TOKEN(TMINUS);
"*"		          				return TOKEN(TMUL);
"/"				          		return TOKEN(TDIV);

.                       printf("Unknown token!\n"); yyterminate();

%%
