%{
#include <string>
#include "node.h"
#include "parser.hpp"

#define SAVE_TOKEN  yylval.string = new std::string(yytext, yyleng)
#define TOKEN(t)    (yylval.token = t)
#define BUGOUT      printf("[DEBUG]: %s\n", yytext)
%}

%option noyywrap
%s comment
%s multicomment

%%

\"(\\.|[^"\\])*\"               SAVE_TOKEN; return STRING;
\'(\\.|[^'\\])*\'               SAVE_TOKEN; return STRING;

"//"                            BEGIN(comment);
<comment>\n                     BEGIN(INITIAL);
<comment><<EOF>>                BEGIN(INITIAL);
<comment>.*                     ;

"/*"                            BEGIN(multicomment);
<multicomment>[^*\n]*           ;
<multicomment>"*"+[^*/\n]*      ;
<multicomment>"*"+"/"           BEGIN(INITIAL);

[ \t\n]					        ;
";"                             return TOKEN(END);

"true"                          return TOKEN(TRUE);
"false"                         return TOKEN(FALSE);
"extern"                        return TOKEN(EXTERN);
"return"				        return TOKEN(RETURN);
"if"                            return TOKEN(IF);
"else"                          return TOKEN(ELSE);
"while"                         return TOKEN(WHILE);

[a-zA-Z_][a-zA-Z0-9_]*          SAVE_TOKEN; return IDENTIFIER;
[0-9]+\.[0-9]* 			        SAVE_TOKEN; return DOUBLE;
[0-9]+					        SAVE_TOKEN; return INTEGER;

"="						        return TOKEN(ASSIGN);
"+="                            return TOKEN(PLUSASN);
"-="                            return TOKEN(MINUSASN);
"*="                            return TOKEN(MULASN);
"/="                            return TOKEN(DIVASN);

"=="				          	return TOKEN(CEQ);
"!="			          		return TOKEN(CNE);
"<"				          		return TOKEN(CLT);
"<="	          				return TOKEN(CLE);
">"				          		return TOKEN(CGT);
">="					        return TOKEN(CGE);

"("	          					return TOKEN(LPAREN);
")"					          	return TOKEN(RPAREN);
"{"         				    return TOKEN(LBRACE);
"}"					          	return TOKEN(RBRACE);

"."         					return TOKEN(DOT);
","				          		return TOKEN(COMMA);

"+"				          		return TOKEN(PLUS);
"-"		          				return TOKEN(MINUS);
"*"		          				return TOKEN(MUL);
"/"				          		return TOKEN(DIV);

"!"                             return TOKEN(NOT);

.                               printf("Unknown token!\n"); yyterminate();

%%
