%{
	#include "node.h"
    #include <cstdio>
    #include <cstdlib>
	NBlock *programBlock; /* the top level root node of our final AST */

	extern int yylex();
	void yyerror(const char *s) { std::printf("Error: %s\n", s);std::exit(1); }
%}

/* Represents the many different ways we can access our data */
%union {
	Node *node;
	NBlock *block;
	NExpression *expr;
	NStatement *stmt;
	NIdentifier *ident;
	NVariableDeclaration *var_decl;
	std::vector<NVariableDeclaration*> *varvec;
	std::vector<NExpression*> *exprvec;
	std::string *string;
	int token;
}

/* Define our terminal symbols (tokens). This should
   match our tokens.l lex file. We also define the node type
   they represent.
 */
%token <string> IDENTIFIER INTEGER DOUBLE STRING
%token <token> CEQ CNE CLT CLE CGT CGE
%token <token> ASSIGN PLUSASN MINUSASN MULASN DIVASN
%token <token> IF ELSE WHILE
%token <token> LPAREN RPAREN LBRACE RBRACE COMMA DOT
%token <token> PLUS MINUS MUL DIV
%token <token> NOT
%token <token> RETURN EXTERN END
%token <token> TRUE FALSE

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above. Ex: when
   we call an ident (defined by union type ident) we are really
   calling an (NIdentifier*). It makes the compiler happy.
 */
%type <ident> ident
%type <expr> numeric expr 
%type <varvec> func_decl_args
%type <exprvec> call_args
%type <block> program stmts block
%type <stmt> stmt var_decl func_decl extern_decl conditional elseif
%type <token> comparison

/* Operator precedence for mathematical operators */
%left PLUS MINUS
%left MUL DIV

%start program

%%

program : stmts { programBlock = $1; }
		;
		
stmts : stmt { $$ = new NBlock(); $$->statements.push_back($<stmt>1); }
	  | stmts stmt { $1->statements.push_back($<stmt>2); }
	  ;

stmt : var_decl END | func_decl | conditional | extern_decl END
	 | expr END { $$ = new NExpressionStatement(*$1); }
	 | RETURN expr END { $$ = new NReturnStatement(*$2); }
     ;

block : LBRACE stmts RBRACE { $$ = $2; }
	  | LBRACE RBRACE { $$ = new NBlock(); }
	  ;

conditional : IF LPAREN expr RPAREN block { $$ = new NConditional(*$3, *$5, *(new NBlock())); }
			| IF LPAREN expr RPAREN block ELSE block { $$ = new NConditional(*$3, *$5, *$7); }
			| IF LPAREN expr RPAREN block elseif { 
				NBlock* n = new NBlock();
				n->statements.push_back($6);
				$$ = new NConditional(*$3, *$5, *n);
			 }
			| WHILE LPAREN expr RPAREN block { $$ = new NLoop(*$3, *$5); } 
			;

elseif : ELSE IF LPAREN expr RPAREN block { $$ = new NConditional(*$4, *$6, *(new NBlock())); }
	   | ELSE IF LPAREN expr RPAREN block ELSE block { $$ = new NConditional(*$4, *$6, *$8); }
	   | ELSE IF LPAREN expr RPAREN block elseif {
			NBlock* n = new NBlock();
			n->statements.push_back($7);
			$$ = new NConditional(*$4, *$6, *n);
		}
	   ;

var_decl : ident ident { $$ = new NVariableDeclaration(*$1, *$2); }
		 | ident ident ASSIGN expr { $$ = new NVariableDeclaration(*$1, *$2, $4); }
		 ;

extern_decl : EXTERN ident ident LPAREN func_decl_args RPAREN
                { $$ = new NExternDeclaration(*$2, *$3, *$5); delete $5; }
            ;

func_decl : ident ident LPAREN func_decl_args RPAREN block 
			{ $$ = new NFunctionDeclaration(*$1, *$2, *$4, *$6); delete $4; }
		  ;
	
func_decl_args : /*blank*/  { $$ = new VariableList(); }
		  | var_decl { $$ = new VariableList(); $$->push_back($<var_decl>1); }
		  | func_decl_args COMMA var_decl { $1->push_back($<var_decl>3); }
		  ;

ident : IDENTIFIER { $$ = new NIdentifier(*$1); delete $1; }
	  ;

numeric : INTEGER { $$ = new NInteger(atol($1->c_str())); delete $1; }
		| DOUBLE { $$ = new NDouble(atof($1->c_str())); delete $1; }
		;
	
expr : ident ASSIGN expr { $$ = new NAssignment(*$<ident>1, *$3); }
	 	| ident PLUSASN expr { $$ = new NAssignment(*$<ident>1, $2, *$3); }
	 	| ident MINUSASN expr { $$ = new NAssignment(*$<ident>1, $2, *$3); }
	 	| ident MULASN expr { $$ = new NAssignment(*$<ident>1, $2, *$3); }
	 	| ident DIVASN expr { $$ = new NAssignment(*$<ident>1, $2, *$3); }
	 | ident LPAREN call_args RPAREN { $$ = new NMethodCall(*$1, *$3); delete $3; }
	 | ident { $<ident>$ = $1; }
	 | STRING { $$ = new NString($1->c_str()); delete $1; }
	 | numeric
         | expr MUL expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
         | expr DIV expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
         | expr PLUS expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
         | expr MINUS expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
		 | MINUS expr { $$ = new NUnaryOperator($1, *$2); }
	 | NOT expr { $$ = new NUnaryOperator($1, *$2); }
	 | TRUE { $$ = new NBool(true); }
	 | FALSE { $$ = new NBool(false); }
 	 | expr comparison expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
     | LPAREN expr RPAREN { $$ = $2; }
	 ;
	
call_args : /*blank*/  { $$ = new ExpressionList(); }
		  | expr { $$ = new ExpressionList(); $$->push_back($1); }
		  | call_args COMMA expr  { $1->push_back($3); }
		  ;

comparison : CEQ | CNE | CLT | CLE | CGT | CGE;

%%
