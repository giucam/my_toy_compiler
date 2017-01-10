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
	std::vector<NAssignment *> *argvec;
	std::string *string;
	int token;
}

/* Define our terminal symbols (tokens). This should
   match our tokens.l lex file. We also define the node type
   they represent.
 */
%token <string> TIDENTIFIER TINTEGER TDOUBLE TSTRING
%token <token> TCEQ TCNE TCLT TCLE TCGT TCGE TEQUAL
%token <token> TLPAREN TRPAREN TLBRACE TRBRACE TCOMMA TDOT
%token <token> TPLUS TMINUS TMUL TDIV
%token <token> TRETURN TEXTERN TLET TCOLON TSEMICOLON
%token <token> TSTRUCT TFUNC

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above. Ex: when
   we call an ident (defined by union type ident) we are really
   calling an (NIdentifier*). It makes the compiler happy.
 */
%type <ident> ident
%type <expr> numeric expr string
%type <varvec> func_decl_args struct_decl_args
%type <argvec> call_args
%type <block> program stmts block
%type <stmt> stmt var_decl func_decl extern_decl struct_decl
%type <token> comparison

/* Operator precedence for mathematical operators */
%left TPLUS TMINUS
%left TMUL TDIV

%start program

%%

program : stmts { programBlock = $1; }
		;
		
stmts : stmt { $$ = new NBlock(); $$->statements.push_back($<stmt>1); }
	  | stmts stmt { $1->statements.push_back($<stmt>2); }
	  ;

stmt : var_decl | func_decl | extern_decl | struct_decl
     | ident TEQUAL expr { $$ = new NExpressionStatement(*(new NAssignment(*$<ident>1, *$3))); }
	 | expr { $$ = new NExpressionStatement(*$1); }
	 | TRETURN expr { $$ = new NReturnStatement(*$2); }
     ;

block : TLBRACE stmts TRBRACE { $$ = $2; }
	  | TLBRACE TRBRACE { $$ = new NBlock(); }
	  ;

var_decl : TLET ident TCOLON ident { $$ = new NVariableDeclaration($4, *$2); }
         | TLET ident TCOLON ident TEQUAL expr { $$ = new NVariableDeclaration($4, *$2, new NAssignment(*$2, *$6)); }
         | TLET ident TCOLON ident TEQUAL TLBRACE call_args TRBRACE { $$ = new NVariableDeclaration($4, *$2, *$7); }
         | TLET ident TEQUAL expr { $$ = new NVariableDeclaration(*$2, new NAssignment(*$2, *$4)); }
         ;

extern_decl : TEXTERN ident TLPAREN func_decl_args TRPAREN TCOLON ident
                { $$ = new NExternDeclaration(*$7, *$2, *$4); delete $4; }
            ;

func_decl : TFUNC ident TLPAREN func_decl_args TRPAREN TCOLON ident block
			{ $$ = new NFunctionDeclaration(*$7, *$2, *$4, *$8); delete $4; }
		  ;
	
func_decl_args : /*blank*/  { $$ = new VariableList(); }
		  | ident TCOLON ident { $$ = new VariableList(); $$->push_back(new NVariableDeclaration($3, *$1)); }
		  | func_decl_args TCOMMA ident TCOLON ident { $1->push_back(new NVariableDeclaration($5, *$3)); }
		  ;

struct_decl : TSTRUCT ident TLBRACE struct_decl_args TRBRACE { $$ = new NStructDeclaration(*$2, *$4); }
            ;

struct_decl_args : /*blank*/ { $$ = new VariableList(); }
                 | ident TCOLON ident TSEMICOLON { $$ = new VariableList(); $$->push_back(new NVariableDeclaration($3, *$1)); }
                 | struct_decl_args ident TCOLON ident TSEMICOLON { $1->push_back(new NVariableDeclaration($4, *$2)); }
                 ;

ident : TIDENTIFIER { $$ = new NIdentifier(*$1); delete $1; }
      | ident TDOT TIDENTIFIER { $$ = new NIdentifier($<ident>1, *$3); delete $3; }
      ;

numeric : TINTEGER { $$ = new NInteger(atol($1->c_str())); delete $1; }
		| TDOUBLE { $$ = new NDouble(atof($1->c_str())); delete $1; }
		;

string : TSTRING { $$ = new NString($1); };

expr : ident TLPAREN call_args TRPAREN { $$ = new NMethodCall(*$1, *$3); delete $3; }
     | ident { $<ident>$ = $1; }
     | numeric
     | string
     | expr TMUL expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
     | expr TDIV expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
     | expr TPLUS expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
     | expr TMINUS expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
     | expr comparison expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
     | TLPAREN expr TRPAREN { $$ = $2; }
     ;
	
call_args : /*blank*/  { $$ = new AssignmentList(); }
          | ident TEQUAL expr { $$ = new AssignmentList(); $$->push_back(new NAssignment(*$<ident>1, *$3)); }
          | call_args TCOMMA ident TEQUAL expr  { $1->push_back(new NAssignment(*$<ident>3, *$5)); }
          ;

comparison : TCEQ | TCNE | TCLT | TCLE | TCGT | TCGE;

%%
