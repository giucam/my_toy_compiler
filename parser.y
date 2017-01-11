%{
	#include "node.h"
        #include <cstdio>
        #include <cstdlib>
	NBlock *programBlock; /* the top level root node of our final AST */

	extern int yylex();
	void yyerror(const char *s) { std::printf("Error: %s\n", s);std::exit(1); }
%}

%error-verbose
/* %glr-parser */

/* Represents the many different ways we can access our data */
%union {
	Node *node;
	NBlock *block;
	NExpression *expr;
	NStatement *stmt;
	NIdentifier *ident;
	NTuple *tuple;
	NVariableDeclaration *var_decl;
	NFunctionDeclaration *func_decl;
	NIfacePrototype *ifaceProto;
	std::vector<NVariableDeclaration*> *varvec;
	std::vector<NExpression*> *exprvec;
	std::vector<NAssignment *> *argvec;
	std::vector<NIfaceParameter *> *parameterVec;
	std::vector<NIfacePrototype *> *protoVec;
	std::vector<NFunctionDeclaration *> *funclist;
	std::string *string;
	int token;
}

/* Define our terminal symbols (tokens). This should
   match our tokens.l lex file. We also define the node type
   they represent.
 */
%token <string> TIDENTIFIER TINTEGER TDOUBLE TSTRING
%token <token> TCEQ TCNE TCLT TCLE TCGT TCGE TEQUAL
%token <token> TLPAREN TRPAREN TLBRACE TRBRACE TCOMMA TDOT TELLIPSIS TLBRACKET TRBRACKET
%token <token> TPLUS TMINUS TMUL TDIV
%token <token> TRETURN TEXTERN TLET TCOLON TSEMICOLON
%token <token> TSTRUCT TFUNC TIFACE TIMPL

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above. Ex: when
   we call an ident (defined by union type ident) we are really
   calling an (NIdentifier*). It makes the compiler happy.
 */
%type <ident> ident
%type <expr> numeric expr string tuple
%type <varvec> func_decl_args struct_decl_args
%type <argvec> init_args
%type <exprvec> call_args
%type <parameterVec> iface_parameters
%type <protoVec> iface_prototypes
%type <funclist> func_declarations
%type <block> program stmts block
%type <stmt> stmt var_decl extern_decl struct_decl iface_decl impl_decl
%type <token> comparison
%type <ifaceProto> iface_prototype
%type <func_decl> func_decl
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

stmt : var_decl TSEMICOLON | func_decl | extern_decl | struct_decl | iface_decl | impl_decl
     | ident TEQUAL expr TSEMICOLON { $$ = new NExpressionStatement(*(new NAssignment(*$<ident>1, *$3))); }
     | expr TSEMICOLON { $$ = new NExpressionStatement(*$1); }
     | TRETURN expr TSEMICOLON { $$ = new NReturnStatement(*$2); }
     ;

block : TLBRACE stmts TRBRACE { $$ = $2; }
	  | TLBRACE TRBRACE { $$ = new NBlock(); }
	  ;

var_decl : TLET ident TCOLON ident { $$ = new NVariableDeclaration($4, *$2); }
         | TLET ident TCOLON ident TEQUAL expr { $$ = new NVariableDeclaration($4, *$2, new NAssignment(*$2, *$6)); }
         | TLET ident TCOLON ident TEQUAL TLBRACE init_args TRBRACE { $$ = new NVariableDeclaration($4, *$2, *$7); }
         | TLET ident TEQUAL expr { $$ = new NVariableDeclaration(*$2, new NAssignment(*$2, *$4)); }
         ;

extern_decl : TEXTERN ident TLPAREN func_decl_args TRPAREN TCOLON ident { $$ = new NExternDeclaration(*$7, *$2, *$4, false); delete $4; }
/*             | TEXTERN ident TLPAREN func_decl_args TCOMMA TELLIPSIS TRPAREN TCOLON ident { $$ = new NExternDeclaration(*$9, *$2, *$4, true); delete $4; } */
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

iface_decl : TIFACE TIDENTIFIER TLPAREN iface_parameters TRPAREN TLBRACE iface_prototypes TRBRACE { $$ = new NIfaceDeclaration(*$2, $4, $7); delete $2; delete $4; delete $7; }
           ;

iface_parameters : TIDENTIFIER { $$ = new NIfaceParameterList(); $$->push_back(new NIfaceParameter(*$1)); delete $1; }
                 | iface_parameters TCOMMA TIDENTIFIER { $$->push_back(new NIfaceParameter(*$3)); delete $3; }
                 ;

iface_prototype : TFUNC TIDENTIFIER TLPAREN iface_parameters TRPAREN TSEMICOLON { $$ = new NIfacePrototype(*$2, $4); delete $2; }
                ;

iface_prototypes : iface_prototype { $$ = new NIfacePrototypeList(); $$->push_back($1); }
                 | iface_prototypes iface_prototype { $$->push_back($2); }
                 ;

impl_decl : TIMPL TIDENTIFIER TLPAREN iface_parameters TRPAREN TLBRACE func_declarations TRBRACE { $$ = new NImplDeclaration(*$2, $4, $7); delete $2; delete $4; }
          ;

func_declarations : func_decl { $$ = new FuncDeclarationList(); $$->push_back($1); }
                  | func_declarations func_decl { $$->push_back($2); }
                  ;

numeric : TINTEGER { $$ = new NInteger(atol($1->c_str())); delete $1; }
        | TINTEGER TDOT TINTEGER { $$ = new NDouble(std::stof(*$1 + "." + *$3)); delete $1; delete $3;}
        ;

string : TSTRING { $$ = new NString($1); };

tuple: TLBRACKET { $$ = new NTuple(); }
     | tuple expr TCOMMA { static_cast<NTuple *>($$)->add($2); }
     | tuple expr TRBRACKET { static_cast<NTuple *>($$)->add($2); }
     ;

expr : TIDENTIFIER TLPAREN call_args TRPAREN { $$ = new NMethodCall(*$1, *$3); delete $3; }
     | ident { $<ident>$ = $1; }
     | numeric
     | string
     | tuple
     | expr TMUL expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
     | expr TDIV expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
     | expr TPLUS expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
     | expr TMINUS expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
     | expr comparison expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
/*      | expr TDOT TIDENTIFIER { $$ = new NIdentifier($1, *$3); } */
/*      | expr TDOT TINTEGER { $$ = new NIdentifier($1, std::stoi(*$3)); } */
     | TINTEGER TDOT TIDENTIFIER TLPAREN call_args TRPAREN { $5->insert($5->begin(), new NInteger(std::stol(*$1))); $$ = new NMethodCall(*$3, *$5); delete $1; delete $3; }
     | expr TDOT TIDENTIFIER TLPAREN call_args TRPAREN { $5->insert($5->begin(), $1); $$ = new NMethodCall(*$3, *$5); delete $3; }
     | TLPAREN expr TRPAREN { $$ = $2; }
     ;

ident : TIDENTIFIER { $$ = new NIdentifier(*$1); delete $1; }
      | ident TDOT TIDENTIFIER { $$ = new NIdentifier($<ident>1, *$3); delete $3; }
/*       | ident TDOT TINTEGER { $$ = new NIdentifier($<ident>1, std::stoi(*$3)); delete $3; } */
      ;

init_args : /*blank*/  { $$ = new AssignmentList(); }
          | ident TEQUAL expr { $$ = new AssignmentList(); $$->push_back(new NAssignment(*$<ident>1, *$3)); }
          | init_args TCOMMA ident TEQUAL expr  { $1->push_back(new NAssignment(*$<ident>3, *$5)); }
          ;

call_args : /*blank*/  { $$ = new ExpressionList(); }
          | expr { $$ = new ExpressionList(); $$->push_back($1); }
          | call_args TCOMMA expr  { $1->push_back($3); }
          ;


comparison : TCEQ | TCNE | TCLT | TCLE | TCGT | TCGE;

%%
