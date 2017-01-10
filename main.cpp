#include <iostream>
#include "codegen.h"
#include "node.h"

using namespace std;

extern int yyparse();
extern NBlock* programBlock;

int main(int argc, char **argv)
{
	yyparse();
    // see http://comments.gmane.org/gmane.comp.compilers.llvm.devel/33877
	InitializeNativeTarget();
	InitializeNativeTargetAsmPrinter();
	InitializeNativeTargetAsmParser();

	CodeGenContext context;
	context.generateCode(*programBlock);
	
	return 0;
}

