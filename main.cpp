#include <iostream>

#include "codegen.h"
#include "node.h"
#include "parser.h"
#include "common.h"

using namespace std;

static void usage()
{
    fmt::print(stderr, "usage:\n");
    exit(1);
}

int main(int argc, char **argv)
{
    if (argc < 3) {
        usage();
    }

    NBlock programBlock;

    Parser parser(argv[1]);
    parser.parse(&programBlock);

    llvm::InitializeNativeTarget();
//     InitializeNativeTargetAsmPrinter();
//     InitializeNativeTargetAsmParser();

    CodeGenContext context;
    context.generateCode(programBlock);
    context.writeOutput(argv[2]);

    return 0;
}

