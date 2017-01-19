#include <iostream>
#include <unistd.h>

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

static bool checkOption(char **argv, int &i, int argc, const char *option, std::vector<const char *> &clangargs)
{
    if (strcmp(argv[i], option) == 0) {
        if (i + 1 >= argc) {
            usage();
        }
        clangargs.push_back(argv[i]);
        clangargs.push_back(argv[++i]);
        return true;
    } else if (strncmp(argv[i], option, strlen(option)) == 0) {
        clangargs.push_back(argv[i]);
        return true;
    }
    return false;
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        usage();
    }

    std::vector<const char *> clangargs;
    clangargs.push_back("clang");

    std::vector<std::string> inputfiles;
    for (int i = 1; i < argc; ++i) {
        if (checkOption(argv, i, argc, "-o", clangargs) ||
            checkOption(argv, i, argc, "-l", clangargs)) {
            continue;
        }
        inputfiles.push_back(argv[i]);
    }

    for (auto &&file: inputfiles) {
        NBlock programBlock;

        if (access(file.c_str(), F_OK) != 0) {
            error("cannot open file '{}' for reading", file);
        }

        Parser parser(file);
        parser.parse(&programBlock, false);

        llvm::InitializeNativeTarget();
    //     InitializeNativeTargetAsmPrinter();
    //     InitializeNativeTargetAsmParser();

        CodeGenContext context;
        context.generateCode(programBlock);

        file += ".ll";
        context.writeOutput(file);
        clangargs.push_back(strdup(file.c_str()));
    }
    clangargs.push_back(nullptr);

    execvp("clang", const_cast<char *const *>(clangargs.data()));

    return 0;
}

