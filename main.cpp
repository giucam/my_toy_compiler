#include <iostream>
#include <unistd.h>

#include <llvm/Support/TargetRegistry.h>

#include "codegen.h"
#include "node.h"
#include "parser.h"
#include "common.h"
#include "cparser.h"

using namespace std;

struct Target
{
    std::string triple;
    llvm::DataLayout dataLayout;
};

static Target initLLVMTarget()
{
    llvm::InitializeNativeTarget();

    auto triple = llvm::sys::getDefaultTargetTriple();
    std::string errorMsg;
    auto target = llvm::TargetRegistry::lookupTarget(triple, errorMsg);
    if (!target) {
        error("{}", errorMsg);
    }

    auto cpu = "generic";
    auto features = "";

    llvm::TargetOptions opt;
    auto rm = llvm::Optional<llvm::Reloc::Model>();
    auto targetMachine = target->createTargetMachine(triple, cpu, features, opt, rm);
    auto datalayout = targetMachine->createDataLayout();

    return { triple, datalayout };
}

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

static void parseCImports(std::vector<Import> &imports, NBlock *block)
{
    char name[32];
    strcpy(name, "/tmp/XXXXXX.c");
    auto fd = mkstemps(name, 2);

    for (auto &&i: imports) {
        if (i.lang == Import::Lang::C) {
            fmt::print("INCLUDING {}\n", i.file);
            write(fd, "#include <", 10);
            write(fd, i.file.c_str(), i.file.size());
            write(fd, ">\n", 2);
        }
    }

    char buf[256];
    auto s = readlink((std::string("/proc/self/fd/") + std::to_string(fd)).c_str(), buf, 256);
    buf[s] = '\0';

    CParser parser(buf);
    parser.parse(block);

    close(fd);
    unlink(buf);
}

static void parseNativeImports(std::vector<Import> &imports, NBlock *block)
{
    for (auto &&i: imports) {
        if (i.lang == Import::Lang::Native) {
            Parser p(i.file);
            p.parse(block, true);
        }
    }
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
            checkOption(argv, i, argc, "-l", clangargs) ||
            checkOption(argv, i, argc, "-L", clangargs) ||
            checkOption(argv, i, argc, "-O", clangargs)) {
        } else if (strcmp(argv[i], "-shared") == 0) {
            clangargs.push_back("-shared");
        } else {
            inputfiles.push_back(argv[i]);
        }
    }

    if (inputfiles.empty()) {
        usage();
    }

    auto target = initLLVMTarget();

    for (auto &&file: inputfiles) {
        NBlock programBlock;

        Parser parser(file);

        std::vector<Import> imports;
        parser.parseImports(imports);

        parseCImports(imports, &programBlock);
        parseNativeImports(imports, &programBlock);

        parser.parse(&programBlock, false);

        CodeGenContext context(file);
        context.module().setTargetTriple(target.triple);
        context.module().setDataLayout(target.dataLayout);
        context.generateCode(programBlock);

        file += ".ll";
        context.writeOutput(file);
        clangargs.push_back(strdup(file.c_str()));
    }
    clangargs.push_back("baselib.cpp");
    clangargs.push_back(nullptr);

    execvp("clang", const_cast<char *const *>(clangargs.data()));

    return 0;
}

