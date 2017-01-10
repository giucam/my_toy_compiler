#include <stack>
#include <unordered_map>
#include <typeinfo>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/Support/raw_ostream.h>

using namespace llvm;

class NBlock;
class NIdentifier;
class NIfaceDeclaration;
class NIfacePrototype;

class CodeGenBlock {
public:
    BasicBlock *block;
    Value *returnValue;
    std::map<std::string, Value*> locals;
    NIdentifier *currentId;
};

struct Struct {
    Type *type;
    std::vector<std::string> fields;
};

struct FunctionData {
    std::vector<std::string> argumentNames;
    std::string varargsName;
};

struct Tuple {
    Type *type;
};

class CodeGenContext {
    std::stack<CodeGenBlock *> blocks;
    Function *mainFunction;

public:
    std::unordered_map<std::string, Struct> structs;
    std::unordered_map<Type *, Struct *> structsByType;
    std::unordered_map<Type *, Tuple> tuples;
    std::unordered_map<Function *, FunctionData> functions;

    std::unordered_map<std::string, NIfacePrototype *> ifacePrototypes;
    std::unordered_map<std::string, NIfaceDeclaration *> interfaces;

    Module *module;
    LLVMContext TheContext;
    CodeGenContext() { module = new Module("main", TheContext); }

    Value *findValue(const NIdentifier &ident, const NIdentifier *parent);

    void generateCode(NBlock& root);
    GenericValue runCode();
    std::map<std::string, Value*>& locals() { return blocks.top()->locals; }
    CodeGenBlock *currentBlock() { return blocks.top(); }
    void pushBlock(BasicBlock *block) { blocks.push(new CodeGenBlock()); blocks.top()->returnValue = NULL; blocks.top()->block = block; }
    void popBlock() { CodeGenBlock *top = blocks.top(); blocks.pop(); delete top; }
    void setCurrentReturnValue(Value *value) { blocks.top()->returnValue = value; }
    Value* getCurrentReturnValue() { return blocks.top()->returnValue; }
};
