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

class NBlock;
class NIdentifier;
class NIfaceDeclaration;
class NIfacePrototype;
class NFunctionDeclaration;
class TypeName;

class CodeGenBlock {
public:
    llvm::BasicBlock *block;
    llvm::Function *function;
    CodeGenBlock *parent;
    std::unordered_map<std::string, llvm::Value *> locals;
    bool returned;
};

struct StructInfo {
    llvm::Type *type;
    std::vector<std::string> fields;
};

struct TupleInfo {
    llvm::Type *type;
};

class CodeGenContext {
public:
    CodeGenContext();

    bool isFunctionNameAvailable(const std::string &name) const;

    llvm::LLVMContext &context() { return m_context; }
    llvm::Module &module() { return *m_module.get(); }
    const llvm::Module &module() const { return *m_module.get(); }
    llvm::Value *findValue(const NIdentifier &ident);

    llvm::Type *typeOf(const TypeName &type);

    const StructInfo *structInfo(llvm::Type *type) const;
    StructInfo *newStructType(llvm::StructType *type);

    llvm::StructType *tupleType(const std::vector<llvm::Type *> &argTypes);
    llvm::Value *makeTupleValue(const std::vector<llvm::Value *> &values, const std::string &name);

    const TupleInfo *tupleInfo(llvm::Type *type) const;

    NIfacePrototype *ifacePrototype(const std::string &name) const;
    NIfaceDeclaration *interface(const std::string &name) const;
    void addInterface(NIfaceDeclaration *iface);

    llvm::Function *functionTemplate(const std::string &name, std::vector<llvm::Value *> &values);
    void addFunctionTemplate(NFunctionDeclaration *func);

    void generateCode(NBlock &root);

    void writeOutput(const std::string &filename);

    void storeLocal(const std::string &name, llvm::Value *value);
    llvm::Value *local(const std::string &name) const;

    const CodeGenBlock *currentBlock() const { return &m_blocks.top(); }
    CodeGenBlock *currentBlock() { return &m_blocks.top(); }
    void pushBlock(llvm::BasicBlock *block, llvm::Function *function, CodeGenBlock *parent);
    void popBlock();

private:
    llvm::Function *makeConcreteFunction(NFunctionDeclaration *func, std::vector<llvm::Value *> &values);

    std::stack<CodeGenBlock> m_blocks;
    llvm::Function *m_mainFunction;
    llvm::LLVMContext m_context;
    std::unique_ptr<llvm::Module> m_module;
    std::unordered_map<std::string, StructInfo> m_structInfo;
    std::unordered_map<llvm::Type *, StructInfo *> m_structInfoByType;
    std::unordered_map<std::string, llvm::StructType *> m_tupleTypes;
    std::unordered_map<llvm::Type *, TupleInfo> m_tupleInfo;
    std::unordered_map<std::string, NIfacePrototype *> m_ifacePrototypes;
    std::unordered_map<std::string, NIfaceDeclaration *> m_interfaces;
    std::unordered_map<std::string, NFunctionDeclaration *> m_functionTemplates;
    std::unordered_map<std::string, llvm::Function *> m_concreteTemplates;
};
