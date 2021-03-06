
#ifndef CODEGEN_H
#define CODEGEN_H

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
#include "llvm/IR/DIBuilder.h"
#include <llvm/Support/TargetSelect.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/Support/raw_ostream.h>

#include "value.h"
#include "common.h"
#include "stage.h"

class NBlock;
class NIdentifier;
class NIfaceDeclaration;
class NIfacePrototype;
class NFunctionDeclaration;
class TypeName;
class CodeGenContext;
class NFunctionArgumentDeclaration;
class Node;
class NInteger;
class NBoolean;
class NDouble;
class NString;
class NVariableDeclaration;
class NMultiVariableDeclaration;
class NExternVariableDeclaration;
class NExternDeclaration;
class NStructDeclaration;
class NUnionDeclaration;
class NImplDeclaration;
class NEnumDeclaration;
class NExpressionStatement;
class NBinaryOperator;
class NMethodCall;
class NAssignment;
class NAddressOfExpression;
class NExpressionPack;
class NIfStatement;
class NWhileStatement;
class NForStatement;
class NReturnStatement;
class NInitializerListExpression;
class NCastExpression;
class NSizeofExpression;

class CodeGenBlock {
public:
    llvm::BasicBlock *block;
    llvm::Function *function;
    CodeGenBlock *parent;
    std::unordered_map<std::string, Value> locals;
    bool returned;

    Optional<Value> local(const std::string &name) const;
};

struct StructInfo {
    Type type;
    struct FieldInfo {
        std::string name;
        bool mut;
        Type type;
    };
    std::vector<FieldInfo> fields;
    Value sizeValue;
};

struct UnionInfo {
    llvm::Type *type;
    struct FieldInfo {
        std::string name;
        bool mut;
        Type type;
    };
    std::vector<FieldInfo> fields;
};

struct TupleInfo {
    llvm::Type *type;
};

struct FunctionInfo {
    std::vector<Type> argTypes;
    Type returnType;
};

class Allocator
{
public:
    virtual ~Allocator() {}

    virtual llvm::Value *allocate(llvm::Type *ty, const std::string &name) = 0;
    virtual llvm::Value *allocateSized(llvm::Type *ty, llvm::Value *sizeValue, const std::string &name) = 0;

};

class Debug
{
public:
    Debug(CodeGenContext *ctx, const std::string &filename);

    llvm::DIFile *fileUnit(const std::string &name);
    llvm::DIBuilder &builder() { return m_builder; }

    llvm::DISubprogram *createFunction(const Token &token, const std::string &funcName, const std::vector<NFunctionArgumentDeclaration> &arguments);
    void createVariable(const Token &token, const std::string &name, const Value &value);

    void pushScope(llvm::DIScope *scope);
    void popScope();

    void setGlobalScope();
    void restoreScope();

    void setLocation(const Token &token);

    void finalize();

private:
    CodeGenContext *m_ctx;
    llvm::DIBuilder m_builder;
    llvm::DIFile *m_file;
    llvm::DICompileUnit *m_cunit;
    std::unordered_map<std::string, llvm::DIFile *> m_fileUnits;
    std::vector<llvm::DIScope *> m_scopes;
    llvm::DIScope *m_scope;
};

class CodeGenContext : public Stage
{
public:
    CodeGenContext(const std::string &name);

    Optional<Value> visit(Node &, int pass) { return {}; }
    Optional<Value> visit(NInteger &integer, int pass);
    Optional<Value> visit(NBoolean &b, int pass);
    Optional<Value> visit(NDouble &b, int pass);
    Optional<Value> visit(NString &str, int pass);
    Optional<Value> visit(NVariableDeclaration &decl, int pass);
    Optional<Value> visit(NMultiVariableDeclaration &decl, int pass);
    Optional<Value> visit(NExternVariableDeclaration &decl, int pass);
    Optional<Value> visit(NIdentifier &ident, int pass);
    Optional<Value> visit(NFunctionDeclaration &decl, int pass);
    Optional<Value> visit(NExternDeclaration &decl, int pass);
    Optional<Value> visit(NStructDeclaration &decl, int pass);
    Optional<Value> visit(NUnionDeclaration &decl, int pass);
    Optional<Value> visit(NIfaceDeclaration &decl, int pass);
    Optional<Value> visit(NImplDeclaration &decl, int pass);
    Optional<Value> visit(NEnumDeclaration &decl, int pass);
    Optional<Value> visit(NBlock &block, int pass);
    Optional<Value> visit(NExpressionStatement &expr, int pass);
    Optional<Value> visit(NBinaryOperator &op, int pass);
    Optional<Value> visit(NMethodCall &call, int pass);
    Optional<Value> visit(NAssignment &ass, int pass);
    Optional<Value> visit(NAddressOfExpression &addr, int pass);
    Optional<Value> visit(NExpressionPack &pack, int pass);
    Optional<Value> visit(NIfStatement &ifs, int pass);
    Optional<Value> visit(NWhileStatement &ws, int pass);
    Optional<Value> visit(NForStatement &fs, int pass);
    Optional<Value> visit(NReturnStatement &ret, int pass);
    Optional<Value> visit(NInitializerListExpression &init, int pass);
    Optional<Value> visit(NCastExpression &cast, int pass);
    Optional<Value> visit(NSizeofExpression &so, int pass);

    Debug &debug() { return m_debug; }

    bool isFunctionNameAvailable(const std::string &name) const;

    llvm::IRBuilder<> &builder() { return m_builder; }
    llvm::LLVMContext &context() { return m_context; }
    llvm::Module &module() { return *m_module.get(); }
    const llvm::Module &module() const { return *m_module.get(); }

    llvm::Type *typeOf(const std::string &name);

    const StructInfo *structInfo(llvm::Type *type) const;
    const StructInfo *structInfo(const std::string &name) const;
    StructInfo *newStructType(llvm::StructType *type);

    const UnionInfo *unionInfo(llvm::Type *type) const;
    const UnionInfo *unionInfo(const std::string &name) const;
    UnionInfo *newUnionType(llvm::StructType *type);

    llvm::StructType *tupleType(const std::vector<llvm::Type *> &argTypes);
    llvm::Value *makeTupleValue(const std::vector<llvm::Value *> &values, const std::string &name);

    const TupleInfo *tupleInfo(llvm::Type *type) const;

    NIfacePrototype *ifacePrototype(const std::string &name) const;
    NIfaceDeclaration *interface(const std::string &name) const;
    void addInterface(NIfaceDeclaration *iface);

    FunctionInfo *addFunctionInfo(llvm::Function *function);
    const FunctionInfo *functionInfo(llvm::Function *function) const;

    void generateCode(NBlock &root);

    void writeOutput(const std::string &filename);

    void storeLocal(const std::string &name, Value value);
    Optional<Value> local(const std::string &name) const;

    void storeGlobal(const std::string &name, Value value);
    Optional<Value> global(const std::string &name) const;

    const CodeGenBlock *currentBlock() const { return &m_blocks.top(); }
    CodeGenBlock *currentBlock() { return &m_blocks.top(); }
    void pushBlock(llvm::BasicBlock *block, llvm::Function *function, CodeGenBlock *parent);
    void popBlock();

    llvm::Value *convertTo(llvm::Value *value, const Type &from, const Type &to);

    bool addDeclaredType(const std::string &name, llvm::Type *type);
    llvm::Type *declaredType(const std::string &name) const;

    void setAllocationBlock(CodeGenBlock *b) { m_allocationBlock = b; }
    CodeGenBlock *allocationBlock() const { return m_allocationBlock; }

    void inject(Node *node, InjectScope scope);
    int typeSize(const Type &t);
    bool isFunctionDefined(const std::string &name, const std::vector<Type> &args) const override;

private:
    llvm::Function *makeConcreteFunction(NFunctionDeclaration *func, std::vector<FirstClassValue *> &values);

    std::stack<CodeGenBlock> m_blocks;
    llvm::Function *m_mainFunction;
    llvm::LLVMContext m_context;
    std::unique_ptr<llvm::Module> m_module;
    llvm::IRBuilder<> m_builder;
    Debug m_debug;
    CodeGenBlock *m_allocationBlock = nullptr;
    std::unordered_map<std::string, StructInfo> m_structInfo;
    std::unordered_map<llvm::Type *, StructInfo *> m_structInfoByType;
    std::unordered_map<std::string, UnionInfo> m_unionInfo;
    std::unordered_map<llvm::Type *, UnionInfo *> m_unionInfoByType;
    std::unordered_map<std::string, llvm::StructType *> m_tupleTypes;
    std::unordered_map<llvm::Type *, TupleInfo> m_tupleInfo;
    std::unordered_map<std::string, NIfacePrototype *> m_ifacePrototypes;
    std::unordered_map<std::string, NIfaceDeclaration *> m_interfaces;
    std::unordered_map<std::string, Value> m_globals;
    std::unordered_map<llvm::Function *, FunctionInfo> m_functionInfo;
    std::unordered_map<std::string, llvm::Type *> m_declaredTypes;
};

std::string typeName(llvm::Type *ty);

class StackAllocator : public Allocator
{
public:
    StackAllocator(CodeGenContext &ctx)
        : m_ctx(ctx)
    {}

    llvm::Value *allocate(llvm::Type *ty, const std::string &name) override;
    llvm::Value *allocateSized(llvm::Type *ty, llvm::Value *sizeValue, const std::string &name) override;

private:
    CodeGenContext &m_ctx;
};

#endif
