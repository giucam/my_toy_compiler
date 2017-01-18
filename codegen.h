
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
class CodeGenContext;

template<class T>
class Optional
{
public:
    Optional() : m_valid(false) {}
    Optional(T t) : m_valid(true), m_value(std::move(t)) {}

    operator bool() const { return m_valid; }
    operator const T &() const { return m_value; }
    const T *operator->() const { return &m_value; }

private:
    bool m_valid;
    T m_value;
};

class Value {
public:
    struct V {
        llvm::Value *value;
        llvm::Type *type;

        llvm::Value *load(CodeGenContext &ctx) const;
    };

private:
    struct IfaceBase
    {
        virtual ~IfaceBase() {}

        virtual const std::vector<Value::V> &unpack() const = 0;
        virtual llvm::Value *extract(int id) const = 0;
        virtual llvm::Value *extract(const std::string &name) const = 0;

        virtual std::shared_ptr<const IfaceBase> clone(std::vector<V> &values) const = 0;
    };
    template<class T>
    struct Iface : IfaceBase
    {
        Iface(T h) : handler(std::move(h)) {}
        const std::vector<Value::V> &unpack() const override { return handler.unpack(); }
        llvm::Value *extract(int id) const override { return handler.extract(id); }
        llvm::Value *extract(const std::string &name) const override { return handler.extract(name); }
        std::shared_ptr<const IfaceBase> clone(std::vector<V> &values) const override { return std::make_shared<Iface<T>>(handler.clone(values)); }
        T handler;
    };

public:
    Value() {}
    template<class T>
    Value(T handler)
        : m_iface(std::make_shared<Iface<T>>(std::move(handler))) {}
    Value(const std::shared_ptr<const IfaceBase> &iface)
        : m_iface(iface) {}

    inline const std::vector<V> &unpack() const { return m_iface->unpack(); }
    inline llvm::Value *extract(int id) const { return m_iface->extract(id); }
    inline llvm::Value *extract(const std::string &name) const { return m_iface->extract(name); }

    Value clone(std::vector<V> &values) const { return Value(m_iface->clone(values)); }

    std::shared_ptr<const IfaceBase> m_iface;
};

Value simpleValue(llvm::Value *val, llvm::Type *t);
inline Value simpleValue(llvm::Value *val) { return simpleValue(val, val->getType()); }

class CodeGenBlock {
public:
    llvm::BasicBlock *block;
    llvm::Function *function;
    CodeGenBlock *parent;
    std::unordered_map<std::string, Value> locals;
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

    llvm::Type *typeOf(const TypeName &type);

    const StructInfo *structInfo(llvm::Type *type) const;
    StructInfo *newStructType(llvm::StructType *type);

    llvm::StructType *tupleType(const std::vector<llvm::Type *> &argTypes);
    llvm::Value *makeTupleValue(const std::vector<llvm::Value *> &values, const std::string &name);

    const TupleInfo *tupleInfo(llvm::Type *type) const;

    NIfacePrototype *ifacePrototype(const std::string &name) const;
    NIfaceDeclaration *interface(const std::string &name) const;
    void addInterface(NIfaceDeclaration *iface);

    llvm::Function *functionTemplate(const std::string &name, std::vector<Value::V> &values);
    void addFunctionTemplate(NFunctionDeclaration *func);

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

    llvm::Value *convertTo(llvm::Value *value, llvm::Type *type) const;

    llvm::Value *allocate(llvm::Type *type, const std::string &name, llvm::Value *toStore);

private:
    llvm::Function *makeConcreteFunction(NFunctionDeclaration *func, std::vector<Value::V> &values);

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
    std::unordered_map<std::string, Value> m_globals;
};

#endif
