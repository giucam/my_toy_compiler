
#ifndef CHECKER_H
#define CHECKER_H

#include <unordered_map>
#include <stack>
#include <variant>

#include "stage.h"
#include "types.h"

class Node;
class NInteger;
class NBoolean;
class NString;
class NVariableDeclaration;
class NIdentifier;
class NFunctionDeclaration;
class NExternDeclaration;
class NStructDeclaration;
class NBlock;
class NBinaryOperator;
class NMethodCall;
class NAddressOfExpression;
class NExpressionPack;
class NIfStatement;
class NReturnStatement;
class NInitializerListExpression;
class NCastExpression;

class Checker : public Stage
{
public:
    class Instance
    {
    public:
        Instance(Type t);
        Instance(const Instance &) = delete;

        struct Aggregate {
            Aggregate(const std::string &name) : m_name(name) {}
            std::shared_ptr<Instance> field(const std::string &name);
            std::shared_ptr<Instance> field(int index);
            std::vector<std::shared_ptr<Instance>> &fields() { return m_fields; }

            std::vector<std::shared_ptr<Instance>> m_fields;
            std::string m_name;
        };
        struct Tuple {
            std::shared_ptr<Instance> field(int index);
            std::vector<std::shared_ptr<Instance>> &fields() { return m_fields; }

            std::vector<std::shared_ptr<Instance>> m_fields;
        };
        struct Pointer {
            Pointer(Type t);
            void pointTo(std::shared_ptr<Instance> i) { m_pointed = i; }
            std::shared_ptr<Instance> pointed() const;

            std::shared_ptr<Instance> m_pointed;
        };
        struct Primitive {};
        struct Void {};
        struct ArgPack {};
        struct DynArray {};
        struct DeclaredAggregate {};

        template<class T>
        T *kind() { return std::get_if<T>(&m_kind); }

        Type &type() { return m_type; }

    private:
        Type m_type;
        std::variant<Aggregate, Tuple, Pointer, Primitive, Void, ArgPack, DynArray, DeclaredAggregate> m_kind;
    };

    struct StructInfo {
        Type type;
        struct FieldInfo {
            std::string name;
            bool mut;
            Type type;
        };
        std::vector<FieldInfo> fields;
    };
    struct CodeGenBlock {
        CodeGenBlock *parent;
        std::unordered_map<std::string, std::shared_ptr<Instance>> locals;
        bool returned;

        std::shared_ptr<Instance> local(const std::string &name);
    };
    struct FunctionInfo {
        bool defined;
        Type returnType;
        struct Arg {
            std::string name;
            std::shared_ptr<Instance> instance;
        };
        std::vector<Arg> args;
    };
public:

    struct Declaration {};
    struct DummyType {};

    using ReturnType = std::variant<std::shared_ptr<Instance>, Declaration, DummyType>;

    Checker();

    ReturnType visit(Node &, const Type &hint);
    ReturnType visit(NInteger &integer, const Type &hint);
    ReturnType visit(NBoolean &b, const Type &hint);
    ReturnType visit(NString &str, const Type &hint);
    ReturnType visit(NVariableDeclaration &decl, const Type &hint);
    ReturnType visit(NIdentifier &ident, const Type &hint);
    ReturnType visit(NFunctionDeclaration &decl, const Type &hint);
    ReturnType visit(NExternDeclaration &decl, const Type &hint);
    ReturnType visit(NStructDeclaration &decl, const Type &hint);
    ReturnType visit(NBlock &block, const Type &hint);
    ReturnType visit(NBinaryOperator &op, const Type &hint);
    ReturnType visit(NMethodCall &call, const Type &hint);
    ReturnType visit(NAddressOfExpression &addr, const Type &hint);
    ReturnType visit(NExpressionPack &pack, const Type &hint);
    ReturnType visit(NIfStatement &ifs, const Type &hint);
    ReturnType visit(NReturnStatement &ret, const Type &hint);
    ReturnType visit(NInitializerListExpression &init, const Type &hint);
    ReturnType visit(NCastExpression &cast, const Type &hint);

    void inject(Node *node, InjectScope scope) override;
    int typeSize(const Type &t) override;
    bool isFunctionDefined(const std::string &name) const override;

    const StructInfo *structInfo(llvm::Type *type) const;
    StructInfo *structInfo(llvm::Type *type);
    const StructInfo *structInfo(const std::string &name) const;
    StructInfo *newStructType(const std::string &name);

    void storeLocal(const std::string &name, std::shared_ptr<Instance> inst);
    std::shared_ptr<Instance> local(const std::string &name);

    const CodeGenBlock *currentBlock() const { return &m_blocks.top(); }
    CodeGenBlock *currentBlock() { return &m_blocks.top(); }
    void pushBlock(CodeGenBlock *parent);
    void popBlock();

    FunctionInfo *addFunctionInfo(const std::string &name);
    FunctionInfo *functionInfo(const std::string &name);

private:
    std::unordered_map<std::string, StructInfo> m_structInfo;
    std::stack<CodeGenBlock> m_blocks;
    std::unordered_map<std::string, FunctionInfo> m_functionInfo;
};

#endif
