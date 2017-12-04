
#ifndef CHECKER_H
#define CHECKER_H

#include <unordered_map>
#include <stack>
#include <variant>
#include <map>

#include "stage.h"
#include "types.h"

class Node;
class NInteger;
class NDouble;
class NBoolean;
class NString;
class NVariableDeclaration;
class NMultiVariableDeclaration;
class NExternVariableDeclaration;
class NIdentifier;
class NAssignment;
class NFunctionDeclaration;
class NExternDeclaration;
class NStructDeclaration;
class NUnionDeclaration;
class NEnumDeclaration;
class NIfaceDeclaration;
class NImplDeclaration;
class NBlock;
class NBinaryOperator;
class NMethodCall;
class NAddressOfExpression;
class NExpressionPack;
class NIfStatement;
class NReturnStatement;
class NInitializerListExpression;
class NCastExpression;
class NExpressionStatement;
class NForStatement;
class NWhileStatement;
class NExpression;
class NStatement;
class NSizeofExpression;

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
        struct Array {};
        struct DeclaredAggregate {};
        struct Templated {};

        template<class T>
        T *kind() { return std::get_if<T>(&m_kind); }

        Type &type() { return m_type; }

    private:
        Type m_type;
        std::variant<Aggregate, Tuple, Pointer, Primitive, Void, ArgPack, DynArray, Array, DeclaredAggregate, Templated> m_kind;
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
        std::string name;
        bool varargs;
        bool defined;
        Type returnType;
        struct Arg {
            std::string name;
            Type type;
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
    ReturnType visit(NDouble &d, const Type &hint);
    ReturnType visit(NBoolean &b, const Type &hint);
    ReturnType visit(NString &str, const Type &hint);
    ReturnType visit(NVariableDeclaration &decl, const Type &hint);
    ReturnType visit(NMultiVariableDeclaration &decl, const Type &hint);
    ReturnType visit(NExternVariableDeclaration &decl, const Type &hint);
    ReturnType visit(NIdentifier &ident, const Type &hint);
    ReturnType visit(NAssignment &ass, const Type &hint);
    ReturnType visit(NFunctionDeclaration &decl, const Type &hint);
    ReturnType visit(NExternDeclaration &decl, const Type &hint);
    ReturnType visit(NStructDeclaration &decl, const Type &hint);
    ReturnType visit(NUnionDeclaration &decl, const Type &hint);
    ReturnType visit(NEnumDeclaration &decl, const Type &hint);
    ReturnType visit(NIfaceDeclaration &decl, const Type &hint);
    ReturnType visit(NImplDeclaration &decl, const Type &hint);
    ReturnType visit(NBlock &block, const Type &hint);
    ReturnType visit(NBinaryOperator &op, const Type &hint);
    ReturnType visit(NMethodCall &call, const Type &hint);
    ReturnType visit(NAddressOfExpression &addr, const Type &hint);
    ReturnType visit(NExpressionPack &pack, const Type &hint);
    ReturnType visit(NIfStatement &ifs, const Type &hint);
    ReturnType visit(NReturnStatement &ret, const Type &hint);
    ReturnType visit(NInitializerListExpression &init, const Type &hint);
    ReturnType visit(NCastExpression &cast, const Type &hint);
    ReturnType visit(NExpressionStatement &s, const Type &hint);
    ReturnType visit(NForStatement &s, const Type &hint);
    ReturnType visit(NWhileStatement &w, const Type &hint);
    ReturnType visit(NSizeofExpression &so, const Type &hint);

    void inject(Node *node, InjectScope scope) override;
    bool isFunctionDefined(const std::string &name, const std::vector<Type> &args) const override;

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
    FunctionInfo *functionInfo(const std::string &name, const std::vector<Type> &types);

    void addFunctionTemplate(NFunctionDeclaration *func);
    FunctionInfo *functionTemplate(const std::string &name, const std::vector<std::shared_ptr<Instance>> args);
    FunctionInfo *makeConcreteFunction(NFunctionDeclaration *func, const std::vector<std::shared_ptr<Instance>> args);

    void storeGlobal(const std::string &name, const std::shared_ptr<Instance> &value);
    std::shared_ptr<Instance> global(const std::string &name) const;

    NBlock *rootBlock() const { return m_rootBlock; }

private:
    void addStatement(NStatement *s);

    NBlock *m_currentBlock = nullptr;
    NBlock *m_rootBlock = nullptr;
    std::unordered_map<std::string, StructInfo> m_structInfo;
    std::stack<CodeGenBlock> m_blocks;
    std::multimap<std::string, FunctionInfo> m_functionInfo;
    std::unordered_map<std::string, NFunctionDeclaration *> m_functionTemplates;
    std::unordered_map<std::string, std::shared_ptr<Instance>> m_globals;
    std::unique_ptr<NExpression> m_expression;
};

#endif
