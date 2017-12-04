
#ifndef NODE_H
#define NODE_H

#include <iostream>
#include <vector>
#include <memory>
#include <llvm/IR/Value.h>

#include "lexer.h"
#include "codegen.h"
#include "types.h"
#include "checker.h"

class CodeGenContext;
class Value;
class NStatement;
class NExpression;
class NVariableDeclaration;
class NAssignment;
class NIfacePrototype;
class NFunctionDeclaration;
class NImplDeclaration;

typedef std::vector<NStatement*> StatementList;
typedef std::vector<std::unique_ptr<NExpression>> ExpressionList;
typedef std::vector<NVariableDeclaration *> VariableList;
typedef std::vector<NAssignment *> AssignmentList;
typedef std::vector<NIfacePrototype *> NIfacePrototypeList;
typedef std::vector<NFunctionDeclaration *> FuncDeclarationList;

class Node {
public:
    Node() : m_callChecker(std::make_unique<Concrete<Node>>()) { }
    Node(const Token &token) : Node() { m_token = token; }
    virtual ~Node() {}

    template<class T>
    void init(T *derived) {
        m_callChecker = std::make_unique<Concrete<T>>();
    }

    Optional<Value> visit(CodeGenContext &c, int pass) { return m_callChecker->visit(this, c, pass); }
    Checker::ReturnType visit(Checker &c, const Type &hint = VoidType()) { return m_callChecker->visit(this, c, hint); }

    const Token &token() const { return m_token; }

private:
    struct Interface {
        virtual Optional<Value> visit(Node *n, CodeGenContext &c, int pass) = 0;
        virtual Checker::ReturnType visit(Node *n, Checker &c, const Type &h) = 0;
    };
    template<class T>
    struct Concrete : Interface
    {
        Optional<Value> visit(Node *n, CodeGenContext &c, int pass) override
        {
            return c.visit(*static_cast<T *>(n), pass);
        }
        Checker::ReturnType visit(Node *n, Checker &c, const Type &h) override
        {
            return c.visit(*static_cast<T *>(n), h);
        }
    };
    std::unique_ptr<Interface> m_callChecker;
    Token m_token;
};

class NExpression : public Node
{
public:
    NExpression() : Node(), m_context(nullptr) {}
    NExpression(const Token &tok) : Node(tok), m_context(nullptr) {}
    NExpression(const NExpression &) = delete;
    void pushContext(NExpression *context) { m_context = context; }
    NExpression *context() const { return m_context; }

    void attach(std::unique_ptr<NExpression> ex) { m_attached.push_back(std::move(ex)); }

    virtual void pushConstraints(bool negate) {}
    virtual void popConstraints() {}

//     virtual llvm::Value *load(CodeGenContext &context) { return codeGen(context); }
//     virtual std::vector<NExpression *> unpack(CodeGenContext &) { return { this }; }

private:
    NExpression *m_context;
    std::vector<std::unique_ptr<NExpression>> m_attached;
};

class NStatement : public Node {
public:
    NStatement() : Node() {}
    NStatement(const Token &tok) : Node(tok) {}
};

class NInteger : public NExpression {
public:
    long long value;
    NInteger(const Token &tok, long long value) : NExpression(tok), value(value) { init(this); }
};

class NBoolean : public NExpression {
public:
    bool value;
    NBoolean(const Token &tok, bool value) : NExpression(tok), value(value) { init(this); }
};

class NDouble : public NExpression {
public:
    double value;
    NDouble(const Token &tok, double value) : NExpression(tok), value(value) {  init(this); }
};

class NString : public NExpression {
public:
    std::string value;
    NString(const Token &tok, const std::string &val) : NExpression(tok), value(val) { init(this); }

};

class NExpressionPack : public NExpression {
public:
    NExpressionPack(const Token &tok, ExpressionList &l) : NExpression(tok), m_list(std::move(l)) { init(this); }

//     std::vector<NExpression *> unpack(CodeGenContext &context) override;

    const ExpressionList &expressionList() const { return m_list; }

private:
    ExpressionList m_list;
};

class NIdentifier : public NExpression {
public:
    NIdentifier(const Token &token, const std::string &name) : NExpression(token), name(name), type(Name) { init(this); }
    NIdentifier(const Token &token, int index) : NExpression(token), index(index), type(Index) { init(this); }
//     NIdentifier(const NIdentifier &i) : NExpression(), parent(i.parent), type(i.type) { if (i.type == Name) { new (&name) std::string(i.name); } else { index = i.index; } }
    ~NIdentifier() {}

//     llvm::Value *load(CodeGenContext &context) override;
//     std::vector<NExpression *> unpack(CodeGenContext &context) override;

    union {
        std::string name;
        int index;
    };
    enum {
        Name,
        Index
    } type;
};

class NMethodCall : public NExpression {
public:
    const std::string name;
    ExpressionList arguments;

    NMethodCall(const Token &tok, const std::string &name, ExpressionList &args) :
                NExpression(tok), name(name) { init(this); std::swap(arguments, args); }
};

class NBinaryOperator : public NExpression {
public:
    enum class OP {
        Mul,
        Add,
        Sub,
        Div,
        Remainder,
        Lesser,
        Greater,
        Equal,
        NotEqual,
        GreaterEqual,
        LesserEqual,
        Or,
    };

    OP op;
    std::unique_ptr<NExpression> lhs;
    std::unique_ptr<NExpression> rhs;
    NBinaryOperator(const Token &tok, std::unique_ptr<NExpression> lhs, OP op, std::unique_ptr<NExpression> rhs) :
                NExpression(tok), op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) { init(this); }

    void pushConstraints(bool negate) override;
    void popConstraints() override;

    struct Constraint {
        std::function<void (bool)> pushFunc;
        std::function<void ()> popFunc;
    };
    std::vector<Constraint> m_constraints;
};

class NAssignment : public NExpression {
public:
    std::unique_ptr<NExpression> lhs;
    std::unique_ptr<NExpression> rhs;
    NAssignment(const Token &tok, std::unique_ptr<NExpression> lhs, std::unique_ptr<NExpression> rhs)
        : NExpression(tok), lhs(std::move(lhs)), rhs(std::move(rhs)) { init(this); }
};

class NBlock : public NExpression {
public:
    StatementList statements;
    NBlock(): NExpression() { init(this); }
};

class NAddressOfExpression : public NExpression
{
public:
    NAddressOfExpression(const Token &token, std::unique_ptr<NExpression> expr) : NExpression(token), m_expression(std::move(expr)) { init(this); }

    NExpression *expression() const { return m_expression.get(); }

private:
    std::unique_ptr<NExpression> m_expression;
};

class NExpressionStatement : public NStatement {
public:
    NExpressionStatement(std::unique_ptr<NExpression> expression)
        : m_expression(std::move(expression)) { init(this); }

    NExpression *expression() const { return m_expression.get(); }

private:
    std::unique_ptr<NExpression> m_expression;
};

class NReturnStatement : public NStatement {
public:
    NReturnStatement(std::unique_ptr<NExpression> expression)
        : m_expression(std::move(expression)) { init(this); }

    NExpression *expression() const { return m_expression.get(); }

private:
    std::unique_ptr<NExpression> m_expression;
};

class NVariableName
{
public:
    NVariableName(const Token &tok, const std::string &name, bool mut) : m_token(tok), m_name(name), m_mut(mut) {}

    const std::string &name() const { return m_name; }
    const Token &token() const { return m_token; }
    bool isMutable() const { return m_mut; }
private:
    Token m_token;
    std::string m_name;
    bool m_mut;
};

class NVariableDeclaration : public NStatement {
public:
    NVariableName id;
    Type declaredType;
    std::unique_ptr<NExpression> expression;

    NVariableDeclaration(const Token &tok, const NVariableName &name, const Type &type, std::unique_ptr<NExpression> expr)
        : NStatement(tok), id(name), declaredType(type), expression(std::move(expr)) { init(this); }
};

class NMultiVariableDeclaration : public NStatement {
public:
    NMultiVariableDeclaration(const Token &tok, std::vector<NVariableName> &names, std::unique_ptr<NExpression> expr) : NStatement(tok), m_expression(std::move(expr)) { init(this); std::swap(m_names, names); }

    const std::vector<NVariableName> &names() const { return m_names; }
    NExpression *expression() const { return m_expression.get(); }

private:
    std::vector<NVariableName> m_names;
    std::unique_ptr<NExpression> m_expression;
};

class NFunctionArgumentDeclaration
{
public:
    NFunctionArgumentDeclaration(const Token &tok, const std::string &name, Type type, bool mut) : m_token(tok), m_name(name), m_type(type), m_mut(mut) {}

    const Token &token() const { return m_token; }
    const std::string &name() const { return m_name; }
    Type type() const { return m_type; }
    bool isMutable() const { return m_mut; }

private:
    Token m_token;
    std::string m_name;
    Type m_type;
    bool m_mut;
};

class NExternDeclaration : public NStatement {
public:
    NExternDeclaration(const std::string &id, Type type, std::vector<NFunctionArgumentDeclaration> &arguments, bool varargs)
        : m_type(type), m_name(id), m_varargs(varargs) { init(this); std::swap(m_arguments, arguments); }

    const std::string &name() const { return m_name; }
    Type returnType() const { return m_type; }
    const std::vector<NFunctionArgumentDeclaration> &arguments() const { return m_arguments; }
    bool isVarargs() const { return m_varargs; }

private:
    Type m_type;
    std::string m_name;
    std::vector<NFunctionArgumentDeclaration> m_arguments;
    bool m_varargs;
};

class NExternVariableDeclaration : public NStatement {
public:
    NExternVariableDeclaration(const Token &tok, const std::string &name, Type type) : NStatement(tok), m_name(name), m_type(type) { init(this); }

    const std::string &name() const { return m_name; }
    Type type() const { return m_type; }

private:
    std::string m_name;
    Type m_type;
};

class NFunctionDeclaration : public NStatement {
public:
    Type type;
    std::string id;
    std::vector<NFunctionArgumentDeclaration> arguments;
    NBlock *block;
    NFunctionDeclaration(const Token &tok, const std::string &id, const Type &type,
                         const std::vector<NFunctionArgumentDeclaration> &arguments, NBlock *block) :
                            NStatement(tok), type(type), id(id), arguments(arguments), block(block) { init(this); }
};

class NStructDeclaration : public NStatement
{
public:
    struct Field
    {
        std::string name;
        Type type;
        bool mut;
    };

    enum class Flags
    {
        None = 0,
        AbiSafe = 1,
        MasterDefinition = 2,
    };

    std::string id;
    std::vector<Field> elements;
    Type m_type;
    bool m_hasBody;
    Flags m_flags;
    NStructDeclaration(const std::string &id, Flags f);

    const Type &type() const { return m_type; }
    void setFields(std::vector<Field> &e) { std::swap(e, elements); m_hasBody = true; }
};
inline NStructDeclaration::Flags operator|(NStructDeclaration::Flags a, NStructDeclaration::Flags b) { return (NStructDeclaration::Flags)((int)a | (int)b); }
inline NStructDeclaration::Flags &operator|=(NStructDeclaration::Flags &a, NStructDeclaration::Flags b) { a = a | b; return a; }
inline bool operator&(NStructDeclaration::Flags a, NStructDeclaration::Flags b) { return (int)a & (int)b; }

class NUnionDeclaration : public NStatement
{
public:
    struct Field
    {
        std::string name;
        Type type;
        bool mut;
    };

    std::string id;
    std::vector<Field> elements;
    Type m_type;
    NUnionDeclaration(const std::string &id, std::vector<Field> &e);

    const Type &type() const { return m_type; }
};

class NIfaceDeclaration: public NStatement {
public:
    NIfaceDeclaration(const std::string &name, std::vector<Type> &par, NIfacePrototypeList &pro) : name(name) { init(this); std::swap(parameters, par); std::swap(prototypes, pro); }

    std::string name;
    std::vector<Type> parameters;
    NIfacePrototypeList prototypes;
    std::vector<NImplDeclaration *> implementations;
};

class NIfacePrototype : public NStatement {
public:
    NIfacePrototype(const std::string &name, std::vector<Type> &par) : name(name) { init(this); std::swap(parameters, par); }

    std::string name;
    std::vector<Type> parameters;
    NIfaceDeclaration *iface;
};

class NImplDeclaration : public NStatement
{
public:
    NImplDeclaration(const std::string &name, std::vector<Type> &par, FuncDeclarationList &funcs) : name(name) { init(this); std::swap(parameters, par); std::swap(functions, funcs); }

    std::string name;
    std::string id;
    std::vector<Type> parameters;
    FuncDeclarationList functions;
    std::vector<llvm::Type *> parameterTypes;
};

class NIfStatement : public NStatement
{
public:
    NIfStatement(std::unique_ptr<NExpression> condition, NBlock *block, NBlock *elseBlock) : m_condition(std::move(condition)), m_block(block), m_elseBlock(elseBlock) { init(this); }

    NExpression *condition() const { return m_condition.get(); }
    NBlock *block() { return m_block; }
    NBlock *elseBlock() { return m_elseBlock; }

private:
    std::unique_ptr<NExpression> m_condition;
    NBlock *m_block;
    NBlock *m_elseBlock;
};

class NWhileStatement : public NStatement
{
public:
    NWhileStatement(const Token &tok, std::unique_ptr<NExpression> condition, NBlock *block) : NStatement(tok), m_condition(std::move(condition)), m_block(block) { init(this); }

    NExpression *condition() const { return m_condition.get(); }
    NBlock *block() { return m_block; }

private:
    std::unique_ptr<NExpression> m_condition;
    NBlock *m_block;
};

class NEnumDeclaration : public NStatement
{
public:
    struct Entry {
        const std::string name;
        long long int value;
    };

    NEnumDeclaration(const std::string &name, const Type &type, std::vector<Entry> &entries)
        : m_name(name), m_type(type) { init(this); std::swap(entries, m_entries); }

    const std::string &name() const { return m_name; }
    Type type() const { return m_type; }
    const std::vector<Entry> &entries() const { return m_entries; }

private:
    std::string m_name;
    Type m_type;
    std::vector<Entry> m_entries;
};

class NCastExpression : public NExpression
{
public:
    NCastExpression(const Token &token, std::unique_ptr<NExpression> expr, const Type &type)
        : NExpression(token), m_expression(std::move(expr)), m_type(type)
    { init(this); }

    Type type() const { return m_type; }
    NExpression *expression() const { return m_expression.get(); }

private:
    std::unique_ptr<NExpression> m_expression;
    Type m_type;
};

class NForStatement : public NStatement
{
public:
    NForStatement(const Token &itToken, const Token &exprTok, std::unique_ptr<NExpression> arrayExpr, NBlock *block);

    Token itToken() const { return m_itToken; }
    Token exprToken() const { return m_exprToken; }
    NExpression *arrayExpr() const { return m_arrayExpr.get(); }
    NBlock *block() const { return m_block; }

private:
    Token m_itToken;
    Token m_exprToken;
    std::unique_ptr<NExpression> m_arrayExpr;
    NBlock *m_block;
};

class NInitializerListExpression : public NExpression
{
public:
    struct Initializer
    {
        Token token;
        std::string name;
        std::unique_ptr<NExpression> value;
    };

    NInitializerListExpression(const Token &tok, std::vector<Initializer> &initializers);

    const std::vector<Initializer> &initializers() const { return m_initializers; }

private:
    std::vector<Initializer> m_initializers;
};

class NSizeofExpression : public NExpression
{
public:
    NSizeofExpression(const Token &tok, const Type &t) : NExpression(tok), type(t) { init(this); }
    Type type;
};

#endif
