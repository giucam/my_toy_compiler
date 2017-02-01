
#ifndef NODE_H
#define NODE_H

#include <iostream>
#include <vector>
#include <memory>
#include <llvm/IR/Value.h>

#include "lexer.h"
#include "codegen.h"
#include "types.h"

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
    Node() {}
    Node(const Token &token) : m_token(token) {}
    virtual ~Node() {}
    virtual Optional<Value> codeGen(CodeGenContext &context) { return {}; }

    const Token &token() const { return m_token; }

private:
    Token m_token;
};

class NExpression : public Node
{
public:
    NExpression() : m_context(nullptr) {}
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
    NStatement() {}
    NStatement(const Token &tok) : Node(tok) {}
};

class NInteger : public NExpression {
public:
    long long value;
    NInteger(const Token &tok, long long value) : NExpression(tok), value(value) { }
    Optional<Value> codeGen(CodeGenContext &context) override;
};

class NBoolean : public NExpression {
public:
    bool value;
    NBoolean(const Token &tok, bool value) : NExpression(tok), value(value) { }
    Optional<Value> codeGen(CodeGenContext &context) override;
};

class NDouble : public NExpression {
public:
    double value;
    NDouble(const Token &tok, double value) : NExpression(tok), value(value) { }
    Optional<Value> codeGen(CodeGenContext &context) override;
};

class NString : public NExpression {
public:
    std::string value;
    NString(const std::string &val) : NExpression(), value(val) {}

    Optional<Value> codeGen(CodeGenContext &context) override;
};

class NExpressionPack : public NExpression {
public:
    NExpressionPack(const Token &tok, ExpressionList &l) : NExpression(tok), m_list(std::move(l)) {}

    Optional<Value> codeGen(CodeGenContext &context) override;
//     std::vector<NExpression *> unpack(CodeGenContext &context) override;

private:
    ExpressionList m_list;
};

class NIdentifier : public NExpression {
public:
    NIdentifier(const Token &token, const std::string &name) : NExpression(token), name(name), type(Name) { }
    NIdentifier(const Token &token, int index) : NExpression(token), index(index), type(Index) {}
//     NIdentifier(const NIdentifier &i) : NExpression(), parent(i.parent), type(i.type) { if (i.type == Name) { new (&name) std::string(i.name); } else { index = i.index; } }
    ~NIdentifier() {}

    Optional<Value> codeGen(CodeGenContext &context) override;
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
                NExpression(tok), name(name) { std::swap(arguments, args); }
    Optional<Value> codeGen(CodeGenContext &context) override;
};

class NBinaryOperator : public NExpression {
public:
    enum class OP {
        Mul,
        Add,
        Sub,
        Div,
        Lesser,
        Greater,
        Equal,
        NotEqual,
    };

    OP op;
    std::unique_ptr<NExpression> lhs;
    std::unique_ptr<NExpression> rhs;
    NBinaryOperator(std::unique_ptr<NExpression> lhs, OP op, std::unique_ptr<NExpression> rhs) :
                NExpression(), op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) { }
    Optional<Value> codeGen(CodeGenContext &context) override;

    void pushConstraints(bool negate) override;
    void popConstraints() override;

    struct Constraint {
        Value lVal;
        Value rVal;
    };
    std::vector<Constraint> m_constraints;
};

class NAssignment : public NExpression {
public:
    std::unique_ptr<NExpression> lhs;
    std::unique_ptr<NExpression> rhs;
    NAssignment(const Token &tok, std::unique_ptr<NExpression> lhs, std::unique_ptr<NExpression> rhs)
        : NExpression(tok), lhs(std::move(lhs)), rhs(std::move(rhs)) { }

    Optional<Value> codeGen(CodeGenContext &context) override;
};

class NBlock : public NExpression {
public:
    StatementList statements;
    NBlock(): NExpression() { }
    Optional<Value> codeGen(CodeGenContext &context) override;
};

class NAddressOfExpression : public NExpression
{
public:
    NAddressOfExpression(const Token &token, std::unique_ptr<NExpression> expr) : NExpression(token), m_expression(std::move(expr)) {}

    NExpression *expression() const { return m_expression.get(); }

    Optional<Value> codeGen(CodeGenContext &context) override;

private:
    std::unique_ptr<NExpression> m_expression;
};

class NExpressionStatement : public NStatement {
public:
    NExpressionStatement(std::unique_ptr<NExpression> expression)
        : expression(std::move(expression)) { }

    Optional<Value> codeGen(CodeGenContext &context) override;

private:
    std::unique_ptr<NExpression> expression;
};

class NReturnStatement : public NStatement {
public:
    NReturnStatement(std::unique_ptr<NExpression> expression)
        : expression(std::move(expression)) { }

    Optional<Value> codeGen(CodeGenContext &context) override;

private:
    std::unique_ptr<NExpression> expression;
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

class NVariableInitializer
{
public:
    virtual Value init(CodeGenContext &ctx, const std::string &name) = 0;
};

class NVarExpressionInitializer : public NVariableInitializer
{
public:
    NVarExpressionInitializer(const Token &tok, const Type &type, std::unique_ptr<NExpression> expr) : token(tok), type(type), expression(std::move(expr)) {}

    Value init(CodeGenContext &ctx, const std::string &name) override;

    Token token;
    Type type;
    std::unique_ptr<NExpression> expression;
};

class NVarStructInitializer : public NVariableInitializer
{
public:
    struct Field
    {
        std::string name;
        std::unique_ptr<NExpression> init;
    };
    Token token;
    Type type;
    std::vector<Field> fields;

    NVarStructInitializer(const Token &tok, const Type &type, std::vector<Field> &f) : token(tok), type(type) { std::swap(fields, f); }

    Value init(CodeGenContext &ctx, const std::string &name) override;
};

class NVariableDeclaration : public NStatement {
public:
    NVariableName id;
    std::unique_ptr<NVariableInitializer> init;
    NVariableDeclaration(const Token &tok, const NVariableName &name, std::unique_ptr<NVariableInitializer> init) : NStatement(tok), id(name), init(std::move(init)) { }

    Optional<Value> codeGen(CodeGenContext &context) override;
};

class NMultiVariableDeclaration : public NStatement {
public:
    NMultiVariableDeclaration(const Token &tok, std::vector<NVariableName> &names, std::unique_ptr<NExpression> expr) : NStatement(tok), m_expression(std::move(expr)) { std::swap(m_names, names); }

    const std::vector<NVariableName> &names() const { return m_names; }
    NExpression *expression() const { return m_expression.get(); }

    Optional<Value> codeGen(CodeGenContext &context) override;

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
        : m_type(type), m_name(id), m_varargs(varargs) { std::swap(m_arguments, arguments); }

    const std::string &name() const { return m_name; }
    Type returnType() const { return m_type; }
    const std::vector<NFunctionArgumentDeclaration> &arguments() const { return m_arguments; }
    bool isVarargs() const { return m_varargs; }

    Optional<Value> codeGen(CodeGenContext &context) override;

private:
    Type m_type;
    std::string m_name;
    std::vector<NFunctionArgumentDeclaration> m_arguments;
    bool m_varargs;
};

class NExternVariableDeclaration : public NStatement {
public:
    NExternVariableDeclaration(const Token &tok, const std::string &name, Type type) : NStatement(tok), m_name(name), m_type(type) {}

    const std::string &name() const { return m_name; }
    Type type() const { return m_type; }

    Optional<Value> codeGen(CodeGenContext &context) override;

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
    NFunctionDeclaration(const std::string &id, const Type &type,
                         const std::vector<NFunctionArgumentDeclaration> &arguments, NBlock *block) :
                            type(type), id(id), arguments(arguments), block(block) { }

    Optional<Value> codeGen(CodeGenContext &context) override;
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

    std::string id;
    std::vector<Field> elements;
    Type m_type;
    bool m_hasBody;
    NStructDeclaration(const std::string &id);

    const Type &type() const { return m_type; }
    void setFields(std::vector<Field> &e) { std::swap(e, elements); m_hasBody = true; }

    Optional<Value> codeGen(CodeGenContext &context) override;
};

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

    Optional<Value> codeGen(CodeGenContext &context) override;
};

class NIfaceDeclaration: public NStatement {
public:
    NIfaceDeclaration(const std::string &name, std::vector<Type> &par, NIfacePrototypeList &pro) : name(name) { std::swap(parameters, par); std::swap(prototypes, pro); }
    Optional<Value> codeGen(CodeGenContext &context) override;

    std::string name;
    std::vector<Type> parameters;
    NIfacePrototypeList prototypes;
    std::vector<NImplDeclaration *> implementations;
};

class NIfacePrototype : public NStatement {
public:
    NIfacePrototype(const std::string &name, std::vector<Type> &par) : name(name) { std::swap(parameters, par); }

    std::string name;
    std::vector<Type> parameters;
    NIfaceDeclaration *iface;
};

class NImplDeclaration : public NStatement
{
public:
    NImplDeclaration(const std::string &name, std::vector<Type> &par, FuncDeclarationList &funcs) : name(name) { std::swap(parameters, par); std::swap(functions, funcs); }

    Optional<Value> codeGen(CodeGenContext &context) override;

    std::string name;
    std::string id;
    std::vector<Type> parameters;
    FuncDeclarationList functions;
    std::vector<llvm::Type *> parameterTypes;
};

class NIfStatement : public NStatement
{
public:
    NIfStatement(std::unique_ptr<NExpression> condition, NBlock *block, NBlock *elseBlock) : m_condition(std::move(condition)), m_block(block), m_elseBlock(elseBlock) {}

    NExpression *condition() const { return m_condition.get(); }
    NBlock *block() { return m_block; }
    NBlock *elseBlock() { return m_elseBlock; }

    Optional<Value> codeGen(CodeGenContext &context) override;

private:
    std::unique_ptr<NExpression> m_condition;
    NBlock *m_block;
    NBlock *m_elseBlock;
};

class NWhileStatement : public NStatement
{
public:
    NWhileStatement(std::unique_ptr<NExpression> condition, NBlock *block) : m_condition(std::move(condition)), m_block(block) {}

    NExpression *condition() const { return m_condition.get(); }
    NBlock *block() { return m_block; }

    Optional<Value> codeGen(CodeGenContext &context) override;

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
        : m_name(name), m_type(type) { std::swap(entries, m_entries); }

    Optional<Value> codeGen(CodeGenContext &context) override;

private:
    std::string m_name;
    Type m_type;
    std::vector<Entry> m_entries;
};

#endif
