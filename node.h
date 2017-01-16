
#ifndef NODE_H
#define NODE_H

#include <iostream>
#include <vector>
#include <memory>
#include <llvm/IR/Value.h>

#include "lexer.h"

class CodeGenContext;
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
    virtual llvm::Value* codeGen(CodeGenContext& context) { return NULL; }

    const Token &token() const { return m_token; }

private:
    Token m_token;
};

class TypeName
{
public:
    TypeName() {}
    TypeName(const Token &tok, const std::string &name, int pointer = 0) : m_token(tok), m_name(name), m_pointer(pointer) {}

    const Token &token() const { return m_token; }
    bool valid() const { return !m_name.empty(); }
    const std::string &name() const { return m_name; }
    int pointer() const { return m_pointer; }
private:
    Token m_token;
    std::string m_name;
    int m_pointer;
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

    virtual llvm::Value *load(CodeGenContext &context) { return codeGen(context); }
    virtual std::vector<NExpression *> unpack(CodeGenContext &) { return { this }; }

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
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NDouble : public NExpression {
public:
	double value;
	NDouble(const Token &tok, double value) : NExpression(tok), value(value) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NString : public NExpression {
public:
    std::string value;
    NString(const std::string &val) : NExpression(), value(val) {}

    virtual llvm::Value* codeGen(CodeGenContext& context) override;
};

class NExpressionPack : public NExpression {
public:
    NExpressionPack(const Token &tok, ExpressionList &l) : NExpression(tok), m_list(std::move(l)) {}

    llvm::Value *codeGen(CodeGenContext &context) override;
    std::vector<NExpression *> unpack(CodeGenContext &context) override;

private:
    ExpressionList m_list;
};

class NIdentifier : public NExpression {
public:
    NIdentifier(const Token &token, const std::string &name) : NExpression(token), name(name), type(Name) { }
    NIdentifier(const Token &token, int index) : NExpression(token), index(index), type(Index) {}
//     NIdentifier(const NIdentifier &i) : NExpression(), parent(i.parent), type(i.type) { if (i.type == Name) { new (&name) std::string(i.name); } else { index = i.index; } }
    ~NIdentifier() {}

    llvm::Value *codeGen(CodeGenContext &context) override;
    llvm::Value *load(CodeGenContext &context) override;
    std::vector<NExpression *> unpack(CodeGenContext &context) override;

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
// 	NMethodCall(const std::string &name) : NExpression(), name(name) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
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
    };

    OP op;
    std::unique_ptr<NExpression> lhs;
    std::unique_ptr<NExpression> rhs;
    NBinaryOperator(std::unique_ptr<NExpression> lhs, OP op, std::unique_ptr<NExpression> rhs) :
                NExpression(), op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) { }
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NAssignment : public NExpression {
public:
    std::unique_ptr<NExpression> lhs;
    std::unique_ptr<NExpression> rhs;
    llvm::Value *rhsValue;
    NAssignment(std::unique_ptr<NExpression> lhs, std::unique_ptr<NExpression> rhs)
        : NExpression(), lhs(std::move(lhs)), rhs(std::move(rhs)), rhsValue(nullptr) { }

    llvm::Value *codeGen(CodeGenContext &context) override;

    llvm::Value *genRhs(CodeGenContext &context);
};

class NBlock : public NExpression {
public:
	StatementList statements;
	NBlock(): NExpression() { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NAddressOfExpression : public NExpression
{
public:
    NAddressOfExpression(const Token &token, std::unique_ptr<NExpression> expr) : NExpression(token), m_expression(std::move(expr)) {}

    NExpression *expression() const { return m_expression.get(); }

    llvm::Value *codeGen(CodeGenContext &context) override;

private:
    std::unique_ptr<NExpression> m_expression;
};

class NExpressionStatement : public NStatement {
public:
    NExpressionStatement(std::unique_ptr<NExpression> expression)
        : expression(std::move(expression)) { }

    llvm::Value* codeGen(CodeGenContext &context) override;

private:
    std::unique_ptr<NExpression> expression;
};

class NReturnStatement : public NStatement {
public:
    NReturnStatement(std::unique_ptr<NExpression> expression)
        : expression(std::move(expression)) { }

    llvm::Value *codeGen(CodeGenContext &context) override;

private:
    std::unique_ptr<NExpression> expression;
};

class NVariableName
{
public:
    NVariableName(const Token &tok, const std::string &name) : m_token(tok), m_name(name) {}

    const std::string &name() const { return m_name; }
    const Token &token() const { return m_token; }
private:
    Token m_token;
    std::string m_name;
};

class NVariableDeclaration : public NStatement {
public:
    TypeName type;
    NVariableName id;
    AssignmentList expressions;
    NVariableDeclaration(const Token &tok, const NVariableName &name, const TypeName &type) : NStatement(tok), type(type), id(name) { }
    NVariableDeclaration(const Token &tok, const NVariableName &name, const TypeName &type, NAssignment *assignmentExpr) : NStatement(tok), type(type), id(name), expressions({ assignmentExpr }) { }
    NVariableDeclaration(const Token &tok, const NVariableName &name, const TypeName &type, AssignmentList exprs) : NStatement(tok), type(type), id(name), expressions(exprs) { }
    NVariableDeclaration(const Token &tok, const NVariableName &name, NAssignment *expr) : NStatement(tok), id(name), expressions({ expr }) {}

    llvm::Value *codeGen(CodeGenContext& context) override;
};

class NMultiVariableDeclaration : public NStatement {
public:
    NMultiVariableDeclaration(const Token &tok, std::vector<NVariableName> &names, std::unique_ptr<NExpression> expr) : NStatement(tok), m_expression(std::move(expr)) { std::swap(m_names, names); }

    const std::vector<NVariableName> &names() const { return m_names; }
    NExpression *expression() const { return m_expression.get(); }

    llvm::Value *codeGen(CodeGenContext& context) override;

private:
    std::vector<NVariableName> m_names;
    std::unique_ptr<NExpression> m_expression;
};

class NFunctionArgumentDeclaration : public NStatement
{
public:
    NFunctionArgumentDeclaration(const Token &tok, const std::string &name, const TypeName &type) : NStatement(tok), m_name(name), m_type(type) {}

    const std::string &name() const { return m_name; }
    const TypeName &type() const { return m_type; }

    llvm::Value *codeGen(CodeGenContext& context) override;

private:
    Token m_token;
    std::string m_name;
    TypeName m_type;
};

class NExternDeclaration : public NStatement {
public:
    NExternDeclaration(const std::string &id, const TypeName &type, std::vector<NFunctionArgumentDeclaration> &arguments, bool varargs)
        : m_type(type), m_name(id), m_varargs(varargs) { std::swap(m_arguments, arguments); }

    const std::string &name() const { return m_name; }
    const TypeName &returnType() const { return m_type; }
    const std::vector<NFunctionArgumentDeclaration> &arguments() const { return m_arguments; }
    bool isVarargs() const { return m_varargs; }

    llvm::Value *codeGen(CodeGenContext &context) override;

private:
    TypeName m_type;
    std::string m_name;
    std::vector<NFunctionArgumentDeclaration> m_arguments;
    bool m_varargs;
};

class NExternVariableDeclaration : public NStatement {
public:
    NExternVariableDeclaration(const Token &tok, const std::string &name, const TypeName &type) : NStatement(tok), m_name(name), m_type(type) {}

    const std::string &name() const { return m_name; }
    const TypeName &type() const { return m_type; }

    llvm::Value *codeGen(CodeGenContext &context) override;

private:
    std::string m_name;
    TypeName m_type;
};

class NFunctionDeclaration : public NStatement {
public:
    TypeName type;
    std::string id;
    std::vector<NFunctionArgumentDeclaration> arguments;
    NBlock *block;
    NFunctionDeclaration(const std::string &id, const TypeName &type,
                         const std::vector<NFunctionArgumentDeclaration> &arguments, NBlock *block) :
                            type(type), id(id), arguments(arguments), block(block) { }

    llvm::Value *codeGen(CodeGenContext &context) override;
};

class NStructDeclaration : public NStatement {
public:
    std::string id;
    std::vector<NVariableDeclaration> elements;
    NStructDeclaration(const std::string &id, const std::vector<NVariableDeclaration> &elements) : id(id), elements(elements) {}

    llvm::Value *codeGen(CodeGenContext& context) override;
};

class NIfaceDeclaration: public NStatement {
public:
    NIfaceDeclaration(const std::string &name, std::vector<TypeName> &par, NIfacePrototypeList &pro) : name(name) { std::swap(parameters, par); std::swap(prototypes, pro); }
    llvm::Value *codeGen(CodeGenContext& context) override;

    std::string name;
    std::vector<TypeName> parameters;
    NIfacePrototypeList prototypes;
    std::vector<NImplDeclaration *> implementations;
};

class NIfacePrototype : public NStatement {
public:
    NIfacePrototype(const std::string &name, std::vector<TypeName> &par) : name(name) { std::swap(parameters, par); }

    std::string name;
    std::vector<TypeName> parameters;
    NIfaceDeclaration *iface;
};

class NImplDeclaration : public NStatement
{
public:
    NImplDeclaration(const std::string &name, std::vector<TypeName> &par, FuncDeclarationList &funcs) : name(name) { std::swap(parameters, par); std::swap(functions, funcs); }

    llvm::Value *codeGen(CodeGenContext& context) override;

    std::string name;
    std::string id;
    std::vector<TypeName> parameters;
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

    llvm::Value *codeGen(CodeGenContext& context) override;

private:
    std::unique_ptr<NExpression> m_condition;
    NBlock *m_block;
    NBlock *m_elseBlock;
};

#endif
