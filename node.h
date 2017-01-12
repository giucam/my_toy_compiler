
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
class NIfaceParameter;
class NIfacePrototype;
class NFunctionDeclaration;
class NImplDeclaration;

typedef std::vector<NStatement*> StatementList;
typedef std::vector<std::unique_ptr<NExpression>> ExpressionList;
typedef std::vector<NVariableDeclaration *> VariableList;
typedef std::vector<NAssignment *> AssignmentList;
typedef std::vector<NIfaceParameter> NIfaceParameterList;
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
    TypeName(const Token &tok, const std::string &name) : m_token(tok), m_name(name) {}

    const Token &token() const { return m_token; }
    bool valid() const { return !m_name.empty(); }
    const std::string &name() const { return m_name; }
private:
    Token m_token;
    std::string m_name;
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
    virtual std::vector<llvm::Value *> unpack(CodeGenContext &context) { return { load(context) }; }

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
	NInteger(long long value) : NExpression(), value(value) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NDouble : public NExpression {
public:
	double value;
	NDouble(double value) : NExpression(), value(value) { }
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
    std::vector<llvm::Value *> unpack(CodeGenContext &context) override;

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

class NVariableDeclaration : public NStatement {
public:
    TypeName type;
    std::string id;
    AssignmentList expressions;
    NVariableDeclaration(const Token &tok, const std::string &name, const TypeName &type) : NStatement(tok), type(type), id(name) { }
    NVariableDeclaration(const Token &tok, const std::string &name, const TypeName &type, NAssignment *assignmentExpr) : NStatement(tok), type(type), id(name), expressions({ assignmentExpr }) { }
    NVariableDeclaration(const Token &tok, const std::string &name, const TypeName &type, AssignmentList exprs) : NStatement(tok), type(type), id(name), expressions(exprs) { }
    NVariableDeclaration(const Token &tok, const std::string &name, NAssignment *expr) : NStatement(tok), id(name), expressions({ expr }) {}

    llvm::Value *codeGen(CodeGenContext& context) override;
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

class NTuple : public NExpression {
public:
    NTuple(): NExpression() {}
    void add(NExpression *expr) { expressions.push_back(expr); }

    llvm::Value *codeGen(CodeGenContext& context) override;

    std::vector<NExpression *> expressions;
};

class NIfaceDeclaration: public NStatement {
public:
    NIfaceDeclaration(const std::string &name, NIfaceParameterList &par, NIfacePrototypeList &pro) : name(name) { std::swap(parameters, par); std::swap(prototypes, pro); }
    llvm::Value *codeGen(CodeGenContext& context) override;

    std::string name;
    NIfaceParameterList parameters;
    NIfacePrototypeList prototypes;
    std::vector<NImplDeclaration *> implementations;
};

class NIfaceParameter {
public:
    NIfaceParameter(const Token &tok, const std::string &name) : m_token(tok), m_name(name) {}

    const Token &token() const { return m_token; }
    const std::string &name() const { return m_name; }

private:
    Token m_token;
    std::string m_name;
};

class NIfacePrototype : public NStatement {
public:
    NIfacePrototype(const std::string &name, NIfaceParameterList &par) : name(name) { std::swap(parameters, par); }

    std::string name;
    NIfaceParameterList parameters;
    NIfaceDeclaration *iface;
};

class NImplDeclaration : public NStatement
{
public:
    NImplDeclaration(const std::string &name, NIfaceParameterList &par, FuncDeclarationList &funcs) : name(name) { std::swap(parameters, par); std::swap(functions, funcs); }

    llvm::Value *codeGen(CodeGenContext& context) override;

    std::string name;
    std::string id;
    NIfaceParameterList parameters;
    FuncDeclarationList functions;
    std::vector<llvm::Type *> parameterTypes;
};

#endif
