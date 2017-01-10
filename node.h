#include <iostream>
#include <vector>
#include <llvm/IR/Value.h>

class CodeGenContext;
class NStatement;
class NExpression;
class NVariableDeclaration;
class NAssignment;

typedef std::vector<NStatement*> StatementList;
typedef std::vector<NExpression*> ExpressionList;
typedef std::vector<NVariableDeclaration*> VariableList;
typedef std::vector<NAssignment *> AssignmentList;

class Node {
public:
	virtual ~Node() {}
	virtual llvm::Value* codeGen(CodeGenContext& context) { return NULL; }
};

class NExpression : public Node {
};

class NStatement : public Node {
};

class NInteger : public NExpression {
public:
	long long value;
	NInteger(long long value) : value(value) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NDouble : public NExpression {
public:
	double value;
	NDouble(double value) : value(value) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NString : public NExpression {
public:
    std::string value;
    NString(std::string *val) : value(*val) {}

    virtual llvm::Value* codeGen(CodeGenContext& context) override;
};

class NIdentifier : public NExpression {
public:
        const NIdentifier *parent;
        union {
            std::string name;
            int index;
        };
        enum {
            Name,
            Index
        } type;
	NIdentifier(const std::string& name) : parent(nullptr), name(name), type(Name) { }
	NIdentifier(const NIdentifier *p, const std::string &name) : parent(p), name(name), type(Name) {}
	NIdentifier(const NIdentifier *p, int index) : parent(p), index(index), type(Index) {}
	~NIdentifier() {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NMethodCall : public NExpression {
public:
	const NIdentifier& id;
	AssignmentList arguments;
	NMethodCall(const NIdentifier& id, AssignmentList& arguments) :
		id(id), arguments(arguments) { }
	NMethodCall(const NIdentifier& id) : id(id) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NBinaryOperator : public NExpression {
public:
	int op;
	NExpression& lhs;
	NExpression& rhs;
	NBinaryOperator(NExpression& lhs, int op, NExpression& rhs) :
		lhs(lhs), rhs(rhs), op(op) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NAssignment : public NExpression {
public:
	const NIdentifier& lhs;
	NExpression& rhs;
        llvm::Value *rhsValue;
	NAssignment(const NIdentifier& lhs, NExpression& rhs) :
		lhs(lhs), rhs(rhs), rhsValue(nullptr) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);

        llvm::Value *genRhs(CodeGenContext &context);
};

class NBlock : public NExpression {
public:
	StatementList statements;
	NBlock() { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NExpressionStatement : public NStatement {
public:
	NExpression& expression;
	NExpressionStatement(NExpression& expression) : 
		expression(expression) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NReturnStatement : public NStatement {
public:
	NExpression& expression;
	NReturnStatement(NExpression& expression) : 
		expression(expression) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NVariableDeclaration : public NStatement {
public:
	const NIdentifier *type;
	NIdentifier& id;
        AssignmentList expressions;
	NVariableDeclaration(const NIdentifier *type, NIdentifier& id) :
		type(type), id(id) { }
	NVariableDeclaration(const NIdentifier *type, NIdentifier& id, NAssignment *assignmentExpr) :
		type(type), id(id), expressions({ assignmentExpr }) { }
        NVariableDeclaration(const NIdentifier *type, NIdentifier& id, AssignmentList exprs) :
		type(type), id(id), expressions(exprs) { }
        NVariableDeclaration(NIdentifier &id, NAssignment *expr) : type(nullptr), id(id), expressions({ expr }) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NExternDeclaration : public NStatement {
public:
    const NIdentifier& type;
    const NIdentifier& id;
    VariableList arguments;
    bool varargs;
    NExternDeclaration(const NIdentifier& type, const NIdentifier& id,
            const VariableList& arguments, bool varargs) :
        type(type), id(id), arguments(arguments), varargs(varargs) {}
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NFunctionDeclaration : public NStatement {
public:
	const NIdentifier& type;
	const NIdentifier& id;
	VariableList arguments;
	NBlock& block;
	NFunctionDeclaration(const NIdentifier& type, const NIdentifier& id, 
			const VariableList& arguments, NBlock& block) :
		type(type), id(id), arguments(arguments), block(block) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NStructDeclaration : public NStatement {
public:
    const NIdentifier &id;
    VariableList elements;
    NStructDeclaration(const NIdentifier &id, const VariableList &elements) : id(id), elements(elements) {}

    llvm::Value* codeGen(CodeGenContext& context) override;
};

class NTuple : public NExpression {
public:
    NTuple() {}
    void add(NExpression *expr) { expressions.push_back(expr); }

    llvm::Value *codeGen(CodeGenContext& context) override;

    std::vector<NExpression *> expressions;
};
