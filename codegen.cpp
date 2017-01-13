#include <fstream>

#include "llvm/Support/raw_os_ostream.h"

#include "node.h"
#include "codegen.h"
// #include "parser.hpp"
#include "common.h"

using namespace std;

template<class... Args>
void err(const Token &tok, Args &&... args)
{
    Err(tok.filename(), tok.lineNo(), tok.columnNo()).line(std::forward<Args>(args)...).line("{}", tok.line()).line(std::string(std::max(tok.columnNo() - 1, 0), ' ') + '^');
}

/* Compile the AST into a module */
void CodeGenContext::generateCode(NBlock& root)
{
	std::cout << "Generating code...\n";
	
	/* Create the top level interpreter function to call as entry */
	vector<Type*> argTypes;
	BasicBlock *bblock = BasicBlock::Create(TheContext, "entry", 0, 0);
	
	/* Push a new variable/block context */
	pushBlock(bblock);
	root.codeGen(*this); /* emit bytecode for the toplevel block */
	ReturnInst::Create(TheContext, bblock);
	popBlock();

        mainFunction = module->getFunction("main");
	
	/* Print the bytecode in a human-readable format 
	   to see if our program compiled properly
	 */
	std::cout << "Code is generated.\n";
	PassManager<Module> pm;
        AnalysisManager<Module>* am = new AnalysisManager<Module>;
	pm.addPass(PrintModulePass(outs()));
	pm.run(*module, *am);
}

void CodeGenContext::writeOutput(const std::string &filename)
{
    std::ofstream out(filename);
    raw_os_ostream stream(out);
    module->print(stream, nullptr);
}

/* Executes the AST by running the main function */
GenericValue CodeGenContext::runCode() {
	std::cout << "Running code...\n";
	ExecutionEngine *ee = EngineBuilder( unique_ptr<Module>(module) ).create();
	ee->finalizeObject();
	vector<GenericValue> noargs;
	GenericValue v = ee->runFunction(mainFunction, noargs);
	std::cout << "Code was run.\n";
	return v;
}

/* Returns an LLVM type based on the identifier */
static Type *typeOf(const TypeName &type, CodeGenContext& context)
{
    Type *basicType = [&]() -> Type * {
        if (type.name().compare("i32") == 0) {
            return Type::getInt32Ty(context.TheContext);
        } else if (type.name() == "u32") {
            return Type::getInt32Ty(context.TheContext);
        } else if (type.name().compare("f64") == 0) {
            return Type::getDoubleTy(context.TheContext);
        } else if (type.name().compare("void") == 0) {
            return Type::getVoidTy(context.TheContext);
        } else if (type.name().compare("string") == 0) {
            return Type::getInt8PtrTy(context.TheContext);
        } else if (context.structs.find(type.name()) != context.structs.end()) {
            return context.structs[type.name()].type;
        }
        err(type.token(), "type '{}' was not declared in this scope", type.name());
        return nullptr; //silence the warning
    }();

    for (int pointer = type.pointer(); pointer > 0; --pointer) {
        basicType = basicType->getPointerTo();
    }
    return basicType;
}

Value *CodeGenContext::findValue(const NIdentifier &ident)
{
//     std::cout<<"FIND "<<ident.name<<" "<<ident.context()<<"\n";
//     if (parent) {
    if (ident.context()) {
        Value *parentV = ident.context()->codeGen(*this);
//         Value *parentV = findValue(*parent, parent->parent);
        Type *t = parentV->getType();

        parentV->dump();
        t->dump();

        t = t->getPointerElementType();
        while (t->isPointerTy()) {
            parentV = new LoadInst(parentV, "", false, currentBlock()->block);
            t = parentV->getType()->getPointerElementType();
        }

        t->dump();
        if (structsByType.find(t) != structsByType.end()) {
            StructType *st = static_cast<StructType *>(t);
            Struct &str = structs[st->getName()];

            int id = -1;
            for (size_t i = 0; i < str.fields.size(); ++i) {
                if (str.fields[i] == ident.name) {
                    id = i;
                    break;
                }
            }
            if (id == -1) {
                err(ident.token(), "struct '{}' has no field named '{}'", st->getName().str(), ident.name);
            }

            auto id1 = ConstantInt::get(TheContext, llvm::APInt(32, 0, false));
            auto id2 = ConstantInt::get(TheContext, llvm::APInt(32, id, false));

            return GetElementPtrInst::CreateInBounds(parentV, {id1, id2}, "", currentBlock()->block);
        } else if (tuples.find(t) != tuples.end()) {
            if (ident.type != NIdentifier::Index) {
                err(ident.token(), "tuple elements can only be accessed by index");
                exit(1);
            }
            StructType *st = static_cast<StructType *>(t);
            if (ident.index < 0 || ident.index >= (int)st->elements().size()) {
                err(ident.token(), "invalid index '{}' when accessing tuple with {} elements", ident.index, st->elements().size());
            }
            auto id1 = ConstantInt::get(TheContext, llvm::APInt(32, 0, false));
            auto id2 = ConstantInt::get(TheContext, llvm::APInt(32, ident.index, false));

            return GetElementPtrInst::CreateInBounds(parentV, {id1, id2}, "", currentBlock()->block);
        }
    }

    auto it = locals().find(ident.name);
    if (it == locals().end()) {
        err(ident.token(), "'{}' was not declared in this scope", ident.name);
    }
    return it->second;
}

/* -- Code Generation -- */

Value* NInteger::codeGen(CodeGenContext& context)
{
	std::cout << "Creating integer: " << value << endl;
	return ConstantInt::get(Type::getInt32Ty(context.TheContext), value, true);
}

Value* NDouble::codeGen(CodeGenContext& context)
{
	std::cout << "Creating double: " << value << endl;
	return ConstantFP::get(Type::getDoubleTy(context.TheContext), value);
}

Value* NString::codeGen(CodeGenContext& context)
{
    std::cout << "Creating string: \"" << value << "\", (";
    for (auto it = value.begin(); it != value.end();) {
        char c = *it;
        if (c == '\n') std::cout << "\\n";
        else std::cout << c;
        if (++it != value.end()) {
            std::cout << ", ";
        } else {
            break;
        }
    }
    std::cout << ")\n";
    auto constant = ConstantDataArray::getString(context.TheContext, value);
    auto var = new GlobalVariable(*context.module, ArrayType::get(Type::getInt8Ty(context.TheContext), value.length() + 1), true, llvm::GlobalValue::PrivateLinkage, constant, ".str");

    auto id1 = ConstantInt::get(context.TheContext, llvm::APInt(32, 0, false));
    auto id2 = ConstantInt::get(context.TheContext, llvm::APInt(32, 0, false));

    return GetElementPtrInst::CreateInBounds(var, {id1, id2}, "", context.currentBlock()->block);
}

Value* NIdentifier::codeGen(CodeGenContext& context)
{
    return context.findValue(*this);
//     return new LoadInst(context.findValue(*this), "", false, context.currentBlock()->block);
}

Value* NIdentifier::load(CodeGenContext& context)
{
//     return context.findValue(*this);
    return new LoadInst(context.findValue(*this), "", false, context.currentBlock()->block);
}

Value *NAddressOfExpression::codeGen(CodeGenContext& context)
{
    return expression()->codeGen(context);
}

static std::string mangleFuncName(const std::string &name, const std::string &iface, const std::string &sig)
{
    return iface + sig + "::" + name;
}

Value *NExpressionPack::codeGen(CodeGenContext& context)
{
    std::cout << "Creating tuple declaration " << m_list.size()<<"\n";

    std::vector<Value *> values;
    std::vector<Type *> argTypes;
    for (auto &&expr: m_list) {
        auto value = expr->codeGen(context);
        values.push_back(value);
        argTypes.push_back(value->getType());
    }

    StructType *type = StructType::create(context.TheContext, argTypes, "tuple");
    Tuple &tuple = context.tuples[type];
    tuple.type = type;

    AllocaInst *alloc = new AllocaInst(type, "tuple", context.currentBlock()->block);

    int id = 0;
    for (auto &&v: values) {
        auto id1 = ConstantInt::get(context.TheContext, llvm::APInt(32, 0, false));
        auto id2 = ConstantInt::get(context.TheContext, llvm::APInt(32, id++, false));

        auto value = GetElementPtrInst::CreateInBounds(alloc, {id1, id2}, "", context.currentBlock()->block);
        new StoreInst(v, value, false, context.currentBlock()->block);
    }

    return new LoadInst(alloc, "", false, context.currentBlock()->block);
    return alloc;
}


std::vector<NExpression *> NExpressionPack::unpack()
{
    std::vector<NExpression *> vec;
    for (auto &&ex: m_list) {
        auto v = ex->unpack();
        vec.insert(vec.end(), v.begin(), v.end());
    }
    return vec;
}

static std::string typeName(Type *ty)
{
    std::string str;
    while (ty->isPointerTy()) {
        str += '*';
        ty = ty->getPointerElementType();
    }
    if (ty->isStructTy()) {
        auto st = static_cast<StructType *>(ty);
        str += st->getName();
    } else {
        raw_string_ostream stream(str);
        ty->print(stream);
        stream.flush();
    }
    return str;
}

Value *NMethodCall::codeGen(CodeGenContext& context)
{
    std::cout << "Creating method call: " << name << endl;

    std::vector<NExpression *> argExpr;

    if (this->context()) {
        auto vec = this->context()->unpack();
        argExpr.insert(argExpr.end(), vec.begin(), vec.end());
    }

    for (auto it = arguments.begin(); it != arguments.end(); it++) {
        auto vec = (*it)->unpack();
        argExpr.insert(argExpr.end(), vec.begin(), vec.end());
    }

    std::vector<Value *> values;
    values.reserve(argExpr.size());
    Function *function = context.module->getFunction(name.c_str());

    if (!function) {
        auto it = context.ifacePrototypes.find(name);
        if (it != context.ifacePrototypes.end()) {
            auto proto = it->second;
            auto iface = proto->iface;

            std::cout<<"in iface "<<iface->name<<"\n";

            std::vector<Type *> toMatch;
            toMatch.resize(iface->parameters.size());

            int numPars = std::min(proto->parameters.size(), argExpr.size());
            for (int i = 0; i < numPars; ++i) {
                auto v = argExpr[i]->codeGen(context);
                values.push_back(v);
                auto t = v->getType();
                while (t->isPointerTy()) {
                    t = t->getPointerElementType();
                }

                auto par = proto->parameters[i];
                std::cout<<"    ";v->getType()->dump(); std::cout<<" -> "<<par.name()<<"\n";
                for (size_t i = 0; i < iface->parameters.size(); ++i) {
                    if (iface->parameters[i].name() == par.name()) {
                        toMatch[i] = t;
                    }
                }
            }

            NImplDeclaration *implementation = nullptr;
            for (auto &&impl: iface->implementations) {
                if (impl->parameterTypes == toMatch) {
                    implementation = impl;
                    break;
                }
            }
            if (!implementation) {
                err(token(), "could not find suitable implementation for the '{}' interface, when calling '{}'", iface->name, name);
            }

            std::cout<<"found impl\n";

            auto name = mangleFuncName(this->name, iface->name, implementation->id);
            function = context.module->getFunction(name.c_str());
        }
    }

    if (function == NULL) {
        error("no such function '{}'", name);
    }

    size_t i = 0;
    for (auto &&arg: function->getArgumentList()) {
        if (values.size() <= i) {
            values.push_back(argExpr[i]->codeGen(context));
        }
        auto ty = values[i]->getType();
        auto argTy = arg.getType();

        auto pointerLevel = [](Type *t) {
            int p = 0;
            while (t->isPointerTy()) {
                t = t->getPointerElementType();
                ++p;
            }
            return p;
        };

        int argPl = pointerLevel(argTy);

        while (pointerLevel(ty) > argPl) {
            values[i] = new LoadInst(values[i], "", false, context.currentBlock()->block);
            ty = values[i]->getType();
        }

        if (ty != argTy) {
            err(argExpr[i]->token(), "wrong argument type to function; expected '{}', found '{}'", typeName(argTy), typeName(ty));
        }
        i++;
    }
    if (function->isVarArg()) {
        for (; i < argExpr.size(); ++i) {
            // varargs, we need to do a load here
            values.push_back(argExpr[i]->load(context));
        }
    } else {
        size_t numArgs = i;
        if (numArgs < argExpr.size()) {
            err(argExpr[numArgs]->token(), "too many arguments to function");
        }
    }

    CallInst *call = CallInst::Create(function, makeArrayRef(values), "", context.currentBlock()->block);
    return call;
}

Value *NBinaryOperator::codeGen(CodeGenContext& context)
{
    std::cout << "Creating binary operation " << (int)op << endl;
    auto instructionOp = [&](Type *type) -> int {
        if (type->isIntegerTy()) {
            switch (op) {
                case OP::Add: return Instruction::Add;
                case OP::Mul: return Instruction::Mul;
                case OP::Sub: return Instruction::Sub;
                case OP::Div: return Instruction::SDiv;
            }
        } else {
            switch (op) {
                case OP::Add: return Instruction::FAdd;
                case OP::Mul: return Instruction::FMul;
                case OP::Sub: return Instruction::FSub;
                case OP::Div: return Instruction::FDiv;
            }
        }
        return -1;
    };

    auto rhsExprs = rhs->unpack();
    auto lhsExprs = lhs->unpack();
    if (rhsExprs.size() != lhsExprs.size()) {
        error("both operands must have the same cardinality");
    }
    assert(rhsExprs.size() == 1);
    for (size_t i = 0; i < rhsExprs.size(); ++i) {
        auto lhsValue = lhsExprs[i]->load(context);
        auto rhsValue = rhsExprs[i]->load(context);
        auto lhst = lhsValue->getType();
        auto rhst = rhsValue->getType();
        if (lhst != rhst) {
            err(token(), "mismatched types in binary operation");
        }
        auto instr = instructionOp(lhst);
        if (instr == -1) {
            err(token(), "invalid operands for binary expression");
        }
        return BinaryOperator::Create((Instruction::BinaryOps)instr, lhsValue, rhsValue, "", context.currentBlock()->block);
    }
    return nullptr;
}

Value* NAssignment::codeGen(CodeGenContext& context)
{
//     std::cout << "Creating assignment for " << lhs->name << " "<<this<<endl;

    auto lhsValue = lhs->codeGen(context);
    auto rhsValue = genRhs(context);
//     if (rhsValue->getType()->isPointerTy()) {
//         rhsValue = new LoadInst(rhsValue, "", false, context.currentBlock()->block);
//     }

    return new StoreInst(rhsValue, lhsValue, false, context.currentBlock()->block);
}

Value *NAssignment::genRhs(CodeGenContext &context)
{
    if (!rhsValue) {
        rhsValue = rhs->load(context);
    }
    return rhsValue;
}

Value* NBlock::codeGen(CodeGenContext& context)
{
	StatementList::const_iterator it;
	Value *last = NULL;
	for (it = statements.begin(); it != statements.end(); it++) {
		std::cout << "block: Generating code for " << typeid(**it).name() << endl;
		last = (**it).codeGen(context);
	}
	std::cout << "Creating block" << endl;
	return last;
}

Value* NExpressionStatement::codeGen(CodeGenContext& context)
{
	std::cout << "Generating code for " << typeid(expression).name() << endl;
	return expression->codeGen(context);
}

Value* NReturnStatement::codeGen(CodeGenContext& context)
{
    std::cout << "Generating return code for " << typeid(expression).name() << endl;
    Value *returnValue = expression->load(context);

//     if (returnValue->getType()->isPointerTy()) {
//         returnValue = new LoadInst(returnValue, "", false, context.currentBlock()->block);
//     }

    context.setCurrentReturnValue(returnValue);

    return returnValue;
}

Value* NVariableDeclaration::codeGen(CodeGenContext& context)
{
    const auto numExpressions = expressions.size();
    std::cout << "Creating variable declaration " << id << endl;

    Type *t = [&]() {
        if (!type.valid()) {
            if (numExpressions == 0) {
                err(token(), "missing type or initializer when declaring variable '{}'", id);
                exit(1);
            }

            auto value = expressions.front()->genRhs(context);
            return value->getType();
        }
        return typeOf(type, context);
    }();

    AllocaInst *alloc = new AllocaInst(t, id.c_str(), context.currentBlock()->block);
    context.locals()[id] = alloc;

    if (t->isStructTy() && context.structsByType.find(t) != context.structsByType.end()) {
        Struct &str = context.structs[type.name()];
        if (numExpressions != str.fields.size()) {
            err(token(), "wrong number of initializers passed when declaring variable '{}' of type '{}'", id, type.name());
        }

        NIdentifier ident(token(), id);
        for (size_t i = 0; i < expressions.size(); ++i) {
            expressions[i]->lhs->pushContext(&ident);
            expressions[i]->codeGen(context);
        }
    } else if (numExpressions == 1) {
        expressions.front()->codeGen(context);
    } else if (numExpressions != 0) {
        err(token(), "wrong number of initializers passed when declaring variable '{}' of type '{}'", id, type.name());
    }
    return alloc;
}

Value *NFunctionArgumentDeclaration::codeGen(CodeGenContext &context)
{
    AllocaInst *alloc = new AllocaInst(typeOf(type(), context), name().c_str(), context.currentBlock()->block);
    context.locals()[name()] = alloc;
    return alloc;
}

Value *NExternDeclaration::codeGen(CodeGenContext &context)
{
    std::vector<Type *> argTypes;
    argTypes.reserve(arguments().size());
    for (auto &&arg: arguments()) {
        argTypes.push_back(typeOf(arg.type(), context));
    }
    FunctionType *ftype = FunctionType::get(typeOf(returnType(), context), makeArrayRef(argTypes), isVarargs());
    Function *function = Function::Create(ftype, GlobalValue::ExternalLinkage, name().c_str(), context.module);

    auto &data = context.functions[function];
    for (auto &&arg: arguments()) {
        data.argumentNames.push_back(arg.name());
    }

    return function;
}

Value* NFunctionDeclaration::codeGen(CodeGenContext& context)
{
    if (context.module->getFunction(id.c_str())) {
        std::cerr << "error: function '" << id.c_str() << "' is already declared.\n";
        exit(1);
    }

	vector<Type*> argTypes;
	for (auto it = arguments.begin(); it != arguments.end(); it++) {
            argTypes.push_back(typeOf(it->type(), context));
	}
	FunctionType *ftype = FunctionType::get(typeOf(type, context), makeArrayRef(argTypes), false);
	Function *function = Function::Create(ftype, GlobalValue::ExternalLinkage, id.c_str(), context.module);
	BasicBlock *bblock = BasicBlock::Create(context.TheContext, "entry", function, 0);

	context.pushBlock(bblock);

	Function::arg_iterator argsValues = function->arg_begin();
    Value* argumentValue;

	for (auto it = arguments.begin(); it != arguments.end(); it++) {
		argumentValue = &*argsValues++;
		argumentValue->setName(it->name().c_str());

                it->codeGen(context);
		StoreInst *inst = new StoreInst(argumentValue, context.locals()[it->name()], false, bblock);
	}

        auto &data = context.functions[function];
        for (auto it = arguments.begin(); it != arguments.end(); it++) {
            auto &&name = it->name();
            data.argumentNames.push_back(name);
        }
	
	block->codeGen(context);
	ReturnInst::Create(context.TheContext, context.getCurrentReturnValue(), bblock);

	context.popBlock();
	std::cout << "Creating function: " << id << endl;
	return function;
}

Value* NStructDeclaration::codeGen(CodeGenContext& context)
{
    std::cout << "Creating struct declaration " << " " << id << endl;
    vector<Type *> argTypes;
    for (auto it = elements.begin(); it != elements.end(); it++) {
        argTypes.push_back(typeOf(it->type, context));
        std::cout<<"    with arg " << it->type.name() << " " <<it->id<<"\n";
    }
    StructType *type = StructType::create(context.TheContext, argTypes, id.c_str());
    Struct &str = context.structs[id];
    str.type = type;
    str.fields.reserve(elements.size());
    context.structsByType[type] = &str;
    for (auto &&el: elements) {
        str.fields.push_back(el.id);
    }
    return 0;
}

Value *NTuple::codeGen(CodeGenContext& context)
{
    auto name = std::string("tuple") + std::to_string(context.tuples.size());
    std::cout << "Creating tuple declaration " << name << expressions.size()<<"\n";

    std::vector<Value *> values;
    std::vector<Type *> argTypes;
    for (auto &&expr: expressions) {
        auto value = expr->codeGen(context);
        values.push_back(value);
        argTypes.push_back(value->getType());
    }

    StructType *type = StructType::create(context.TheContext, argTypes, name.c_str());
    Tuple &tuple = context.tuples[type];
    tuple.type = type;

    AllocaInst *alloc = new AllocaInst(type, name.c_str(), context.currentBlock()->block);

    int id = 0;
    for (auto &&v: values) {
        auto id1 = ConstantInt::get(context.TheContext, llvm::APInt(32, 0, false));
        auto id2 = ConstantInt::get(context.TheContext, llvm::APInt(32, id++, false));

        auto value = GetElementPtrInst::CreateInBounds(alloc, {id1, id2}, "", context.currentBlock()->block);
        new StoreInst(v, value, false, context.currentBlock()->block);
    }

//     return new LoadInst(alloc, "", false, context.currentBlock()->block);
    return alloc;
}

Value *NIfaceDeclaration::codeGen(CodeGenContext &context)
{
    context.interfaces[name] = this;
    if (prototypes.size() == 0) {
        error("cannot create an empty interface");
    }
    for (auto &&p: prototypes) {
        p->iface = this;
        context.ifacePrototypes[p->name] = p;
    }

    return 0;
}

Value *NImplDeclaration::codeGen(CodeGenContext &context)
{
    auto it = context.interfaces.find(name);
    if (it == context.interfaces.end()) {
        error("trying to implement unknown interface '{}'", name);
    }

    NIfaceDeclaration *iface = it->second;

    for (auto &&p: iface->prototypes) {
        bool hasProto = false;
        for (auto &&f: functions) {
            if (f->id == p->name) {
                hasProto = true;
                break;
            }
        }
        if (!hasProto) {
            error("missing implementation in interface '{}' for function '{}'", name, p->name);
        }
    }
    for (auto &&f: functions) {
        bool hasProto = false;
        for (auto &&p: iface->prototypes) {
            if (f->id == p->name) {
                hasProto = true;
                break;
            }
        }
        if (!hasProto) {
            error("unrelated function '{}' in implementation of interface '{}'", f->id, name);
        }
    }

    iface->implementations.push_back(this);

    raw_string_ostream stream(id);
    for (auto &&par: parameters) {
        auto t = typeOf(TypeName(par.token(), par.name()), context);
        t->print(stream);
        parameterTypes.push_back(t);
    }
    stream.flush();

    for (auto &&func: functions) {
        func->id = mangleFuncName(func->id, iface->name, id);
        func->codeGen(context);
    }

    return 0;
}
