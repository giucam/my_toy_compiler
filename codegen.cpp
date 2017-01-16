#include <fstream>

#include "llvm/Support/raw_os_ostream.h"

#include "node.h"
#include "codegen.h"
// #include "parser.hpp"
#include "common.h"

using namespace std;

/* Compile the AST into a module */
void CodeGenContext::generateCode(NBlock& root)
{
	std::cout << "Generating code...\n";
	
	/* Create the top level interpreter function to call as entry */
	vector<Type*> argTypes;
	BasicBlock *bblock = BasicBlock::Create(TheContext, "entry", 0, 0);
	
	/* Push a new variable/block context */
	pushBlock(bblock, nullptr, nullptr);
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

StructType *tupleType(const std::vector<Type *> &argTypes, CodeGenContext &context)
{
    std::string id("tupletype");
    for (auto &&type: argTypes) {
        id += "_" + typeName(type);
    }
    if (auto type = context.tupleTypes[id]) {
        return type;
    }

    StructType *type = StructType::create(context.TheContext, argTypes, id);
    context.tupleTypes[id] = type;
    return type;
}

Value *makeTupleValue(const std::vector<Value *> &values, const std::string &name, CodeGenContext &context)
{
    std::vector<Type *> argTypes;
    argTypes.reserve(values.size());
    for (auto &&value: values) {
        auto t = value->getType();
        argTypes.push_back(t);
    }

    StructType *type = tupleType(argTypes, context);
    Tuple &tuple = context.tuples[type];
    tuple.type = type;

    if (argTypes.empty()) {
        return ConstantStruct::get(type, {});
    }

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
        } else if (type.name() == "i8") {
            return Type::getInt8Ty(context.TheContext);
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
        } else if (type.name()[0] == '(') {
            auto name = type.name();
            std::vector<Type *> types;
            int start = 1;
            int end = 1;
            while (start < name.size()) {
                end = name.find(',', end);
                if (end == std::string::npos) {
                    if (start == 1) {
                        break;
                    } else {
                        end = name.size() - 2;
                    }
                }
                types.push_back(typeOf(TypeName(type.token(), name.substr(start, end)), context));
            }
            return tupleType(types, context);
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

        bool isLiteral = !t->isPointerTy();
        if (!isLiteral) {
            t = t->getPointerElementType();
        }
        while (t->isPointerTy()) {
            parentV = new LoadInst(parentV, "", false, currentBlock()->block);
            t = parentV->getType()->getPointerElementType();
        }

        t->dump();
        if (structsByType.find(t) != structsByType.end()) {
            StructType *st = static_cast<StructType *>(t);
            Struct &str = structs[st->getName()];

            int id = -1;
            if (ident.type == NIdentifier::Name) {
                for (size_t i = 0; i < str.fields.size(); ++i) {
                    if (str.fields[i] == ident.name) {
                        id = i;
                        break;
                    }
                }
                if (id == -1) {
                    err(ident.token(), "struct '{}' has no field named '{}'", st->getName().str(), ident.name);
                }
            } else {
                if (ident.index >= (int)str.fields.size()) {
                    err(ident.token(), "invalid index '{}' when accessing struct with {} elements", ident.index, str.fields.size());
                }
                id = ident.index;
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

            if (isLiteral) {
                return ExtractValueInst::Create(parentV, { (unsigned int)ident.index }, "", currentBlock()->block);
            } else {
                auto id1 = ConstantInt::get(TheContext, llvm::APInt(32, 0, false));
                auto id2 = ConstantInt::get(TheContext, llvm::APInt(32, ident.index, false));

                return GetElementPtrInst::CreateInBounds(parentV, {id1, id2}, "", currentBlock()->block);
            }
        } else {
            err(ident.token(), "no such field in value");
        }
    }

    auto valueInLocals = [&](CodeGenBlock *block) -> Value * {
        auto it = block->locals.find(ident.name);
        if (it == block->locals.end()) {
            return nullptr;
        }
        return it->second;
    };

    auto block = currentBlock();
    do {
        auto val = valueInLocals(block);
        if (val) {
            return val;
        }
        block = block->parent;
    } while (block);

    if (auto global = module->getGlobalVariable(ident.name)) {
        return global;
    }

    err(ident.token(), "'{}' was not declared in this scope", ident.name);
    return nullptr;
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

std::vector<NExpression *> NIdentifier::unpack(CodeGenContext &context)
{
    auto *val = codeGen(context);
    auto type = val->getType();
    while (type->isPointerTy()) {
        type = type->getPointerElementType();
    }
    if (context.tuples.find(type) != context.tuples.end()) {
        StructType *st = static_cast<StructType *>(type);
        std::vector<NExpression *> expressions;

        for (int i = 0; i < st->elements().size(); ++i) {
            auto expr = new NIdentifier(token(), i);
            expr->pushContext(this);
            expressions.push_back(expr);
        }
        return expressions;
    }

    return { this };
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
    for (auto &&expr: m_list) {
        auto value = expr->load(context);
        values.push_back(value);
    }
    return new LoadInst(makeTupleValue(values, "tuple", context), "", false, context.currentBlock()->block);
}


std::vector<NExpression *> NExpressionPack::unpack(CodeGenContext &context)
{
    std::vector<NExpression *> vec;
    for (auto &&ex: m_list) {
        auto v = ex->unpack(context);
        vec.insert(vec.end(), v.begin(), v.end());
    }
    return vec;
}

static Function *makeConcreteFunction(CodeGenContext &context, NFunctionDeclaration *func, std::vector<Value *> &values)
{
    fmt::print("TEMPLATE {}\n",func->id);
    std::vector<Type *> argTypes;
    values.reserve(func->arguments.size());
    auto valueIt = values.begin();

    auto templateId = std::string(func->id);
    for (auto it = func->arguments.begin(); it != func->arguments.end(); ++it, ++valueIt) {
        Type *valueType = nullptr;
        if (it->type().name() == "(...)") {
            std::vector<Value *> vec;
            for (auto it = valueIt; it < values.end(); ++it) {
                auto val = *it;
                while (val->getType()->isPointerTy()) {
                val->dump();
                val = new LoadInst(val, "", false, context.currentBlock()->block);
                }
                vec.push_back(val);
            }
//             vec.insert(vec.end(), valueIt, values.end());
            auto tuple = makeTupleValue(vec, "tuple", context);
            *valueIt = tuple;
            auto next = valueIt + 1;
            values.erase(next, values.end());

            valueType = tuple->getType();
        } else {
            valueType = typeOf(it->type(), context);
        }

        valueType->dump();
        argTypes.push_back(valueType);
        templateId += typeName(valueType);
    }

    if (auto func = context.concreteTemplates[templateId]) {
        return func;
    }

    FunctionType *ftype = FunctionType::get(typeOf(func->type, context), makeArrayRef(argTypes), false);
    Function *function = Function::Create(ftype, GlobalValue::ExternalLinkage, templateId.c_str(), context.module);
    BasicBlock *bblock = BasicBlock::Create(context.TheContext, "entry", function, 0);

    context.concreteTemplates[templateId] = function;

    context.pushBlock(bblock, function, nullptr);

    Function::arg_iterator argsValues = function->arg_begin();

    auto typeIt = argTypes.begin();
    for (auto it = func->arguments.begin(); it != func->arguments.end(); ++it, ++typeIt) {
        auto argumentValue = &*argsValues++;
        argumentValue->setName(it->name().c_str());

        fmt::print("TEMPLATE ARG {}\n",typeName(*typeIt));

//         it->codeGen(context);
        AllocaInst *alloc = new AllocaInst(*typeIt, it->name().c_str(), context.currentBlock()->block);
        context.locals()[it->name()] = alloc;

        StoreInst *inst = new StoreInst(argumentValue, context.locals()[it->name()], false, bblock);
    }

    auto &data = context.functions[function];
    data.block = context.currentBlock();
    data.hasTupleArg = false;
    for (auto it = func->arguments.begin(); it != func->arguments.end(); it++) {
        auto &&name = it->name();
        data.argumentNames.push_back(name);
    }

    func->block->codeGen(context);
    ReturnInst::Create(context.TheContext, context.getCurrentReturnValue(), context.currentBlock()->block);

    context.popBlock();

    return function;
}

static Value *loadValue(Value *value, CodeGenContext &context)
{
    if (value->getType()->isPointerTy()) {
        return new LoadInst(value, "", false, context.currentBlock()->block);
    }
    return value;
}

static Function *findInterfaceImpl(const Token &tok, const std::string &name, const std::vector<Value *> &values, CodeGenContext &context)
{
    auto it = context.ifacePrototypes.find(name);
    if (it == context.ifacePrototypes.end()) {
        return nullptr;
    }

    auto proto = it->second;
    auto iface = proto->iface;

    std::cout<<"in iface "<<iface->name<<"\n";

    std::vector<Type *> toMatch;
    toMatch.resize(iface->parameters.size());

    int numPars = proto->parameters.size();
//             int numPars = std::min(proto->parameters.size(), argExpr.size());
    for (int i = 0; i < numPars; ++i) {
//                 auto v = values[i]; //argExpr[i]->codeGen(context);
        auto v = values.size() > i ? values[i] : nullptr;
        auto t = v ? v->getType() : (i == 0 ? tupleType({}, context) : nullptr);
        if (!t) {
            break;
        }
//                 values.push_back(v);
//         if (t->isPointerTy()) {
//             t = t->getPointerElementType();
//         }

        auto par = proto->parameters[i];
        std::cout<<"    ";t->dump(); std::cout<<" -> "<<par.name()<<"\n";
        for (size_t i = 0; i < iface->parameters.size(); ++i) {
            if (iface->parameters[i].name() == par.name()) {
                toMatch[i] = t;
            }
        }
    }

    while (true) {
        for (auto &&impl: iface->implementations) {
            if (impl->parameterTypes == toMatch) {
                auto n = mangleFuncName(name, iface->name, impl->id);
                return context.module->getFunction(n.c_str());
            }
        }
        bool changed = false;
        for (auto &t: toMatch) {
            if (t->isPointerTy()) {
                t = t->getPointerElementType();
                changed = true;
                break;
            }
        }
        if (!changed) {
            break;
        }
    }

    err(tok, "could not find suitable implementation for the '{}' interface, when calling '{}'", iface->name, name);
    return nullptr;
}

Value *NMethodCall::codeGen(CodeGenContext& context)
{
    std::cout << "Creating method call: " << name << endl;

    std::vector<NExpression *> argExpr;

    if (this->context()) {
        auto vec = this->context()->unpack(context);
        argExpr.insert(argExpr.end(), vec.begin(), vec.end());
    }

    for (auto it = arguments.begin(); it != arguments.end(); it++) {
        auto vec = (*it)->unpack(context);
        argExpr.insert(argExpr.end(), vec.begin(), vec.end());
    }

    std::vector<Value *> values;
    values.reserve(argExpr.size());

    for (auto &&expr: argExpr) {
        values.push_back(expr->codeGen(context));
    }

    if (name == "count") {
        return ConstantInt::get(Type::getInt32Ty(context.TheContext), values.size(), true);
    }

    Function *function = context.module->getFunction(name.c_str());

    if (!function) {
        function = findInterfaceImpl(token(), name, values, context);
    }

    if (!function) {
        auto it = context.functionTemplates.find(name);
        if (it != context.functionTemplates.end()) {
            function = makeConcreteFunction(context, it->second, values);
        }
    }

    if (function == NULL) {
        error("no such function '{}'", name);
    }

    size_t i = 0;
//     auto &&funcData = context.functions[function];
    for (auto &&arg: function->getArgumentList()) {
//         bool lastArg = (i == function->getArgumentList().size() - 1) && !function->isVarArg() && funcData.hasTupleArg;
//
//         if (lastArg) {
//             std::vector<Value *> vec;
//             for (; i < argExpr.size(); ++i) {
//                 vec.push_back(argExpr[i++]->load(context));
//             }
//             auto tuple = makeTupleValue(vec, context);
//             values.push_back(CastInst::CreatePointerCast(tuple, Type::getInt8PtrTy(context.TheContext), "", context.currentBlock()->block));
//
//             auto type = tuple->getType();
//
//             CastInst::CreatePointerCast(&arg, type, "", funcData.block->block);
//
//             break;
//         }

//         if (values.size() <= i) {
//             values.push_back(argExpr[i]->codeGen(context));
//         }
        auto ty = values[i]->getType();
        auto argTy = arg.getType();

        fmt::print("ARG {} {}\n",i,typeName(ty));

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
            values[i] = loadValue(values[i], context);
//             values.push_back(argExpr[i]->load(context));
        }
    } else {
        size_t numArgs = i;
        if (numArgs < values.size()) {
            err(argExpr[numArgs]->token(), "too many arguments to function");
        }
    }

    CallInst *call = CallInst::Create(function, makeArrayRef(values), "", context.currentBlock()->block);
    return call;
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
//     if (!rhsValue) {
        rhsValue = rhs->load(context);
//     }
    return rhsValue;
}

Value* NBlock::codeGen(CodeGenContext& context)
{
	StatementList::const_iterator it;
	Value *last = NULL;
	for (it = statements.begin(); it != statements.end(); it++) {
		std::cout << "block: Generating code for " << typeid(**it).name() << endl;
		last = (**it).codeGen(context);
                if (context.currentBlock()->returned) {
                    break;
                }
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
    Value *returnValue = expression ? expression->load(context) : nullptr;

//     if (returnValue->getType()->isPointerTy()) {
//         returnValue = new LoadInst(returnValue, "", false, context.currentBlock()->block);
//     }

    context.setCurrentReturnValue(returnValue);
    context.currentBlock()->returned = true;
    return ReturnInst::Create(context.TheContext, returnValue, context.currentBlock()->block);


    return returnValue;
}

static Value *allocStoreVariable(CodeGenContext &ctx, Value *value, const std::string &name)
{
    AllocaInst *alloc = new AllocaInst(value->getType(), name.c_str(), ctx.currentBlock()->block);
    ctx.locals()[name] = alloc;

    return new StoreInst(value, alloc, false, ctx.currentBlock()->block);
}

Value* NVariableDeclaration::codeGen(CodeGenContext& context)
{
    const auto numExpressions = expressions.size();
    std::cout << "Creating variable declaration " << id.name() << endl;

    if (!type.valid()) {
        if (numExpressions == 0) {
            err(token(), "missing type or initializer when declaring variable '{}'", id.name());
        }

        auto value = expressions.front()->genRhs(context);
        return allocStoreVariable(context, value, id.name());
    }

    auto t = typeOf(type, context);
    AllocaInst *alloc = new AllocaInst(t, id.name().c_str(), context.currentBlock()->block);
    context.locals()[id.name()] = alloc;

    if (t->isStructTy() && context.structsByType.find(t) != context.structsByType.end()) {
        Struct &str = context.structs[type.name()];
        if (numExpressions != str.fields.size()) {
            err(token(), "wrong number of initializers passed when declaring variable '{}' of type '{}'", id.name(), type.name());
        }

        NIdentifier ident(token(), id.name());
        for (size_t i = 0; i < expressions.size(); ++i) {
            expressions[i]->lhs->pushContext(&ident);
            expressions[i]->codeGen(context);
        }
    } else if (numExpressions == 1) {
        expressions.front()->codeGen(context);
    } else if (numExpressions != 0) {
        err(token(), "wrong number of initializers passed when declaring variable '{}' of type '{}'", id.name(), type.name());
    }
    return alloc;
}

Value *NMultiVariableDeclaration::codeGen(CodeGenContext &context)
{
    fmt::print("\n\nMULTI\n\n");
    auto expressions = expression()->unpack(context);
    auto expression = [&](const Token &tok, size_t i) -> NExpression * {
        if (expressions.size() <= i) {
            return nullptr;
        }
        return expressions[i];
    };

    size_t i = 0;
    for (auto &&name: names()) {
        auto expr = expression(name.token(), i);
        bool lastName = i == names().size() - 1;

        fmt::print("MUL {} {} {}\n",i,names().size(), expressions.size());

        Value *value;
        if (expr) {
            if (lastName && expressions.size() > i + 1) {
                std::vector<Value *> values;
                for (size_t j = i; j < expressions.size(); ++j) {
                    values.push_back(expressions[j]->load(context));
                }
                value = makeTupleValue(values, name.name(), context);
            } else {
                value = expr->load(context);

                AllocaInst *alloc = new AllocaInst(value->getType(), name.name().c_str(), context.currentBlock()->block);
                new StoreInst(value, alloc, false, context.currentBlock()->block);
                value = alloc;
            }
        } else {
            value = makeTupleValue({}, name.name(), context);
        }

        fmt::print("NEW MULTI {}\n",name.name());
        value->getType()->dump();

        context.locals()[name.name()] = value;
        ++i;
    }
    return 0;
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
    data.hasTupleArg = false;
    for (auto &&arg: arguments()) {
        data.argumentNames.push_back(arg.name());
    }

    return function;
}

Value* NFunctionDeclaration::codeGen(CodeGenContext& context)
{
    if (context.module->getFunction(id.c_str()) || context.functionTemplates.find(id) != context.functionTemplates.end()) {
        std::cerr << "error: function '" << id.c_str() << "' is already declared.\n";
        exit(1);
    }

	vector<Type*> argTypes;
	for (auto it = arguments.begin(); it != arguments.end(); it++) {
            if (it->type().name() == "(...)") {
                context.functionTemplates[id] = this;
                return nullptr;
            }
            argTypes.push_back(typeOf(it->type(), context));
	}
	auto retType = typeOf(type, context);
	FunctionType *ftype = FunctionType::get(retType, makeArrayRef(argTypes), false);
	Function *function = Function::Create(ftype, GlobalValue::ExternalLinkage, id.c_str(), context.module);
	BasicBlock *bblock = BasicBlock::Create(context.TheContext, "entry", function, 0);

	context.pushBlock(bblock, function, nullptr);

	Function::arg_iterator argsValues = function->arg_begin();
    Value* argumentValue;

	for (auto it = arguments.begin(); it != arguments.end(); it++) {
		argumentValue = &*argsValues++;
		argumentValue->setName(it->name().c_str());

                it->codeGen(context);
		StoreInst *inst = new StoreInst(argumentValue, context.locals()[it->name()], false, bblock);
	}

        auto &data = context.functions[function];
        data.block = context.currentBlock();
        data.hasTupleArg = false;
        for (auto it = arguments.begin(); it != arguments.end(); it++) {
            auto &&name = it->name();
            data.argumentNames.push_back(name);
        }
	
	block->codeGen(context);
//         if (
	ReturnInst::Create(context.TheContext, context.getCurrentReturnValue(), context.currentBlock()->block);

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
        std::cout<<"    with arg " << it->type.name() << " " <<it->id.name()<<"\n";
    }
    StructType *type = StructType::create(context.TheContext, argTypes, id.c_str());
    Struct &str = context.structs[id];
    str.type = type;
    str.fields.reserve(elements.size());
    context.structsByType[type] = &str;
    for (auto &&el: elements) {
        str.fields.push_back(el.id.name());
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

Value *NIfStatement::codeGen(CodeGenContext &ctx)
{
    auto cond = condition()->codeGen(ctx);
    cond->dump();

    if (cond->getType()->isPointerTy()) {
        cond = new LoadInst(cond, "", false, ctx.currentBlock()->block);
        auto t = static_cast<PointerType *>(cond->getType());
        cond = new ICmpInst(*ctx.currentBlock()->block, CmpInst::ICMP_NE, cond, ConstantPointerNull::get(t));
    }

    auto curBlock = ctx.currentBlock();
    BasicBlock *ifblock = BasicBlock::Create(ctx.TheContext, "if", curBlock->function, 0);
    BasicBlock *elseblock = elseBlock() ? BasicBlock::Create(ctx.TheContext, "else", curBlock->function, 0) : nullptr;
    BasicBlock *afterblock = BasicBlock::Create(ctx.TheContext, "endif", curBlock->function, 0);

    ctx.pushBlock(ifblock, curBlock->function, curBlock);
    block()->codeGen(ctx);
    if (!ctx.currentBlock()->returned) {
        BranchInst::Create(afterblock, ctx.currentBlock()->block);
    }
    ctx.popBlock();

    if (elseBlock()) {
        ctx.pushBlock(elseblock, curBlock->function, curBlock);
        elseBlock()->codeGen(ctx);
        if (!ctx.currentBlock()->returned) {
            BranchInst::Create(afterblock, ctx.currentBlock()->block);
        }
        ctx.popBlock();
    }

    auto branch = BranchInst::Create(ifblock, elseblock ? elseblock : afterblock, cond, ctx.currentBlock()->block);

    ctx.popBlock();
    ctx.pushBlock(afterblock, curBlock->function, curBlock);

    return branch;
}

Value *NExternVariableDeclaration::codeGen(CodeGenContext &ctx)
{
    auto ty = typeOf(type(), ctx);
    auto val = new GlobalVariable(*ctx.module, ty, false, GlobalValue::ExternalLinkage, nullptr, name().c_str());
    return val;
}
