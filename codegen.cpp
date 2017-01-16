#include <fstream>

#include "llvm/Support/raw_os_ostream.h"

#include "node.h"
#include "codegen.h"
// #include "parser.hpp"
#include "common.h"

using namespace llvm;

CodeGenContext::CodeGenContext()
              : m_module(std::make_unique<llvm::Module>("main", m_context))
{
}

/* Compile the AST into a module */
void CodeGenContext::generateCode(NBlock &root)
{
    std::cout << "Generating code...\n";

    /* Create the top level interpreter function to call as entry */
    BasicBlock *bblock = BasicBlock::Create(context(), "entry", 0, 0);

    /* Push a new variable/block context */
    pushBlock(bblock, nullptr, nullptr);
    root.codeGen(*this); /* emit bytecode for the toplevel block */
    ReturnInst::Create(m_context, bblock);
    popBlock();

    m_mainFunction = m_module->getFunction("main");

    /* Print the bytecode in a human-readable format
        to see if our program compiled properly
        */
    std::cout << "Code is generated.\n";
    PassManager<Module> pm;
    AnalysisManager<Module>* am = new AnalysisManager<Module>;
    pm.addPass(PrintModulePass(outs()));
    pm.run(*m_module, *am);
}

void CodeGenContext::writeOutput(const std::string &filename)
{
    std::ofstream out(filename);
    raw_os_ostream stream(out);
    m_module->print(stream, nullptr);
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

bool CodeGenContext::isFunctionNameAvailable(const std::string &name) const
{
    return !module().getFunction(name.c_str()) && (m_functionTemplates.find(name) == m_functionTemplates.end());
}

StructType *CodeGenContext::tupleType(const std::vector<Type *> &argTypes)
{
    std::string id("tupletype");
    for (auto &&type: argTypes) {
        id += "_" + typeName(type);
    }
    if (auto type = m_tupleTypes[id]) {
        return type;
    }

    StructType *type = StructType::create(context(), argTypes, id);
    m_tupleTypes[id] = type;
    return type;
}

Value *CodeGenContext::makeTupleValue(const std::vector<Value *> &values, const std::string &name)
{
    std::vector<Type *> argTypes;
    argTypes.reserve(values.size());
    for (auto &&value: values) {
        auto t = value->getType();
        argTypes.push_back(t);
    }

    StructType *type = tupleType(argTypes);
    TupleInfo &tuple = m_tupleInfo[type];
    tuple.type = type;

    if (argTypes.empty()) {
        return ConstantStruct::get(type, {});
    }

    AllocaInst *alloc = new AllocaInst(type, name.c_str(), currentBlock()->block);

    int id = 0;
    for (auto &&v: values) {
        auto id1 = ConstantInt::get(context(), llvm::APInt(32, 0, false));
        auto id2 = ConstantInt::get(context(), llvm::APInt(32, id++, false));

        auto value = GetElementPtrInst::CreateInBounds(alloc, {id1, id2}, "", currentBlock()->block);
        new StoreInst(v, value, false, currentBlock()->block);
    }

//     return new LoadInst(alloc, "", false, context.currentBlock()->block);
    return alloc;
}

/* Returns an LLVM type based on the identifier */
llvm::Type *CodeGenContext::typeOf(const TypeName &type)
{
    Type *basicType = [&]() -> Type * {
        if (type.name().compare("i32") == 0) {
            return Type::getInt32Ty(context());
        } else if (type.name() == "i8") {
            return Type::getInt8Ty(context());
        } else if (type.name() == "u32") {
            return Type::getInt32Ty(context());
        } else if (type.name().compare("f64") == 0) {
            return Type::getDoubleTy(context());
        } else if (type.name().compare("void") == 0) {
            return Type::getVoidTy(context());
        } else if (type.name().compare("string") == 0) {
            return Type::getInt8PtrTy(context());
        } else if (m_structInfo.find(type.name()) != m_structInfo.end()) {
            return m_structInfo[type.name()].type;
        } else if (type.name()[0] == '(') {
            auto name = type.name();
            std::vector<Type *> types;
            size_t start = 1;
            size_t end = 1;
            while (start < name.size()) {
                end = name.find(',', end);
                if (end == std::string::npos) {
                    if (start == 1) {
                        break;
                    } else {
                        end = name.size() - 2;
                    }
                }
                types.push_back(typeOf(TypeName(type.token(), name.substr(start, end))));
            }
            return tupleType(types);
        }
        err(type.token(), "type '{}' was not declared in this scope", type.name());
        return nullptr; //silence the warning
    }();

    for (int pointer = type.pointer(); pointer > 0; --pointer) {
        basicType = basicType->getPointerTo();
    }
    return basicType;
}

const StructInfo *CodeGenContext::structInfo(llvm::Type *type) const
{
    if (!type->isStructTy()) {
        return nullptr;
    }

    auto it = m_structInfoByType.find(type);
    if (it != m_structInfoByType.end()) {
        return it->second;
    }
    return nullptr;
}

StructInfo *CodeGenContext::newStructType(llvm::StructType *type)
{
    auto info = &m_structInfo[type->getName()];
    m_structInfoByType[type] = info;
    return info;
}

const TupleInfo *CodeGenContext::tupleInfo(llvm::Type *type) const
{
    if (!type->isStructTy()) {
        return nullptr;
    }

    auto it = m_tupleInfo.find(type);
    if (it != m_tupleInfo.end()) {
        return &it->second;
    }
    return nullptr;
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
        if (auto info = structInfo(t)) {
            StructType *st = static_cast<StructType *>(t);

            int id = -1;
            if (ident.type == NIdentifier::Name) {
                for (size_t i = 0; i < info->fields.size(); ++i) {
                    if (info->fields[i] == ident.name) {
                        id = i;
                        break;
                    }
                }
                if (id == -1) {
                    err(ident.token(), "struct '{}' has no field named '{}'", st->getName().str(), ident.name);
                }
            } else {
                if (ident.index >= (int)info->fields.size()) {
                    err(ident.token(), "invalid index '{}' when accessing struct with {} elements", ident.index, info->fields.size());
                }
                id = ident.index;
            }

            auto id1 = ConstantInt::get(context(), llvm::APInt(32, 0, false));
            auto id2 = ConstantInt::get(context(), llvm::APInt(32, id, false));

            return GetElementPtrInst::CreateInBounds(parentV, {id1, id2}, "", currentBlock()->block);
        } else if (tupleInfo(t)) {
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
                auto id1 = ConstantInt::get(context(), llvm::APInt(32, 0, false));
                auto id2 = ConstantInt::get(context(), llvm::APInt(32, ident.index, false));

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

    if (auto global = m_module->getGlobalVariable(ident.name)) {
        return global;
    }

    err(ident.token(), "'{}' was not declared in this scope", ident.name);
    return nullptr;
}

NIfacePrototype *CodeGenContext::ifacePrototype(const std::string &name) const
{
    auto it = m_ifacePrototypes.find(name);
    if (it != m_ifacePrototypes.end()) {
        return it->second;
    }
    return nullptr;
}

NIfaceDeclaration *CodeGenContext::interface(const std::string &name) const
{
    auto it = m_interfaces.find(name);
    if (it == m_interfaces.end()) {
        return nullptr;
    }
    return it->second;
}

void CodeGenContext::addInterface(NIfaceDeclaration *iface)
{
    m_interfaces[iface->name] = iface;
    for (auto &&proto: iface->prototypes) {
        m_ifacePrototypes[proto->name] = proto;
    }
}

llvm::Function *CodeGenContext::functionTemplate(const std::string &name, std::vector<llvm::Value *> &values)
{
    auto it = m_functionTemplates.find(name);
    if (it != m_functionTemplates.end()) {
        return makeConcreteFunction(it->second, values);
    }
    return nullptr;
}

void CodeGenContext::addFunctionTemplate(NFunctionDeclaration *func)
{
    m_functionTemplates[func->id] = func;
}

void CodeGenContext::storeLocal(const std::string &name, llvm::Value *value)
{
    currentBlock()->locals[name] = value;
}

llvm::Value *CodeGenContext::local(const std::string &name) const
{
    auto it = currentBlock()->locals.find(name);
    if (it != currentBlock()->locals.end()) {
        return it->second;
    }
    return nullptr;
}

void CodeGenContext::pushBlock(llvm::BasicBlock *block, llvm::Function *function, CodeGenBlock *parent)
{
    m_blocks.push({block, function, parent, {}, false });
}

void CodeGenContext::popBlock()
{
    m_blocks.pop();
}

/* -- Code Generation -- */

Value* NInteger::codeGen(CodeGenContext& context)
{
	std::cout << "Creating integer: " << value << '\n';
	return ConstantInt::get(Type::getInt32Ty(context.context()), value, true);
}

Value* NDouble::codeGen(CodeGenContext& context)
{
	std::cout << "Creating double: " << value << '\n';
	return ConstantFP::get(Type::getDoubleTy(context.context()), value);
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
    auto constant = ConstantDataArray::getString(context.context(), value);
    auto var = new GlobalVariable(context.module(), ArrayType::get(Type::getInt8Ty(context.context()), value.length() + 1), true, llvm::GlobalValue::PrivateLinkage, constant, ".str");

    auto id1 = ConstantInt::get(context.context(), llvm::APInt(32, 0, false));
    auto id2 = ConstantInt::get(context.context(), llvm::APInt(32, 0, false));

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
    if (context.tupleInfo(type)) {
        StructType *st = static_cast<StructType *>(type);
        std::vector<NExpression *> expressions;

        for (size_t i = 0; i < st->elements().size(); ++i) {
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
    return new LoadInst(context.makeTupleValue(values, "tuple"), "", false, context.currentBlock()->block);
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

Function *CodeGenContext::makeConcreteFunction(NFunctionDeclaration *func, std::vector<Value *> &values)
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
                val = new LoadInst(val, "", false, currentBlock()->block);
                }
                vec.push_back(val);
            }
//             vec.insert(vec.end(), valueIt, values.end());
            auto tuple = makeTupleValue(vec, "tuple");
            *valueIt = tuple;
            auto next = valueIt + 1;
            values.erase(next, values.end());

            valueType = tuple->getType();
        } else {
            valueType = typeOf(it->type());
        }

        valueType->dump();
        argTypes.push_back(valueType);
        templateId += typeName(valueType);
    }

    if (auto func = m_concreteTemplates[templateId]) {
        return func;
    }

    FunctionType *ftype = FunctionType::get(typeOf(func->type), makeArrayRef(argTypes), false);
    Function *function = Function::Create(ftype, GlobalValue::ExternalLinkage, templateId.c_str(), &module());
    BasicBlock *bblock = BasicBlock::Create(context(), "entry", function, 0);

    m_concreteTemplates[templateId] = function;

    pushBlock(bblock, function, nullptr);

    Function::arg_iterator argsValues = function->arg_begin();

    auto typeIt = argTypes.begin();
    for (auto it = func->arguments.begin(); it != func->arguments.end(); ++it, ++typeIt) {
        auto argumentValue = &*argsValues++;
        argumentValue->setName(it->name().c_str());

        fmt::print("TEMPLATE ARG {}\n",typeName(*typeIt));

//         it->codeGen(context);
        AllocaInst *alloc = new AllocaInst(*typeIt, it->name().c_str(), currentBlock()->block);
        storeLocal(it->name(), alloc);

        new StoreInst(argumentValue, local(it->name()), false, bblock);
    }

    func->block->codeGen(*this);
    if (!currentBlock()->returned) {
        ReturnInst::Create(context(), nullptr, currentBlock()->block);
    }

    popBlock();

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
    auto proto = context.ifacePrototype(name);
    if (!proto) {
        return nullptr;
    }
    auto iface = proto->iface;

    std::cout<<"in iface "<<iface->name<<"\n";

    std::vector<Type *> toMatch;
    toMatch.resize(iface->parameters.size());

    size_t numPars = proto->parameters.size();
//             int numPars = std::min(proto->parameters.size(), argExpr.size());
    for (size_t i = 0; i < numPars; ++i) {
//                 auto v = values[i]; //argExpr[i]->codeGen(context);
        auto v = values.size() > i ? values[i] : nullptr;
        auto t = v ? v->getType() : (i == 0 ? context.tupleType({}) : nullptr);
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
                return context.module().getFunction(n.c_str());
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
    std::cout << "Creating method call: " << name << '\n';

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
        return ConstantInt::get(Type::getInt32Ty(context.context()), values.size(), true);
    }

    Function *function = context.module().getFunction(name.c_str());

    if (!function) {
        function = findInterfaceImpl(token(), name, values, context);
    }

    if (!function) {
        function = context.functionTemplate(name, values);
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
//             values.push_back(CastInst::CreatePointerCast(tuple, Type::getInt8PtrTy(context.context()), "", context.currentBlock()->block));
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
//     std::cout << "Creating assignment for " << lhs->name << " "<<this<<'\n';

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
		std::cout << "block: Generating code for " << typeid(**it).name() << '\n';
		last = (**it).codeGen(context);
                if (context.currentBlock()->returned) {
                    break;
                }
	}
	std::cout << "Creating block" << '\n';
	return last;
}

Value* NExpressionStatement::codeGen(CodeGenContext& context)
{
	std::cout << "Generating code for " << typeid(expression).name() << '\n';
	return expression->codeGen(context);
}

Value* NReturnStatement::codeGen(CodeGenContext& context)
{
    Value *returnValue = expression ? expression->load(context) : nullptr;

    context.currentBlock()->returned = true;
    return ReturnInst::Create(context.context(), returnValue, context.currentBlock()->block);
}

static Value *allocStoreVariable(CodeGenContext &ctx, Value *value, const std::string &name)
{
    AllocaInst *alloc = new AllocaInst(value->getType(), name.c_str(), ctx.currentBlock()->block);
    ctx.storeLocal(name, alloc);

    return new StoreInst(value, alloc, false, ctx.currentBlock()->block);
}

Value* NVariableDeclaration::codeGen(CodeGenContext& context)
{
    const auto numExpressions = expressions.size();
    std::cout << "Creating variable declaration " << id.name() << '\n';

    if (!type.valid()) {
        if (numExpressions == 0) {
            err(token(), "missing type or initializer when declaring variable '{}'", id.name());
        }

        auto value = expressions.front()->genRhs(context);
        return allocStoreVariable(context, value, id.name());
    }

    auto t = context.typeOf(type);
    AllocaInst *alloc = new AllocaInst(t, id.name().c_str(), context.currentBlock()->block);
    context.storeLocal(id.name(), alloc);

    if (auto info = context.structInfo(t)) {
        if (numExpressions != info->fields.size()) {
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
                value = context.makeTupleValue(values, name.name());
            } else {
                value = expr->load(context);

                AllocaInst *alloc = new AllocaInst(value->getType(), name.name().c_str(), context.currentBlock()->block);
                new StoreInst(value, alloc, false, context.currentBlock()->block);
                value = alloc;
            }
        } else {
            value = context.makeTupleValue({}, name.name());
        }

        fmt::print("NEW MULTI {}\n",name.name());
        value->getType()->dump();

        context.storeLocal(name.name(), value);
        ++i;
    }
    return 0;
}

Value *NFunctionArgumentDeclaration::codeGen(CodeGenContext &context)
{
    AllocaInst *alloc = new AllocaInst(context.typeOf(type()), name().c_str(), context.currentBlock()->block);
    context.storeLocal(name(), alloc);
    return alloc;
}

Value *NExternDeclaration::codeGen(CodeGenContext &context)
{
    std::vector<Type *> argTypes;
    argTypes.reserve(arguments().size());
    for (auto &&arg: arguments()) {
        argTypes.push_back(context.typeOf(arg.type()));
    }
    FunctionType *ftype = FunctionType::get(context.typeOf(returnType()), makeArrayRef(argTypes), isVarargs());
    Function *function = Function::Create(ftype, GlobalValue::ExternalLinkage, name().c_str(), &context.module());

    return function;
}

Value* NFunctionDeclaration::codeGen(CodeGenContext& context)
{
    if (!context.isFunctionNameAvailable(id)) {
        err(token(), "function '{}' is already declared", id);
    }

    std::vector<Type *> argTypes;
    for (auto it = arguments.begin(); it != arguments.end(); it++) {
        if (it->type().name() == "(...)") {
            context.addFunctionTemplate(this);
            return nullptr;
        }
        argTypes.push_back(context.typeOf(it->type()));
    }
    auto retType = context.typeOf(type);
    FunctionType *ftype = FunctionType::get(retType, makeArrayRef(argTypes), false);
    Function *function = Function::Create(ftype, GlobalValue::ExternalLinkage, id.c_str(), &context.module());
    BasicBlock *bblock = BasicBlock::Create(context.context(), "entry", function, 0);

    context.pushBlock(bblock, function, nullptr);

    Function::arg_iterator argsValues = function->arg_begin();
    Value* argumentValue;

	for (auto it = arguments.begin(); it != arguments.end(); it++) {
		argumentValue = &*argsValues++;
		argumentValue->setName(it->name().c_str());

                it->codeGen(context);
		new StoreInst(argumentValue, context.local(it->name()), false, bblock);
	}

	block->codeGen(context);
        if (!context.currentBlock()->returned) {
            ReturnInst::Create(context.context(), nullptr, context.currentBlock()->block);
        }

	context.popBlock();
	std::cout << "Creating function: " << id << '\n';
	return function;
}

Value* NStructDeclaration::codeGen(CodeGenContext& context)
{
    std::cout << "Creating struct declaration " << " " << id << '\n';
    std::vector<Type *> argTypes;
    for (auto it = elements.begin(); it != elements.end(); it++) {
        argTypes.push_back(context.typeOf(it->type));
        std::cout<<"    with arg " << it->type.name() << " " <<it->id.name()<<"\n";
    }
    StructType *type = StructType::create(context.context(), argTypes, id.c_str());
    auto info = context.newStructType(type);
    info->type = type;
    info->fields.reserve(elements.size());
    for (auto &&el: elements) {
        info->fields.push_back(el.id.name());
    }
    return 0;
}

Value *NIfaceDeclaration::codeGen(CodeGenContext &context)
{
    context.addInterface(this);
    if (prototypes.size() == 0) {
        error("cannot create an empty interface");
    }
    for (auto &&p: prototypes) {
        p->iface = this;
    }

    return 0;
}

Value *NImplDeclaration::codeGen(CodeGenContext &context)
{
    NIfaceDeclaration *iface = context.interface(name);
    if (!iface) {
        error("trying to implement unknown interface '{}'", name);
    }

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
        auto t = context.typeOf(TypeName(par.token(), par.name()));
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
    BasicBlock *ifblock = BasicBlock::Create(ctx.context(), "if", curBlock->function, 0);
    BasicBlock *elseblock = elseBlock() ? BasicBlock::Create(ctx.context(), "else", curBlock->function, 0) : nullptr;
    BasicBlock *afterblock = BasicBlock::Create(ctx.context(), "endif", curBlock->function, 0);

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

    ctx.currentBlock()->block = afterblock;

    return branch;
}

Value *NExternVariableDeclaration::codeGen(CodeGenContext &ctx)
{
    auto ty = ctx.typeOf(type());
    auto val = new GlobalVariable(ctx.module(), ty, false, GlobalValue::ExternalLinkage, nullptr, name().c_str());
    return val;
}
