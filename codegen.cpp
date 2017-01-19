#include <fstream>

#include "llvm/Support/raw_os_ostream.h"

#include "node.h"
#include "codegen.h"
// #include "parser.hpp"
#include "common.h"

CodeGenContext::CodeGenContext()
              : m_module(std::make_unique<llvm::Module>("main", m_context))
{
}

/* Compile the AST into a module */
void CodeGenContext::generateCode(NBlock &root)
{
    std::cout << "Generating code...\n";

    /* Create the top level interpreter function to call as entry */
    llvm::BasicBlock *bblock = llvm::BasicBlock::Create(context(), "entry", 0, 0);

    /* Push a new variable/block context */
    pushBlock(bblock, nullptr, nullptr);
    root.codeGen(*this); /* emit bytecode for the toplevel block */
    llvm::ReturnInst::Create(m_context, bblock);
    popBlock();

    m_mainFunction = m_module->getFunction("main");

    /* Print the bytecode in a human-readable format
        to see if our program compiled properly
        */
    std::cout << "Code is generated.\n";
    llvm::PassManager<llvm::Module> pm;
    llvm::AnalysisManager<llvm::Module>* am = new llvm::AnalysisManager<llvm::Module>;
    pm.addPass(llvm::PrintModulePass(llvm::outs()));
    pm.run(*m_module, *am);
}

void CodeGenContext::writeOutput(const std::string &filename)
{
    std::ofstream out(filename);
    llvm::raw_os_ostream stream(out);
    m_module->print(stream, nullptr);
}

std::string typeName(llvm::Type *ty)
{
    std::string str;
    while (ty->isPointerTy()) {
        str += '*';
        ty = ty->getPointerElementType();
    }
    if (ty->isStructTy()) {
        auto st = static_cast<llvm::StructType *>(ty);
        str += st->getName();
    } else {
        llvm::raw_string_ostream stream(str);
        ty->print(stream);
        stream.flush();
    }
    return str;
}

bool CodeGenContext::isFunctionNameAvailable(const std::string &name) const
{
    return !module().getFunction(name.c_str()) && (m_functionTemplates.find(name) == m_functionTemplates.end());
}

llvm::StructType *CodeGenContext::tupleType(const std::vector<llvm::Type *> &argTypes)
{
    std::string id("tupletype");
    for (auto &&type: argTypes) {
        id += "_" + typeName(type);
    }
    if (auto type = m_tupleTypes[id]) {
        return type;
    }

    llvm::StructType *type = llvm::StructType::create(context(), argTypes, id);
    m_tupleTypes[id] = type;
    return type;
}

llvm::Value *CodeGenContext::makeTupleValue(const std::vector<llvm::Value *> &values, const std::string &name)
{
    std::vector<llvm::Type *> argTypes;
    argTypes.reserve(values.size());
    for (auto &&value: values) {
        auto t = value->getType();
        argTypes.push_back(t);
    }

    llvm::StructType *type = tupleType(argTypes);
    TupleInfo &tuple = m_tupleInfo[type];
    tuple.type = type;

    if (argTypes.empty()) {
        return llvm::ConstantStruct::get(type, {});
    }

    llvm::AllocaInst *alloc = new llvm::AllocaInst(type, name.c_str(), currentBlock()->block);

    int id = 0;
    for (auto &&v: values) {
        auto id1 = llvm::ConstantInt::get(context(), llvm::APInt(32, 0, false));
        auto id2 = llvm::ConstantInt::get(context(), llvm::APInt(32, id++, false));

        auto value = llvm::GetElementPtrInst::CreateInBounds(alloc, {id1, id2}, "", currentBlock()->block);
        new llvm::StoreInst(v, value, false, currentBlock()->block);
    }

//     return new LoadInst(alloc, "", false, context.currentBlock()->block);
    return alloc;
}

/* Returns an LLVM type based on the identifier */
llvm::Type *CodeGenContext::typeOf(const TypeName &type)
{
    llvm::Type *basicType = [&]() -> llvm::Type * {
        if (type.name().compare("i32") == 0) {
            return llvm::Type::getInt32Ty(context());
        } else if (type.name() == "i8") {
            return llvm::Type::getInt8Ty(context());
        } else if (type.name() == "u32") {
            return llvm::Type::getInt32Ty(context());
        } else if (type.name().compare("f64") == 0) {
            return llvm::Type::getDoubleTy(context());
        } else if (type.name().compare("void") == 0) {
            return llvm::Type::getVoidTy(context());
        } else if (type.name().compare("string") == 0) {
            return llvm::Type::getInt8PtrTy(context());
        } else if (m_structInfo.find(type.name()) != m_structInfo.end()) {
            return m_structInfo[type.name()].type;
        } else if (type.name()[0] == '(') {
            auto name = type.name();
            std::vector<llvm::Type *> types;
            size_t start = 1;
            size_t end = 1;
            while (start < name.size()) {
                if (name[start] == ' ') {
                    start++;
                    continue;
                }
                end = name.find(',', start);
                if (end == std::string::npos) {
                    if (start == 1) {
                        break;
                    } else {
                        end = name.size() - 1;
                    }
                }
                auto t = name.substr(start, end - start);
                start = end + 1;
                types.push_back(typeOf(TypeName(type.token(), t)));
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

llvm::Function *CodeGenContext::functionTemplate(const std::string &name, std::vector<Value::V> &values)
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

void CodeGenContext::storeLocal(const std::string &name, Value value)
{
    currentBlock()->locals[name] = value;
}

Optional<Value> CodeGenContext::local(const std::string &name) const
{
    auto valueInLocals = [&](const CodeGenBlock *block) -> Optional<Value> {
        auto it = block->locals.find(name);
        if (it == block->locals.end()) {
            return {};
        }
        return {it->second};
    };

    auto block = currentBlock();
    do {
        auto val = valueInLocals(block);
        if (val) {
            return val;
        }
        block = block->parent;
    } while (block);

    return {};
}

void CodeGenContext::storeGlobal(const std::string &name, Value value)
{
    m_globals[name] = value;
}

Optional<Value> CodeGenContext::global(const std::string &name) const
{
    auto it = m_globals.find(name);
    if (it != m_globals.end()) {
        return {it->second};
    }
    return {};
}

void CodeGenContext::pushBlock(llvm::BasicBlock *block, llvm::Function *function, CodeGenBlock *parent)
{
    m_blocks.push({block, function, parent, {}, false });
}

void CodeGenContext::popBlock()
{
    m_blocks.pop();
}

llvm::Value *CodeGenContext::allocate(llvm::Type *type, const std::string &name, llvm::Value *val)
{
    llvm::AllocaInst *alloc = new llvm::AllocaInst(type, name.c_str(), currentBlock()->block);
    new llvm::StoreInst(val, alloc, false, currentBlock()->block);
    return alloc;
}

/* -- Code Generation -- */

Optional<Value> NInteger::codeGen(CodeGenContext &context)
{
    std::cout << "Creating integer: " << value << '\n';

    return simpleValue(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context.context()), value, true));
}

Optional<Value> NBoolean::codeGen(CodeGenContext &context)
{
    return simpleValue(llvm::ConstantInt::get(llvm::Type::getInt1Ty(context.context()), value, true));
}

Optional<Value> NDouble::codeGen(CodeGenContext &context)
{
    std::cout << "Creating double: " << value << '\n';

    return simpleValue(llvm::ConstantFP::get(llvm::Type::getDoubleTy(context.context()), value));
}

Optional<Value> NString::codeGen(CodeGenContext &context)
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
    auto constant = llvm::ConstantDataArray::getString(context.context(), value);
    auto var = new llvm::GlobalVariable(context.module(), llvm::ArrayType::get(llvm::Type::getInt8Ty(context.context()), value.length() + 1), true, llvm::GlobalValue::PrivateLinkage, constant, ".str");

    auto id1 = llvm::ConstantInt::get(context.context(), llvm::APInt(32, 0, false));
    auto id2 = llvm::ConstantInt::get(context.context(), llvm::APInt(32, 0, false));

    return simpleValue(llvm::GetElementPtrInst::CreateInBounds(var, {id1, id2}, "", context.currentBlock()->block));
}

static Value createValue(CodeGenContext &ctx, llvm::Value *value, llvm::Type *valueType)
{
    auto type = valueType;
    while (type->isPointerTy()) {
        type = type->getPointerElementType();
    }

    if (type->isStructTy()) {
        if (auto info = ctx.structInfo(type)) {
            return structValue(value, valueType, info, ctx);
        } else if (auto info = ctx.tupleInfo(type)) {
            return tupleValue(value, valueType, info, ctx);
        }
    }
    return simpleValue(value, valueType);
}

Optional<Value> NIdentifier::codeGen(CodeGenContext &ctx)
{
    if (context()) {
        auto parent = context()->codeGen(ctx);
        Value::V val;
        if (type == NIdentifier::Index) {
            try {
                val = parent->extract(index);
            } catch (OutOfRangeException &ex) {
                err(token(), "invalid index '{}' when accessing aggregate of type '{}' with {} elements", index, ex.type, ex.size);
            }

        } else {
            try {
                val = parent->extract(name);
            } catch (InvalidFieldException &ex) {
                err(token(), "struct '{}' has no field named '{}'", ex.type, name);
            }
        }
        auto value = createValue(ctx, val.value, val.type);
        value.setMutable(parent->isMutable() && val.mut);
        return value;
    }

    auto val = ctx.local(name);
    if (!val) {
        val = ctx.global(name);
    }
    if (!val) {
        err(token(), "'{}' was not declared in this scope", name);
    }

    return val;
}

Optional<Value> NAddressOfExpression::codeGen(CodeGenContext &context)
{
    auto val = expression()->codeGen(context);

    struct RefValueH
    {
        Value value;
        std::vector<Value::V> values;

        RefValueH(Value value, std::vector<Value::V> &val)
            : value(value)
        {
            std::swap(val, values);
        }

        const std::vector<Value::V> &unpack() const
        {
            return values;
        }

        RefValueH clone(std::vector<Value::V> &values) const
        {
            return RefValueH(value, values);
        }

        Value::V extract(int id) const { auto v = value.extract(id); return { v.value, v.value->getType(), v.mut }; }
        Value::V extract(const std::string &name) const { auto v = value.extract(name); return { v.value, v.value->getType(), v.mut }; }
    };

    std::vector<Value::V> values;
    for (auto &&v: val->unpack()) {
        values.push_back({v.value, v.type->getPointerTo(), true});
    }

    return Value(RefValueH(val, values));
}

static std::string mangleFuncName(const std::string &name, const std::string &iface, const std::string &sig)
{
    return iface + sig + "::" + name;
}

Optional<Value> NExpressionPack::codeGen(CodeGenContext &context)
{
    std::cout << "Creating tuple declaration " << m_list.size()<<"\n";

    std::vector<Value::V> values;
    for (auto &&expr: m_list) {
        auto vals = expr->codeGen(context)->unpack();
        values.insert(values.end(), vals.begin(), vals.end());
    }
    return valuePack(values);
}

llvm::Function *CodeGenContext::makeConcreteFunction(NFunctionDeclaration *func, std::vector<Value::V> &values)
{
    fmt::print("TEMPLATE {}\n",func->id);
    std::vector<llvm::Type *> argTypes;
    values.reserve(func->arguments.size());
    auto valueIt = values.begin();

    auto templateId = std::string(func->id);

    auto addType = [&](llvm::Type *t) {
        t->dump();
        argTypes.push_back(t);
        templateId += typeName(t);
    };

    for (auto it = func->arguments.begin(); it != func->arguments.end(); ++it, ++valueIt) {
        llvm::Type *valueType = nullptr;
        if (it->type().name() == "(...)") {
            for (auto it = valueIt; it < values.end(); ++it) {
                auto ty = it->type;
                if (ty->isStructTy()) {
                    ty = ty->getPointerTo();
                }
                addType(ty);
            }
            break;
        } else {
            valueType = valueIt->type;
        }

        addType(valueType);
    }

    if (auto func = m_concreteTemplates[templateId]) {
        return func;
    }

    llvm::FunctionType *ftype = llvm::FunctionType::get(typeOf(func->type), llvm::makeArrayRef(argTypes), false);
    llvm::Function *function = llvm::Function::Create(ftype, llvm::GlobalValue::ExternalLinkage, templateId.c_str(), &module());
    llvm::BasicBlock *bblock = llvm::BasicBlock::Create(context(), "entry", function, 0);

    m_concreteTemplates[templateId] = function;

    pushBlock(bblock, function, nullptr);

    llvm::Function::arg_iterator argsValues = function->arg_begin();

    auto typeIt = argTypes.begin();
    for (auto it = func->arguments.begin(); it != func->arguments.end(); ++it, ++typeIt) {
        if (it->type().name() == "(...)") {
            std::vector<Value::V> values;
            for (; typeIt != argTypes.end(); ++typeIt) {
                auto argumentValue = &*argsValues++;
                argumentValue->setName(it->name().c_str());

                auto alloc = allocate(*typeIt, it->name(), argumentValue);
                values.push_back({alloc, *typeIt});
            }
            auto value = valuePack(values);
            storeLocal(it->name(), value);
            break;
        }

        auto argumentValue = &*argsValues++;
        argumentValue->setName(it->name().c_str());

        auto alloc = allocate(*typeIt, it->name(), argumentValue);
        auto value = createValue(*this, alloc, *typeIt);
        value.setMutable(it->isMutable());

        storeLocal(it->name(), value);
    }

    func->block->codeGen(*this);
    if (!currentBlock()->returned) {
        llvm::ReturnInst::Create(context(), nullptr, currentBlock()->block);
    }

    popBlock();

    return function;
}

static llvm::Value *loadValue(llvm::Value *value, CodeGenContext &context)
{
    if (value->getType()->isPointerTy()) {
        return new llvm::LoadInst(value, "", false, context.currentBlock()->block);
    }
    return value;
}

static llvm::Function *findInterfaceImpl(const Token &tok, const std::string &name, const std::vector<Value::V> &values, CodeGenContext &context)
{
    auto proto = context.ifacePrototype(name);
    if (!proto) {
        return nullptr;
    }
    auto iface = proto->iface;

    std::cout<<"in iface "<<iface->name<<"\n";

    std::vector<llvm::Type *> toMatch;
    toMatch.resize(iface->parameters.size());

    size_t numPars = proto->parameters.size();
    for (size_t i = 0; i < numPars; ++i) {
        auto v = values.size() > i ? values[i].value : nullptr;
        auto t = v ? v->getType() : (i == 0 ? context.tupleType({}) : nullptr);
        if (!t) {
            break;
        }

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

Optional<Value> NMethodCall::codeGen(CodeGenContext &context)
{
    std::cout << "Creating method call: " << name << '\n';

    std::vector<Value::V> values;
    if (this->context()) {
        auto vec = this->context()->codeGen(context)->unpack();
        values.insert(values.end(), vec.begin(), vec.end());
    }
    for (auto &&arg: arguments) {
        auto value = arg->codeGen(context);
        auto vec = value->unpack();
        values.insert(values.end(), vec.begin(), vec.end());
    }

    if (name == "count") {
        return simpleValue(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context.context()), values.size(), true));
    }

    llvm::Function *function = context.module().getFunction(name.c_str());

    if (!function) {
        function = findInterfaceImpl(token(), name, values, context);
    }

    if (!function) {
        function = context.functionTemplate(name, values);
    }

    if (function == NULL) {
        err(token(), "no such function '{}'", name);
    }

    size_t i = 0;
    for (auto &&arg: function->getArgumentList()) {
        auto argTy = arg.getType();

        auto converted = context.convertTo(values[i].value, argTy);
        if (!converted) {
            err(token(), "wrong argument type to function; expected '{}', found '{}'", typeName(argTy), typeName(values[i].value->getType()));
        }
        values[i].value = converted;
        i++;
    }
    if (function->isVarArg()) {
        for (; i < values.size(); ++i) {
            // varargs, we need to do a load here
            values[i].value = loadValue(values[i].value, context);
        }
    } else {
        size_t numArgs = i;
        if (numArgs < values.size()) {
            err(token(), "too many arguments to function");
        }
    }

    std::vector<llvm::Value *> vals;
    for (auto &&v: values) {
        vals.push_back(v.value);
    }

    llvm::CallInst *call = llvm::CallInst::Create(function, llvm::makeArrayRef(vals), "", context.currentBlock()->block);
    return createValue(context, call, function->getReturnType());
}

Optional<Value> NAssignment::codeGen(CodeGenContext &context)
{
//     std::cout << "Creating assignment for " << lhs->name << " "<<this<<'\n';

    auto lhsValue = lhs->codeGen(context);
    auto rhsValue = rhs->codeGen(context);

    if (!lhsValue->isMutable()) {
        err(token(), "attempting to assign to non-mutable variable");
    }

    auto lhsVec = lhsValue->unpack();
    auto rhsVec = rhsValue->unpack();

    assert(lhsVec.size() == rhsVec.size());

    for (size_t i = 0; i < lhsVec.size(); ++i) {
        new llvm::StoreInst(rhsVec[i].value, lhsVec[i].value, false, context.currentBlock()->block);
    }
    return lhsValue;
}

Optional<Value> NBlock::codeGen(CodeGenContext &context)
{
    StatementList::const_iterator it;
    for (it = statements.begin(); it != statements.end(); it++) {
        std::cout << "block: Generating code for " << typeid(**it).name() << '\n';
        (**it).codeGen(context);
        if (context.currentBlock()->returned) {
            break;
        }
    }
    std::cout << "Creating block" << '\n';
    return {};
}

Optional<Value> NExpressionStatement::codeGen(CodeGenContext &context)
{
    std::cout << "Generating code for " << typeid(expression).name() << '\n';
    return expression->codeGen(context);
}

llvm::Value *CodeGenContext::convertTo(llvm::Value *value, llvm::Type *type) const
{
    fmt::print("CONVERT {} to {}\n",typeName(value->getType()), typeName(type));
    if (value->getType() == type) {
        return value;
    }

    auto baseDest = type;
    int destPointer = 0;
    while (baseDest->isPointerTy()) {
        baseDest = baseDest->getPointerElementType();
        destPointer++;
    }
    auto baseSrc = value->getType();
    int srcPointer = 0;
    while (baseSrc->isPointerTy()) {
        baseSrc = baseSrc->getPointerElementType();
        srcPointer++;
    }
    if (baseDest != baseSrc) {
        return nullptr;
    }

    while (srcPointer > destPointer) {
        value = new llvm::LoadInst(value, "", currentBlock()->block);
        --srcPointer;
    }
    if (destPointer == srcPointer) {
        return value;
    }

    return nullptr;
}

Optional<Value> NReturnStatement::codeGen(CodeGenContext &context)
{
    auto returnValue = expression ? expression->codeGen(context) : Optional<Value>();

    context.currentBlock()->returned = true;

    llvm::Value *llvmval = nullptr;
    if (returnValue) {
        auto vals = returnValue->unpack();
        llvm::Type *type = nullptr;
        auto retType = context.currentBlock()->function->getReturnType();
        if (vals.size() == 1) {
            type = vals.front().value->getType();
            llvmval = context.convertTo(vals.front().value, retType);
        } else if (retType->isStructTy()) {
            auto tt = static_cast<llvm::StructType *>(retType);
            auto types = tt->elements();
            if (types.size() == vals.size()) {
                auto typeIt = types.begin();
                std::vector<llvm::Value *> values;
                for (auto &&v: vals) {
                    auto value = context.convertTo(v.value, *typeIt++);
                    if (!value) {
                        break;
                    }
                    values.push_back(value);
                }
                llvmval = context.makeTupleValue(values, "ret");
                type = llvmval->getType();
                llvmval = context.convertTo(llvmval, retType);
            }
        }
        if (!llvmval) {
            err(token(), "wrong return type for function. found '{}', expected '{}'", typeName(type), typeName(retType));
        }
    }

    return simpleValue(llvm::ReturnInst::Create(context.context(), llvmval, context.currentBlock()->block));
}

Value NVarExpressionInitializer::init(CodeGenContext &ctx, const std::string &name)
{
    if (!expression) {
        err(token, "missing type or initializer when declaring variable '{}'", name);
    }
    auto init = expression->codeGen(ctx);

    if (type.valid()) {
        auto t = ctx.typeOf(type);
        llvm::AllocaInst *alloc = new llvm::AllocaInst(t, name.c_str(), ctx.currentBlock()->block);
        auto value = createValue(ctx, alloc, t);

        new llvm::StoreInst(init->unpack()[0].value, value.unpack()[0].value, false, ctx.currentBlock()->block);
        return value;
    } else {
        std::vector<Value::V> values;
        for (auto &&val: init->unpack()) {
            auto alloc = ctx.allocate(val.type, name, val.load(ctx));
            values.push_back({alloc, val.type});
        }

        auto v = init->clone(values);
        return v;
    }
}

Value NVarStructInitializer::init(CodeGenContext &ctx, const std::string &name)
{
    auto t = ctx.typeOf(type);
    auto info = ctx.structInfo(t);
    if (!info) {
        error("boo");
    }

    llvm::AllocaInst *alloc = new llvm::AllocaInst(t, name.c_str(), ctx.currentBlock()->block);

    if (fields.size() != info->fields.size()) {
        err(token, "wrong number of initializers passed when declaring variable of type '{}'", name, type.name());
    }

    auto value = createValue(ctx, alloc, t);

    for (auto &&f: fields) {
        auto v = value.extract(f.name);
        auto src = f.init->codeGen(ctx);

        new llvm::StoreInst(src->unpack()[0].value, v.value, false, ctx.currentBlock()->block);
    }

    return value;
}

Optional<Value> NVariableDeclaration::codeGen(CodeGenContext &context)
{
    std::cout << "Creating variable declaration " << id.name() << " "<< id.isMutable()<< '\n';

    auto value = init->init(context, id.name());
    value.setMutable(id.isMutable());
    context.storeLocal(id.name(), value);
    return value;
}

Optional<Value> NMultiVariableDeclaration::codeGen(CodeGenContext &context)
{
    fmt::print("\n\nMULTI\n\n");
    auto values = expression()->codeGen(context)->unpack();

    auto getValue = [&](size_t i) -> Value::V * {
        if (values.size() <= i) {
            return {};
        }
        return &values[i];
    };

    size_t i = 0;
    for (auto &&name: names()) {
        auto val = getValue(i);
        bool lastName = i == names().size() - 1;

        Value value;
        if (val) {
            if (lastName && values.size() > i + 1) {

                std::vector<Value::V> vals;
                for (size_t j = i; j < values.size(); ++j) {
                    auto alloc = context.allocate(values[j].type, name.name(), values[j].load(context));
                    vals.push_back({alloc, values[j].type});
                }
                value = valuePack(vals);
                value.setMutable(name.isMutable());
            } else {
                auto alloc = context.allocate(val->type, name.name(), val->load(context));
                value = createValue(context, alloc, val->type);
                value.setMutable(name.isMutable());
            }
        } else {
            std::vector<Value::V> vec;
            value = valuePack(vec);
        }

        fmt::print("NEW MULTI {}\n",name.name());

        context.storeLocal(name.name(), value);
        ++i;
    }
    return {};
}

Optional<Value> NExternDeclaration::codeGen(CodeGenContext &context)
{
    std::vector<llvm::Type *> argTypes;
    argTypes.reserve(arguments().size());
    for (auto &&arg: arguments()) {
        argTypes.push_back(context.typeOf(arg.type()));
    }
    llvm::FunctionType *ftype = llvm::FunctionType::get(context.typeOf(returnType()), llvm::makeArrayRef(argTypes), isVarargs());
    llvm::Function::Create(ftype, llvm::GlobalValue::ExternalLinkage, name().c_str(), &context.module());

    return {};
}

Optional<Value> NFunctionDeclaration::codeGen(CodeGenContext &context)
{
    if (!context.isFunctionNameAvailable(id)) {
        err(token(), "function '{}' is already declared", id);
    }

    std::vector<llvm::Type *> argTypes;
    for (auto it = arguments.begin(); it != arguments.end(); it++) {
        if (it->type().name() == "(...)") {
            context.addFunctionTemplate(this);
            return {};
        }
        argTypes.push_back(context.typeOf(it->type()));
    }
    auto retType = context.typeOf(type);
    llvm::FunctionType *ftype = llvm::FunctionType::get(retType, llvm::makeArrayRef(argTypes), false);
    llvm::Function *function = llvm::Function::Create(ftype, llvm::GlobalValue::ExternalLinkage, id.c_str(), &context.module());
    if (block) {
        llvm::BasicBlock *bblock = llvm::BasicBlock::Create(context.context(), "entry", function, 0);

        context.pushBlock(bblock, function, nullptr);

        llvm::Function::arg_iterator argsValues = function->arg_begin();
        auto typesIt = argTypes.begin();
        for (auto it = arguments.begin(); it != arguments.end(); it++, typesIt++) {
            auto argumentValue = &*argsValues++;
            argumentValue->setName(it->name().c_str());

            auto alloc = context.allocate(*typesIt, it->name(), argumentValue);
            auto value = createValue(context, alloc, *typesIt);
            value.setMutable(it->isMutable());
            context.storeLocal(it->name(), value);
        }

        block->codeGen(context);
        if (!context.currentBlock()->returned) {
            llvm::ReturnInst::Create(context.context(), nullptr, context.currentBlock()->block);
        }

        context.popBlock();
    }
    std::cout << "Creating function: " << id << '\n';
    return {};
}

Optional<Value> NStructDeclaration::codeGen(CodeGenContext &context)
{
    std::cout << "Creating struct declaration " << " " << id << '\n';
    std::vector<llvm::Type *> argTypes;
    for (auto it = elements.begin(); it != elements.end(); it++) {
        argTypes.push_back(context.typeOf(it->type));
        std::cout<<"    with arg " << it->type.name() << " " <<it->name<<"\n";
    }
    llvm::StructType *type = llvm::StructType::create(context.context(), argTypes, id.c_str());
    auto info = context.newStructType(type);
    info->type = type;
    info->fields.reserve(elements.size());
    for (auto &&el: elements) {
        info->fields.push_back({ el.name, el.mut });
    }
    return {};
}

Optional<Value> NIfaceDeclaration::codeGen(CodeGenContext &context)
{
    context.addInterface(this);
    if (prototypes.size() == 0) {
        error("cannot create an empty interface");
    }
    for (auto &&p: prototypes) {
        p->iface = this;
    }

    return {};
}

Optional<Value> NImplDeclaration::codeGen(CodeGenContext &context)
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

    llvm::raw_string_ostream stream(id);
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

    return {};
}

Optional<Value> NIfStatement::codeGen(CodeGenContext &ctx)
{
    auto cond = condition()->codeGen(ctx)->extract(0);

    if (cond.value->getType()->isPointerTy()) {
        cond.value = new llvm::LoadInst(cond.value, "", false, ctx.currentBlock()->block);
        auto t = static_cast<llvm::PointerType *>(cond.value->getType());
        cond.value = new llvm::ICmpInst(*ctx.currentBlock()->block, llvm::CmpInst::ICMP_NE, cond.value, llvm::ConstantPointerNull::get(t));
    }

    auto curBlock = ctx.currentBlock();
    llvm::BasicBlock *ifblock = llvm::BasicBlock::Create(ctx.context(), "if", curBlock->function, 0);
    llvm::BasicBlock *elseblock = elseBlock() ? llvm::BasicBlock::Create(ctx.context(), "else", curBlock->function, 0) : nullptr;
    llvm::BasicBlock *afterblock = llvm::BasicBlock::Create(ctx.context(), "endif", curBlock->function, 0);

    ctx.pushBlock(ifblock, curBlock->function, curBlock);
    block()->codeGen(ctx);
    if (!ctx.currentBlock()->returned) {
        llvm::BranchInst::Create(afterblock, ctx.currentBlock()->block);
    }
    ctx.popBlock();

    if (elseBlock()) {
        ctx.pushBlock(elseblock, curBlock->function, curBlock);
        elseBlock()->codeGen(ctx);
        if (!ctx.currentBlock()->returned) {
            llvm::BranchInst::Create(afterblock, ctx.currentBlock()->block);
        }
        ctx.popBlock();
    }

    llvm::BranchInst::Create(ifblock, elseblock ? elseblock : afterblock, cond.value, ctx.currentBlock()->block);

    ctx.currentBlock()->block = afterblock;

    return {};
}

Optional<Value> NWhileStatement::codeGen(CodeGenContext &ctx)
{
    auto curBlock = ctx.currentBlock();
    llvm::BasicBlock *condblock = llvm::BasicBlock::Create(ctx.context(), "cond", curBlock->function, 0);
    llvm::BasicBlock *whileblock = llvm::BasicBlock::Create(ctx.context(), "while", curBlock->function, 0);
    llvm::BasicBlock *afterblock = llvm::BasicBlock::Create(ctx.context(), "endwhile", curBlock->function, 0);

    ctx.pushBlock(condblock, curBlock->function, curBlock);
    auto cond = condition()->codeGen(ctx)->extract(0);
    llvm::BranchInst::Create(whileblock, afterblock, cond.load(ctx), ctx.currentBlock()->block);
    ctx.popBlock();

    ctx.pushBlock(whileblock, curBlock->function, curBlock);
    block()->codeGen(ctx);
    if (!ctx.currentBlock()->returned) {
        llvm::BranchInst::Create(condblock, ctx.currentBlock()->block);
    }
    ctx.popBlock();

    llvm::BranchInst::Create(condblock, ctx.currentBlock()->block);

    ctx.currentBlock()->block = afterblock;

    return {};
}

Optional<Value> NExternVariableDeclaration::codeGen(CodeGenContext &ctx)
{
    auto ty = ctx.typeOf(type());
    auto val = new llvm::GlobalVariable(ctx.module(), ty, false, llvm::GlobalValue::ExternalLinkage, nullptr, name().c_str());
    auto value = simpleValue(val);
    ctx.storeGlobal(name(), value);
    return value;
}
