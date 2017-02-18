#include <fstream>
#include <string>

#include <llvm/Support/raw_os_ostream.h>

#include "node.h"
#include "codegen.h"
// #include "parser.hpp"
#include "common.h"

Debug::Debug(CodeGenContext *ctx, const std::string &filename)
     : m_ctx(ctx)
     , m_builder(ctx->module())
     , m_cunit(m_builder.createCompileUnit(llvm::dwarf::DW_LANG_C, filename, ".", "PinkPig", 0, "", 0))
{
    ctx->module().addModuleFlag(llvm::Module::Warning, "Debug Info Version", llvm::DEBUG_METADATA_VERSION);
}

void Debug::finalize()
{
    m_builder.finalize();
}

llvm::DIFile *Debug::fileUnit(const std::string &name)
{
    auto it = m_fileUnits.find(name);
    if (it != m_fileUnits.end()) {
        return it->second;
    }

    auto un = m_builder.createFile(name, ".");
    m_fileUnits[name] = un;
    return un;
}

llvm::DISubprogram *Debug::createFunction(const Token &token, const std::string &funcName, const std::vector<NFunctionArgumentDeclaration> &arguments)
{
    std::vector<llvm::Metadata *> types;
    for (auto &&a: arguments) {
        types.push_back(m_builder.createUnspecifiedType(a.type().name()));
    }

    auto unit = fileUnit(token.filename());
    auto functionTy = m_builder.createSubroutineType(m_builder.getOrCreateTypeArray(types));
    return m_builder.createFunction(unit, funcName, llvm::StringRef(), unit, token.lineNo(), functionTy, false, true, token.lineNo(), 0, false);
}

void Debug::createVariable(const Token &token, const std::string &name, const Value &value)
{
    auto file = fileUnit(token.filename());
    auto ty = m_builder.createUnspecifiedType(value.type().name());
    auto var = m_builder.createAutoVariable(m_scopes.top(), name, file, token.lineNo(), ty);

    if (auto fc = value.getSpecialization<FirstClassValue>()) {
        m_builder.insertDeclare(fc->value(), var, m_builder.createExpression(), llvm::DebugLoc::get(token.lineNo(), 0, m_scopes.top()), m_ctx->currentBlock()->block);
    }
}

void Debug::pushScope(llvm::DIScope *scope)
{
    m_scopes.push(scope);
}

void Debug::popScope()
{
    m_scopes.pop();
}

void Debug::setLocation(const Token &token)
{
    m_ctx->builder().SetCurrentDebugLocation(llvm::DebugLoc::get(token.lineNo(), token.columnNo(), m_scopes.top()));
}


CodeGenContext::CodeGenContext(const std::string &name)
              : m_module(std::make_unique<llvm::Module>(name.c_str(), m_context))
              , m_builder(m_context)
              , m_debug(this, name)
{
    auto file = m_debug.fileUnit(name);
    m_debug.pushScope(file);
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
    builder().CreateRetVoid();
    popBlock();

    m_debug.finalize();

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

    llvm::AllocaInst *alloc = builder().CreateAlloca(type, nullptr, name.c_str());

    int id = 0;
    for (auto &&v: values) {
        auto id1 = builder().getInt32(0);
        auto id2 = builder().getInt32(id++);

        auto value = builder().CreateInBoundsGEP(alloc, {id1, id2});
        builder().CreateStore(v, value, false);
    }

    return alloc;
}

/* Returns an LLVM type based on the identifier */
llvm::Type *CodeGenContext::typeOf(const std::string &type)
{
    if (m_structInfo.find(type) != m_structInfo.end()) {
        return m_structInfo[type].type.get(*this);
    }
    return nullptr;
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

const StructInfo *CodeGenContext::structInfo(const std::string &name) const
{
    auto it = m_structInfo.find(name);
    if (it != m_structInfo.end()) {
        return &it->second;
    }
    return nullptr;
}

StructInfo *CodeGenContext::newStructType(llvm::StructType *type)
{
    auto info = &m_structInfo[type->getName()];
    m_structInfoByType[type] = info;
    return info;
}

const UnionInfo *CodeGenContext::unionInfo(llvm::Type *type) const
{
    if (!type->isStructTy()) {
        return nullptr;
    }

    auto it = m_unionInfoByType.find(type);
    if (it != m_unionInfoByType.end()) {
        return it->second;
    }
    return nullptr;
}

const UnionInfo *CodeGenContext::unionInfo(const std::string &name) const
{
    auto it = m_unionInfo.find(name);
    if (it != m_unionInfo.end()) {
        return &it->second;
    }
    return nullptr;
}

UnionInfo *CodeGenContext::newUnionType(llvm::StructType *type)
{
    auto info = &m_unionInfo[type->getName()];
    m_unionInfoByType[type] = info;
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

FunctionInfo *CodeGenContext::addFunctionInfo(llvm::Function *fun)
{
    return &m_functionInfo[fun];
}

const FunctionInfo *CodeGenContext::functionInfo(llvm::Function *fun) const
{
    auto it = m_functionInfo.find(fun);
    if (it == m_functionInfo.end()) {
        return nullptr;
    }
    return &it->second;
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

llvm::Function *CodeGenContext::functionTemplate(const std::string &name, std::vector<FirstClassValue *> &values)
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
    m_builder.SetInsertPoint(block);
}

void CodeGenContext::popBlock()
{
    m_blocks.pop();
    if (m_blocks.size() > 0) {
        m_builder.SetInsertPoint(m_blocks.top().block);
    }
}

bool CodeGenContext::addDeclaredType(const std::string &name, llvm::Type *type)
{
    if (m_declaredTypes.find(name) != m_declaredTypes.end()) {
        return false;
    }
    m_declaredTypes.insert(std::make_pair(name, type));
    return true;
}

llvm::Type *CodeGenContext::declaredType(const std::string &name) const
{
    auto it = m_declaredTypes.find(name);
    if (it == m_declaredTypes.end()) {
        return nullptr;
    }
    return it->second;
}


/* -- Code Generation -- */

Value constantInteger(CodeGenContext &ctx, int val, Type t = {})
{
    if (!t.isValid()) {
        t = IntegerType(true, 32);
    }
    t.setTypeConstraint(TypeConstraint(TypeConstraint::Operator::Equal, val));
    return simpleValue(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.context()), val, true), t);
}

Optional<Value> NInteger::codeGen(CodeGenContext &context)
{
    std::cout << "Creating integer: " << value << '\n';
    return constantInteger(context, value);
}

Optional<Value> NBoolean::codeGen(CodeGenContext &context)
{
    auto v = llvm::ConstantInt::get(llvm::Type::getInt1Ty(context.context()), value, true);
    return simpleValue(v, llvmType(context, v->getType()));
}

Optional<Value> NDouble::codeGen(CodeGenContext &context)
{
    std::cout << "Creating double: " << value << '\n';

    auto v = llvm::ConstantFP::get(llvm::Type::getDoubleTy(context.context()), value);
    return simpleValue(v, FloatingType(64));
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
    auto v = context.builder().CreateGlobalStringPtr(value, ".str");
    return simpleValue(v, llvmType(context, v->getType()));
}

Optional<Value> NIdentifier::codeGen(CodeGenContext &ctx)
{
    if (context()) {
        auto parent = context()->codeGen(ctx);
        Value value;
        if (type == NIdentifier::Index) {
            if (auto v = parent->getSpecialization<AggregateValue>()) {
                try {
                    value = v->extract(index);
                } catch (OutOfRangeException &ex) {
                    err(token(), "invalid index '{}' when accessing aggregate of type '{}' with {} elements", index, ex.type, ex.size);
                }
            } else {
                err(token(), "'{}' is not an aggregate type", parent->getSpecialization<FirstClassValue>()->type().name());
            }
        } else {
            if (auto v = parent->getSpecialization<NamedAggregateValue>()) {
                try {
                    value = v->extract(name);
                } catch (InvalidFieldException &ex) {
                    err(token(), "aggregate '{}' has no field named '{}'", ex.type, name);
                }
            } else {
                err(token(), "'{}' is not a named aggregate type", parent->getSpecialization<FirstClassValue>()->type().name());
            }
        }
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

    if (auto v = val->getSpecialization<FirstClassValue>()) {
        auto value = createValue(context, v->value(), v->type().getPointerTo());
        value.setBindingPoint(ValueBindingPoint(val->bindingPoint()->type().getPointerTo()));

        return value;
    }
    err(token(), "taking address of non first type value");
    return {};

}

static std::string mangleFuncName(const std::string &name, const std::string &iface, const std::string &sig)
{
    return iface + sig + "::" + name;
}

Optional<Value> NExpressionPack::codeGen(CodeGenContext &context)
{
    std::cout << "Creating tuple declaration " << m_list.size()<<"\n";

    if (m_list.size() == 1) {
        return m_list.front()->codeGen(context);
    }

    std::vector<Value> values;
    for (auto &&expr: m_list) {
        auto vals = expr->codeGen(context);
        values.push_back(vals);
    }
    return valuePack(values);
}

llvm::Function *CodeGenContext::makeConcreteFunction(NFunctionDeclaration *func, std::vector<FirstClassValue *> &values)
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
        if (it->type().getSpecialization<ArgumentPackType>()) {
            for (auto it = valueIt; it < values.end(); ++it) {
                auto ty = (*it)->type().get(*this);
                if (ty->isStructTy()) {
                    ty = ty->getPointerTo();
                }
                addType(ty);
            }
            break;
        } else {
            valueType = (*valueIt)->type().get(*this);
        }

        addType(valueType);
    }

    if (auto func = m_concreteTemplates[templateId]) {
        return func;
    }

    llvm::FunctionType *ftype = llvm::FunctionType::get(func->type.get(*this), llvm::makeArrayRef(argTypes), false);
    llvm::Function *function = llvm::Function::Create(ftype, llvm::GlobalValue::ExternalLinkage, templateId.c_str(), &module());
    llvm::BasicBlock *bblock = llvm::BasicBlock::Create(context(), "entry", function, 0);

    auto info = addFunctionInfo(function);
    info->returnType = func->type;

    m_concreteTemplates[templateId] = function;

    pushBlock(bblock, function, nullptr);

    llvm::Function::arg_iterator argsValues = function->arg_begin();

    StackAllocator allocator(*this);
    auto typeIt = argTypes.begin();
    for (auto it = func->arguments.begin(); it != func->arguments.end(); ++it, ++typeIt) {
        if (it->type().getSpecialization<ArgumentPackType>()) {
            std::vector<Value> values;
            for (; typeIt != argTypes.end(); ++typeIt) {
                auto argumentValue = &*argsValues++;
                argumentValue->setName(it->name().c_str());

                Type type = llvmType(*this, *typeIt);
                values.push_back(type.create(*this, &allocator, it->name(), createValue(*this, argumentValue, type)));
                info->argTypes.push_back(type);
            }
            auto value = valuePack(values);
            storeLocal(it->name(), value);
            break;
        }

        auto argumentValue = &*argsValues++;
        argumentValue->setName(it->name().c_str());

        auto value = it->type().create(*this, &allocator, it->name(), createValue(*this, argumentValue, it->type()));
        value.setBindingPoint(ValueBindingPoint(it->type()));
        value.setMutable(it->isMutable());

        storeLocal(it->name(), value);

        info->argTypes.push_back(it->type());
    }

    func->block->codeGen(*this);
    if (!currentBlock()->returned) {
        builder().CreateRetVoid();
    }

    popBlock();

    return function;
}

static llvm::Value *loadValue(llvm::Value *value, CodeGenContext &context)
{
    if (value->getType()->isPointerTy()) {
        return context.builder().CreateLoad(value);
    }
    return value;
}

static llvm::Function *findInterfaceImpl(const Token &tok, const std::string &name, const std::vector<FirstClassValue *> &values, CodeGenContext &context)
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
        auto v = values.size() > i ? values[i]->value() : nullptr;
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

static bool canStoreInto(CodeGenContext &ctx, const Type &lhs, const Type &rhs)
{
    if (lhs.get(ctx) != rhs.get(ctx) || !rhs.typeConstraint().isCompatibleWith(lhs.typeConstraint())) {
        return false;
    }
    if (auto lp = lhs.getSpecialization<PointerType>()) {
        auto rp = rhs.getSpecialization<PointerType>();
        return canStoreInto(ctx, lp->pointerElementType(), rp->pointerElementType());
    }
    return true;
}

Optional<Value> NMethodCall::codeGen(CodeGenContext &context)
{
    std::cout << "Creating method call: " << name << '\n';

    std::vector<Value> values;
    std::vector<FirstClassValue *> firstclasses;

    std::function<void (const Value &)> add = [&](const Value &val) {
        if (auto tuple = val.getSpecialization<PackValue>()) {
            auto vec = tuple->unpack();

            for (auto &&v: vec) {
                add(v);
            }
        } else {
            values.push_back(val);
            firstclasses.push_back(val.getSpecialization<FirstClassValue>());
        }
    };

    if (this->context()) {
        auto val = this->context()->codeGen(context);
        add(val);
    }
    for (auto &&arg: arguments) {
        auto value = arg->codeGen(context);
        add(value);
    }

    if (name == "count") {
        // if we have more than one argument return the number of them, otherwise do nothing, in
        // case there is a function named count taking that argument
        if (values.size() > 1) {
            return constantInteger(context, values.size());
        }
    } else if (name == "size") {
        if (values.size() != 1) {
            err(token(), "the built-in function 'size' accepts only one argument");
        }
        auto t = values.front().type().get(context);
        if (auto info = context.structInfo(t)) {
            return info->sizeValue;
        }
        if (t->isSized()) {
            llvm::DataLayout layout(&context.module());
            int s = layout.getTypeSizeInBits(t) / 8;
            return constantInteger(context, s);
        }
        return constantInteger(context, 0);
    }

    llvm::Function *function = context.module().getFunction(name.c_str());

    if (!function) {
        function = findInterfaceImpl(token(), name, firstclasses, context);
    }

    if (!function) {
        function = context.functionTemplate(name, firstclasses);
    }

    if (!function) {
        auto mangledName = name;
        for (auto &&v: values) {
            mangledName += "_";
            mangledName += v.type().typeName();
        }

        function = context.module().getFunction(mangledName.c_str());
    }

    if (function == NULL) {
        err(token(), "no such function '{}'", name);
    }

    std::vector<llvm::Value *> vals;

    size_t i = 0;
    auto info = context.functionInfo(function);
    for (; i < function->getArgumentList().size(); ++i) {
        auto converted = context.convertTo(firstclasses[i]->value(), firstclasses[i]->type(), info->argTypes[i]);
        if (!converted) {
            err(token(), "wrong argument type to function; expected '{}', found '{}'", info->argTypes[i].name(), firstclasses[i]->type().name());
        }

        vals.push_back(converted);
    }
    if (function->isVarArg()) {
        for (; i < values.size(); ++i) {
            // varargs, we need to do a load here
            vals.push_back(loadValue(firstclasses[i]->value(), context));
        }
    } else {
        size_t numArgs = i;
        if (numArgs < values.size()) {
            err(token(), "too many arguments to function");
        }
    }

    context.debug().setLocation(token());

    llvm::Value *call = context.builder().CreateCall(function, llvm::makeArrayRef(vals));
    return createValue(context, call, info->returnType);
}

Optional<Value> NAssignment::codeGen(CodeGenContext &context)
{
//     std::cout << "Creating assignment for " << lhs->name << " "<<this<<'\n';

    auto lhsValue = lhs->codeGen(context);
    auto rhsValue = rhs->codeGen(context);

    if (!lhsValue->isMutable()) {
        err(token(), "attempting to assign to non-mutable variable");
    }

    auto lhsFirstClass = lhsValue->getSpecialization<FirstClassValue>();
    auto rhsFirstClass = rhsValue->getSpecialization<FirstClassValue>();
    assert(lhsFirstClass && rhsFirstClass);

    auto valueType = [&]() {
        if (auto b = lhsValue->bindingPoint()) {
            if (b->type().isValid()) {
                return b->type();
            }
        }
        auto t = lhsFirstClass->type();
        t.setTypeConstraint(TypeConstraint());
        return t;
    }();

    //attempt to convert the rhs to the type of the lhs
    auto rhsVal = context.convertTo(rhsFirstClass->value(), rhsFirstClass->type(), valueType);
    auto lhsVal = lhsFirstClass->value();
    // if that failed, attempt to dereference the left hand side until it matches what is provided on the right hand side
    if (!rhsVal) {
        auto rt = rhsFirstClass->type();
        rt.setTypeConstraint(TypeConstraint());
        lhsVal = context.convertTo(lhsFirstClass->value(), lhsFirstClass->type(), rt.getPointerTo());

        if (lhsVal && canStoreInto(context, valueType, rhsFirstClass->type().getPointerTo()) ) {
            rhsVal = rhsFirstClass->value();
        }
    }
    if (!rhsVal) {
        err(token(), "cannot store a value of type '{}' to a binding point of type '{}'", rhsValue->type().name(), valueType.name());
    }

    context.builder().CreateStore(rhsVal, lhsVal);
    lhsFirstClass->type().setTypeConstraint(rhsFirstClass->type().typeConstraint());

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

llvm::Value *CodeGenContext::convertTo(llvm::Value *value, const Type &from, const Type &to)
{
    auto baseDest = to.get(*this);
    auto baseSrc = value->getType();

    fmt::print("CONVERT {} ({}) to {}\n", typeName(baseSrc), from.name(), to.name());

    int srcPointer = 0;
    while (baseSrc->isPointerTy()) {
        baseSrc = baseSrc->getPointerElementType();
        srcPointer++;
    }

    auto checkConstraints = [&]() {
        int fromPointer = 0;
        Type t = from;
        while (auto p = t.getSpecialization<PointerType>()) {
            t = p->pointerElementType();
            ++fromPointer;
        }

        t = from;
        while (fromPointer > srcPointer) {
            t = t.getSpecialization<PointerType>()->pointerElementType();
            --fromPointer;
        }
        while (fromPointer < srcPointer) {
            t = t.getPointerTo();
            ++fromPointer;
        }

        return canStoreInto(*this, to, t);
    };

    if (baseDest == value->getType()) {
        if (!checkConstraints()) {
            return nullptr;
        }
        return value;
    }

    int destPointer = 0;
    while (baseDest->isPointerTy()) {
        baseDest = baseDest->getPointerElementType();
        destPointer++;
    }

    if (baseDest != baseSrc) {
        if (to.getSpecialization<IntegerType>() && from.getSpecialization<IntegerType>()) {
            auto intTy = to.getSpecialization<IntegerType>();

            auto max = intTy->maxValue();
            auto min = intTy->minValue();
            TypeConstraint constraint;
            constraint.addConstraint(TypeConstraint::Operator::LesserEqual, max);
            constraint.addConstraint(TypeConstraint::Operator::GreaterEqual, min);
            if (from.typeConstraint().isCompatibleWith(constraint)) {
                return builder().CreateTrunc(value, to.get(*this));
            } else {
                err({}, "cannot truncate from type '{}' to type '{}': value must be between {} and {}",  from.name(), to.name(), min, max);
            }
        }

        return nullptr;
    }

    while (srcPointer > destPointer) {
        value = builder().CreateLoad(value);
        --srcPointer;
    }

    if (destPointer == srcPointer && checkConstraints()) {
        return value;
    }

    return nullptr;
}

Optional<Value> NReturnStatement::codeGen(CodeGenContext &context)
{
    auto returnValue = expression ? expression->codeGen(context) : Optional<Value>();

    auto function = context.currentBlock()->function;
    auto retType = context.functionInfo(function)->returnType;

    context.currentBlock()->returned = true;

    llvm::Value *llvmval = nullptr;
    if (returnValue) {
        auto firstclass = returnValue->getSpecialization<FirstClassValue>();
        llvm::Type *type = nullptr;

        if (firstclass) {
            type = firstclass->value()->getType();
            llvmval = context.convertTo(firstclass->value(), firstclass->type(), retType);

        } else if (auto tupleType = retType.getSpecialization<TupleType>()) {
            auto types = tupleType->unpack();

            auto vals = returnValue->getSpecialization<PackValue>()->unpack();

            if (types.size() == vals.size()) {
                auto typeIt = types.begin();
                std::vector<llvm::Value *> values;
                for (auto &&v: vals) {
                    auto fc = v.getSpecialization<FirstClassValue>();
                    auto value = context.convertTo(fc->value(), fc->type(), *typeIt++);
                    if (!value) {
                        break;
                    }
                    values.push_back(value);
                }
                llvmval = context.makeTupleValue(values, "ret");
                type = llvmval->getType();
                llvmval = context.convertTo(llvmval, llvmType(context, type), retType);


            }
        }
        if (!llvmval) {
            err(token(), "wrong return type for function. found '{}', expected '{}'", typeName(type), retType.name());
        }
    }

    auto v = context.builder().CreateRet(llvmval);
    return simpleValue(v, llvmType(context, v->getType()));
}

Value NVarExpressionInitializer::init(CodeGenContext &ctx, const std::string &name)
{
    if (!expression) {
        err(token, "missing type or initializer when declaring variable '{}'", name);
    }
    auto init = expression->codeGen(ctx);

    StackAllocator allocator(ctx);

    Type varType = type.isValid() ? type : init->type();

    Value value = [&]() {
        try {
            auto value = varType.create(ctx, &allocator, name, init);
            return value;
        } catch (const CreateError &error) {
            switch (error.error) {
                case CreateError::Err::TypeError:
                    err(token, "cannot create a variable of type '{}'", varType.name());
                case CreateError::Err::StoreError:
                    err(token, "mismatched type: assigning value of type '{}' to a variable of type '{}'", init->type().name(), varType.name());
            }
            err(token, "unk");
        }
        return Value();
    }();

    if (type.isValid()) {
        auto bp = init->bindingPoint()->type();
        bp.setTypeConstraint(TypeConstraint());

        if (bp.isValid() && !canStoreInto(ctx, bp, type)) {
            err(token, "cannot take a pointer of type '{}' to type '{}': type constraints not respected", type.name(), bp.name());
        }
        value.setBindingPoint(ValueBindingPoint(type));
    }

    return value;
}

Value NVarStructInitializer::init(CodeGenContext &ctx, const std::string &name)
{
    auto t = type.get(ctx);
    size_t numFields = 0;
    if (auto info = ctx.structInfo(t)) {
        numFields = info->fields.size();
    } else if (auto i = ctx.unionInfo(t)) {
        numFields = i->fields.size();
    }

    StackAllocator allocator(ctx);
    auto value = type.create(ctx, &allocator, name);

    if (fields.empty()) {
        return value;
    }

    if (fields.size() != numFields) {
        err(token, "wrong number of initializers passed when declaring variable of type '{}'", name, type.name());
    }

    auto structValue = value.getSpecialization<NamedAggregateValue>();
    for (auto &&f: fields) {
        auto dest = structValue->extract(f.name);
        auto destfirstclass = dest.getSpecialization<FirstClassValue>();
        auto src = f.init->codeGen(ctx);
        auto firstclass = src->getSpecialization<FirstClassValue>();
        auto srcValue = firstclass->value();

        srcValue = ctx.convertTo(srcValue, firstclass->type(), destfirstclass->type());
        if (!srcValue) {
            err(token, "cannot store a value of type '{}' into a value of type '{}'", firstclass->type().name(), destfirstclass->type().name());
        }

        ctx.builder().CreateStore(srcValue, destfirstclass->value());
    }

    return value;
}

Optional<Value> NVariableDeclaration::codeGen(CodeGenContext &context)
{
    std::cout << "Creating variable declaration " << id.name() << " "<< id.isMutable()<< '\n';

    auto value = init->init(context, id.name());
    value.setMutable(id.isMutable());
    context.storeLocal(id.name(), value);

    context.debug().createVariable(token(), id.name(), value);

    return value;
}

Optional<Value> NMultiVariableDeclaration::codeGen(CodeGenContext &context)
{
    fmt::print("\n\nMULTI\n\n");
    auto source = expression()->codeGen(context);
    std::vector<Value> values;
    if (auto tuple = source->getSpecialization<PackValue>()) {
        auto vec = tuple->unpack();
        values.insert(values.end(), vec.begin(), vec.end());
    }

    auto getValue = [&](size_t i) -> Value * {
        if (values.size() <= i) {
            return {};
        }
        return &values[i];
    };

    StackAllocator allocator(context);
    size_t i = 0;
    for (auto &&name: names()) {
        auto val = getValue(i);
        bool lastName = i == names().size() - 1;

        Value value;
        if (val) {
            if (lastName && values.size() > i + 1) {

                std::vector<Value> vals;
                for (size_t j = i; j < values.size(); ++j) {
                    vals.push_back(values[j].type().create(context, &allocator, name.name(), values[j]));
                }
                value = valuePack(vals);
                value.setMutable(name.isMutable());
            } else {
                value = val->type().create(context, &allocator, name.name(), *val);
                value.setMutable(name.isMutable());
            }
        } else {
            std::vector<Value> vec;
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
        argTypes.push_back(arg.type().get(context));
    }
    llvm::FunctionType *ftype = llvm::FunctionType::get(returnType().get(context), llvm::makeArrayRef(argTypes), isVarargs());
    auto function = llvm::Function::Create(ftype, llvm::GlobalValue::ExternalLinkage, name().c_str(), &context.module());

    auto info = context.addFunctionInfo(function);
    for (auto &&arg: m_arguments) {
        info->argTypes.push_back(arg.type());
    }
    info->returnType = returnType();

    return {};
}

Optional<Value> NFunctionDeclaration::codeGen(CodeGenContext &context)
{
    std::cout << "Creating function: " << id << '\n';
    if (!context.isFunctionNameAvailable(id)) {
        err(token(), "function '{}' is already declared", id);
    }

    std::vector<llvm::Type *> argTypes;
    for (auto it = arguments.begin(); it != arguments.end(); it++) {
        if (it->type().getSpecialization<ArgumentPackType>()) {
            context.addFunctionTemplate(this);
            return {};
        }
        argTypes.push_back(it->type().get(context));
    }
    auto retType = type.get(context);
    llvm::FunctionType *ftype = llvm::FunctionType::get(retType, llvm::makeArrayRef(argTypes), false);
    llvm::Function *function = llvm::Function::Create(ftype, llvm::GlobalValue::ExternalLinkage, id.c_str(), &context.module());

    auto info = context.addFunctionInfo(function);
    for (auto &&arg: arguments) {
        info->argTypes.push_back(arg.type());
    }
    info->returnType = type;

    auto subprogram = context.debug().createFunction(token(), id, arguments);
    function->setSubprogram(subprogram);
    context.debug().pushScope(subprogram);
    context.debug().setLocation(token());

    StackAllocator allocator(context);

    if (block) {
        llvm::BasicBlock *bblock = llvm::BasicBlock::Create(context.context(), "entry", function, 0);

        context.pushBlock(bblock, function, nullptr);

        llvm::Function::arg_iterator argsValues = function->arg_begin();
        auto typesIt = argTypes.begin();
        for (auto it = arguments.begin(); it != arguments.end(); it++, typesIt++) {
            auto argumentValue = &*argsValues++;
            argumentValue->setName(it->name().c_str());

            auto value = it->type().create(context, &allocator, it->name(), createValue(context, argumentValue, it->type()));
            value.setBindingPoint(ValueBindingPoint(it->type()));
            value.setMutable(it->isMutable());
            context.storeLocal(it->name(), value);
        }

        block->codeGen(context);
        if (!context.currentBlock()->returned) {
            context.builder().CreateRetVoid();
        }

        context.popBlock();
    }
    return {};
}

NStructDeclaration::NStructDeclaration(const std::string &id, Flags f)
                  : id(id)
                  , m_type(StructType(id))
                  , m_hasBody(false)
                  , m_flags(f)
{
}

Optional<Value> NStructDeclaration::codeGen(CodeGenContext &context)
{
    auto type = static_cast<llvm::StructType *>(m_type.get(context));
    if (!m_hasBody) {
        return {};
    }

    std::cout << "Creating struct declaration " << id << '\n';

    if (context.structInfo(id)) {
        err(token(), "struct '{}' was already declared", id);
    }

    std::vector<llvm::Type *> argTypes;
    for (auto it = elements.begin(); it != elements.end(); it++) {
        auto t = it->type;
        argTypes.push_back(t.get(context));
        std::cout<<"    with arg " << it->type.name() << " " <<it->name<<"\n";
    }
    type->setBody(argTypes);

    auto info = context.newStructType(type);
    info->type = m_type;
    info->fields.reserve(elements.size());
    for (auto &&el: elements) {
        info->fields.push_back({ el.name, el.mut, el.type });
    }

    if (m_flags & Flags::AbiSafe) {
        auto sizeType = Type(IntegerType(true, 32));
        llvm::Constant *sizeValue = nullptr;
        if (m_flags & Flags::MasterDefinition) {
            llvm::DataLayout layout(&context.module());
            int s = layout.getTypeSizeInBits(type) / 8;
            sizeValue = llvm::ConstantInt::get(sizeType.get(context), s, true);
        }
        using namespace std::string_literals;
        auto name = "__struct__"s + id + "__size"s;
        auto size = new llvm::GlobalVariable(context.module(), sizeType.get(context), false, llvm::GlobalValue::ExternalLinkage, sizeValue, name.c_str());
        info->sizeValue = createValue(context, size, sizeType);
    }

    return {};
}

NUnionDeclaration::NUnionDeclaration(const std::string &id, std::vector<Field> &e)
                 : id(id)
                 , m_type(StructType(id))
{
    std::swap(e, elements);
}

Optional<Value> NUnionDeclaration::codeGen(CodeGenContext &context)
{
    std::cout << "Creating union declaration " << " " << id << '\n';
    std::vector<llvm::Type *> argTypes;
    int size = 0;
    llvm::Type *argType = nullptr;
    llvm::DataLayout layout(&context.module());
    for (auto it = elements.begin(); it != elements.end(); it++) {
        auto t = it->type.get(context);
        if (t->isSized()) {
            int s = layout.getTypeSizeInBits(t);
            if (s > size) {
                size = s;
                argType = t;
            }
        }
    }

    argTypes.push_back(argType);

    auto type = static_cast<llvm::StructType *>(m_type.get(context));
    type->setBody(argTypes);
    auto info = context.newUnionType(type);
    info->type = type;
    info->fields.reserve(elements.size());
    for (auto &&el: elements) {
        info->fields.push_back({ el.name, el.mut, el.type });
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
        auto t = par.get(context);
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
    auto cond = condition()->codeGen(ctx);
    auto condfc = cond->getSpecialization<FirstClassValue>();
    auto condValue = condfc->load(ctx);

    if (condValue->getType()->isPointerTy()) {
        condValue = ctx.builder().CreateIsNotNull(condValue);
    }

    auto curBlock = ctx.currentBlock();
    llvm::BasicBlock *ifblock = llvm::BasicBlock::Create(ctx.context(), "if", curBlock->function, 0);
    llvm::BasicBlock *elseblock = elseBlock() ? llvm::BasicBlock::Create(ctx.context(), "else", curBlock->function, 0) : nullptr;
    llvm::BasicBlock *afterblock = llvm::BasicBlock::Create(ctx.context(), "endif", curBlock->function, 0);

    ctx.pushBlock(ifblock, curBlock->function, curBlock);
    condition()->pushConstraints(false);
    block()->codeGen(ctx);
    bool ifReturned = ctx.currentBlock()->returned;
    if (!ifReturned) {
        ctx.builder().CreateBr(afterblock);
    }
    condition()->popConstraints();
    ctx.popBlock();

    if (ifReturned) {
        condition()->pushConstraints(true);
    }

    if (elseBlock()) {
        ctx.pushBlock(elseblock, curBlock->function, curBlock);
        if (!ifReturned) {
            condition()->pushConstraints(true);
        }
        elseBlock()->codeGen(ctx);
        if (!ctx.currentBlock()->returned) {
            ctx.builder().CreateBr(afterblock);
        }
        if (!ifReturned) {
            condition()->popConstraints();
        }
        ctx.popBlock();
    }

    ctx.builder().CreateCondBr(condValue, ifblock, elseblock ? elseblock : afterblock);

    ctx.currentBlock()->block = afterblock;
    ctx.builder().SetInsertPoint(afterblock);

    return {};
}

Optional<Value> NWhileStatement::codeGen(CodeGenContext &ctx)
{
    auto curBlock = ctx.currentBlock();
    llvm::BasicBlock *condblock = llvm::BasicBlock::Create(ctx.context(), "cond", curBlock->function, 0);
    llvm::BasicBlock *whileblock = llvm::BasicBlock::Create(ctx.context(), "while", curBlock->function, 0);
    llvm::BasicBlock *afterblock = llvm::BasicBlock::Create(ctx.context(), "endwhile", curBlock->function, 0);

    ctx.pushBlock(condblock, curBlock->function, curBlock);
    auto cond = condition()->codeGen(ctx)->getSpecialization<FirstClassValue>()->load(ctx);
    ctx.builder().CreateCondBr(cond, whileblock, afterblock);
    ctx.popBlock();

    ctx.pushBlock(whileblock, curBlock->function, curBlock);
    block()->codeGen(ctx);
    if (!ctx.currentBlock()->returned) {
        ctx.builder().CreateBr(condblock);
    }
    ctx.popBlock();

    ctx.builder().CreateBr(condblock);

    ctx.currentBlock()->block = afterblock;
    ctx.builder().SetInsertPoint(afterblock);

    return {};
}

Optional<Value> NExternVariableDeclaration::codeGen(CodeGenContext &ctx)
{
    if (auto v = ctx.global(name())) {
        return v;
    }
    auto ty = type().get(ctx);
    auto val = new llvm::GlobalVariable(ctx.module(), ty, false, llvm::GlobalValue::ExternalLinkage, nullptr, name().c_str());
    auto value = createValue(ctx, val, type());
    ctx.storeGlobal(name(), value);
    return value;
}

Optional<Value> NEnumDeclaration::codeGen(CodeGenContext &ctx)
{
    class EnumValue : public ValueSpecialization, public NamedAggregateValue
    {
    public:
        std::vector<Value> values;
        std::unordered_map<std::string, int> names;
        Type m_type;

        EnumValue(CodeGenContext &ctx, NEnumDeclaration *decl)
            : m_type(decl->m_type)
        {
            int i = 0;
            for (auto &&e: decl->m_entries) {
                values.push_back(FirstClassValue(llvm::ConstantInt::get(decl->m_type.get(ctx), e.value, true), decl->m_type));
                names[e.name] = i++;
            }
        }

        Type &type()
        {
            return m_type;
        }
        const Type &type() const { return m_type; }

        Value extract(int id) const { return values[id]; }
        Value extract(const std::string &name) const
        {
            auto id = names.find(name)->second;
            return values[id];
        }
    };

    if (m_name.empty()) {
        for (auto &&e: m_entries) {
            ctx.storeGlobal(e.name, constantInteger(ctx, e.value, m_type));
        }
    } else {
        ctx.storeGlobal(m_name, EnumValue(ctx, this));
    }

    return {};
}

Optional<Value> NCastExpression::codeGen(CodeGenContext &ctx)
{
    auto value = m_expression->codeGen(ctx);
    fmt::print("cast {} to {}\n", value->type().name(), m_type.name());

    if (auto firstclass = value->getSpecialization<FirstClassValue>()) {
        auto type = firstclass->type().get(ctx);
        auto castType = m_type.get(ctx);

        if (type->isIntegerTy()) {
            if (castType->isIntegerTy()) {
                fmt::print("int \n");
                return createValue(ctx, ctx.builder().CreateTrunc(firstclass->load(ctx), m_type.get(ctx)), m_type);
            } else if (castType->isFloatingPointTy()) {
                auto val = ctx.builder().CreateSIToFP(firstclass->load(ctx), m_type.get(ctx));
                val->getType()->dump();
                return createValue(ctx, val, m_type);
            }
        } else if (type->isFloatingPointTy()) {
            if (castType->isIntegerTy()) {
                firstclass->value()->dump();
                auto val = ctx.builder().CreateFPToSI(firstclass->load(ctx), m_type.get(ctx));
                return createValue(ctx, val, m_type);
            }
        }
    }

    err(token(), "cannot cast value from type '{}' to '{}'", value->type().name(), m_type.name());
    return {};
}
