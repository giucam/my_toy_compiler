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
     , m_file(m_builder.createFile(filename.data(), "."))
     , m_cunit(m_builder.createCompileUnit(llvm::dwarf::DW_LANG_C, m_file, "PinkPig", 0, "", 0))
     , m_scope(nullptr)
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
    return m_builder.createFunction(unit, funcName, llvm::StringRef(), unit, token.lineNo(), functionTy, false, true, token.lineNo(), (llvm::DINode::DIFlags)0, false);
}

void Debug::createVariable(const Token &token, const std::string &name, const Value &value)
{
    auto file = fileUnit(token.filename());
    auto ty = m_builder.createUnspecifiedType(value.type().name());
    auto var = m_builder.createAutoVariable(m_scope, name, file, token.lineNo(), ty);

    if (auto fc = value.getSpecialization<FirstClassValue>()) {
        m_builder.insertDeclare(fc->value(), var, m_builder.createExpression(), llvm::DebugLoc::get(token.lineNo(), 0, m_scope), m_ctx->currentBlock()->block);
    }
}

void Debug::pushScope(llvm::DIScope *scope)
{
    m_scopes.push_back(scope);
    m_scope = scope;
}

void Debug::popScope()
{
    m_scopes.pop_back();
    m_scope = m_scopes.back();
}

void Debug::setGlobalScope()
{
    m_scope = m_scopes.front();
}

void Debug::restoreScope()
{
    m_scope = m_scopes.back();
}

void Debug::setLocation(const Token &token)
{
    if (token.lineNo() >= 0) {
        m_ctx->builder().SetCurrentDebugLocation(llvm::DebugLoc::get(token.lineNo(), token.columnNo(), m_scope));
    }
}


CodeGenContext::CodeGenContext(const std::string &name)
              : Stage()
              , m_module(std::make_unique<llvm::Module>(name.c_str(), m_context))
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
    root.visit(*this, 1); // forward declare all struct, unions
    root.visit(*this, 2); // define structs, unions and generate function prototypes
    root.visit(*this, 3); // define functions and all code
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
    return !module().getFunction(name.c_str());
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

void CodeGenContext::storeLocal(const std::string &name, Value value)
{
    currentBlock()->locals[name] = value;
}

Optional<Value> CodeGenBlock::local(const std::string &name) const
{
    auto it = locals.find(name);
    if (it == locals.end()) {
        return {};
    }
    return {it->second};
}

Optional<Value> CodeGenContext::local(const std::string &name) const
{
    auto block = currentBlock();
    do {
        auto val = block->local(name);
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

int CodeGenContext::typeSize(const Type &t)
{
    auto elmType = t.get(*this);
    llvm::DataLayout layout(&module());
    return layout.getTypeSizeInBits(elmType);
}

bool CodeGenContext::isFunctionDefined(const std::string &name, const std::vector<Type> &args) const
{
    return module().getFunction(name.c_str());
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

Optional<Value> CodeGenContext::visit(NInteger &integer, int pass)
{
    std::cout << "Creating integer: " << integer.value << '\n';
    return constantInteger(*this, integer.value);
}

Optional<Value> CodeGenContext::visit(NBoolean &b, int pass)
{
    auto v = llvm::ConstantInt::get(llvm::Type::getInt1Ty(context()), b.value, true);
    return simpleValue(v, llvmType(*this, v->getType()));
}

Optional<Value> CodeGenContext::visit(NDouble &d, int pass)
{
    std::cout << "Creating double: " << d.value << '\n';

    auto v = llvm::ConstantFP::get(llvm::Type::getDoubleTy(context()), d.value);
    return simpleValue(v, FloatingType(64));
}

Optional<Value> CodeGenContext::visit(NString &str, int pass)
{
    std::cout << "Creating string: \"" << str.value << "\", (";
    for (auto it = str.value.begin(); it != str.value.end();) {
        char c = *it;
        if (c == '\n') std::cout << "\\n";
        else std::cout << c;
        if (++it != str.value.end()) {
            std::cout << ", ";
        } else {
            break;
        }
    }
    std::cout << ")\n";
    auto v = builder().CreateGlobalStringPtr(str.value, ".str");
    return simpleValue(v, llvmType(*this, v->getType()));
}

Optional<Value> CodeGenContext::visit(NIdentifier &id, int pass)
{
    if (id.context()) {
        auto parent = id.context()->visit(*this, pass);
        Value value;
        if (id.type == NIdentifier::Index) {
            if (auto v = parent->getSpecialization<AggregateValue>()) {
                value = v->extract(id.index);
            } else {
                internalError();
            }
        } else {
            if (auto v = parent->getSpecialization<NamedAggregateValue>()) {
                value = v->extract(id.name);
            } else {
                internalError();
            }
        }
        return value;
    }

    auto val = local(id.name);
    if (!val) {
        val = global(id.name);
    }
    if (!val) {
        internalError();
    }

    return val;
}

Optional<Value> CodeGenContext::visit(NAddressOfExpression &addr, int pass)
{
    auto val = addr.expression()->visit(*this, pass);

    if (auto v = val->getSpecialization<FirstClassValue>()) {
        auto value = createValue(*this, v->value(), v->type().getPointerTo());
        value.setBindingPoint(ValueBindingPoint(val->bindingPoint()->type().getPointerTo()));

        return value;
    }
    err(addr.token(), "taking address of non first type value");
    return {};
}

static std::string mangleFuncName(const std::string &name, const std::string &iface, const std::string &sig)
{
    return iface + sig + "::" + name;
}

Optional<Value> CodeGenContext::visit(NExpressionPack &pack, int pass)
{
    std::cout << "Creating tuple declaration " << pack.expressionList().size()<<"\n";

    if (pack.expressionList().size() == 1) {
        return pack.expressionList().front()->visit(*this, pass);
    }

    std::vector<Value> values;
    for (auto &&expr: pack.expressionList()) {
        auto vals = expr->visit(*this, pass);
        values.push_back(vals);
    }
    return valuePack(values);
}

static std::string dump(llvm::Type *t)
{
    std::string str;
    llvm::raw_string_ostream s(str);
    t->print(s);
    s.flush();
    return str;
};

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

Optional<Value> CodeGenContext::visit(NMethodCall &call, int pass)
{
    std::cout << "Creating method call: " << call.name << '\n';

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

    if (call.context()) {
        auto val = call.context()->visit(*this, pass);
        add(val);
    }
    for (auto &&arg: call.arguments) {
        auto value = arg->visit(*this, pass);
        add(value);
    }

    if (call.name == "count") {
        // if we have more than one argument return the number of them, otherwise do nothing, in
        // case there is a function named count taking that argument
        if (values.size() > 1) {
            return constantInteger(*this, values.size());
        }
    } else if (call.name == "size") {
        if (values.size() != 1) {
            err(call.token(), "the built-in function 'size' accepts only one argument");
        }
        auto t = values.front().type().get(*this);
        if (auto info = structInfo(t)) {
            return info->sizeValue;
        }
        if (t->isSized()) {
            llvm::DataLayout layout(&module());
            int s = layout.getTypeSizeInBits(t) / 8;
            return constantInteger(*this, s);
        }
        return constantInteger(*this, 0);
    }

    llvm::Function *function = module().getFunction(call.name.c_str());

    if (!function) {
        function = findInterfaceImpl(call.token(), call.name, firstclasses, *this);
    }

    if (!function) {
        auto mangledName = call.name;
        for (auto &&v: values) {
            mangledName += "_";
            mangledName += v.type().typeName();
        }

        function = module().getFunction(mangledName.c_str());
    }

    if (function == NULL) {
        err(call.token(), "no such function '{}'", call.name);
    }

    std::vector<llvm::Value *> vals;

    size_t i = 0;
    auto info = functionInfo(function);
    for (auto it = function->arg_begin(); it != function->arg_end(); ++it, ++i) {
        auto converted = convertTo(firstclasses[i]->value(), firstclasses[i]->type(), info->argTypes[i]);
        if (!converted) {
            err(call.token(), "wrong argument type to function; expected '{}', found '{}'", info->argTypes[i].name(), firstclasses[i]->type().name());
        }

        vals.push_back(converted);
    }
    if (function->isVarArg()) {
        for (; i < values.size(); ++i) {
            // varargs, we need to do a load here
            vals.push_back(loadValue(firstclasses[i]->value(), *this));
        }
    } else {
        size_t numArgs = i;
        if (numArgs < values.size()) {
            err(call.token(), "too many arguments to function");
        }
    }

    debug().setLocation(call.token());

    llvm::Value *callValue = builder().CreateCall(function, llvm::makeArrayRef(vals));
    return createValue(*this, callValue, info->returnType);
}

Optional<Value> CodeGenContext::visit(NAssignment &ass, int pass)
{
//     std::cout << "Creating assignment for " << lhs->name << " "<<this<<'\n';

    auto lhsValue = ass.lhs->visit(*this, pass);
    auto rhsValue = ass.rhs->visit(*this, pass);

    if (!lhsValue->isMutable()) {
        err(ass.token(), "attempting to assign to non-mutable variable");
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
    auto rhsVal = convertTo(rhsFirstClass->value(), rhsFirstClass->type(), valueType);
    auto lhsVal = lhsFirstClass->value();
    // if that failed, attempt to dereference the left hand side until it matches what is provided on the right hand side
    if (!rhsVal) {
        auto rt = rhsFirstClass->type();
        rt.setTypeConstraint(TypeConstraint());
        lhsVal = convertTo(lhsFirstClass->value(), lhsFirstClass->type(), rt.getPointerTo());

        if (lhsVal && canStoreInto(*this, valueType, rhsFirstClass->type().getPointerTo()) ) {
            rhsVal = rhsFirstClass->value();
        }
    }
    if (!rhsVal) {
        err(ass.token(), "cannot store a value of type '{}' to a binding point of type '{}'", rhsValue->type().name(), valueType.name());
    }

    debug().setLocation(ass.token());
    builder().CreateStore(rhsVal, lhsVal);
    lhsFirstClass->type().setTypeConstraint(rhsFirstClass->type().typeConstraint());

    return lhsValue;
}

Optional<Value> CodeGenContext::visit(NBlock &block, int pass)
{
    StatementList::const_iterator it;
    for (it = block.statements.begin(); it != block.statements.end(); it++) {
        std::cout << "block: Generating code for " << typeid(**it).name() << '\n';
        (**it).visit(*this, pass);
        if (currentBlock()->returned) {
            break;
        }
    }
    std::cout << "Creating block" << '\n';
    return {};
}

Optional<Value> CodeGenContext::visit(NExpressionStatement &stat, int pass)
{
    std::cout << "Generating code for " << typeid(stat.expression()).name() << '\n';
    return stat.expression()->visit(*this, pass);
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

    if (baseDest == value->getType()) {
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
            }
        }

        internalError();
    }

    while (srcPointer > destPointer) {
        value = builder().CreateLoad(value);
        --srcPointer;
    }

    if (destPointer == srcPointer) {
        return value;
    }

    return nullptr;
}

Optional<Value> CodeGenContext::visit(NReturnStatement &ret, int pass)
{
    auto returnValue = ret.expression() ? ret.expression()->visit(*this, pass) : Optional<Value>();

    auto function = currentBlock()->function;
    auto retType = functionInfo(function)->returnType;

    currentBlock()->returned = true;

    llvm::Value *llvmval = nullptr;
    if (returnValue) {
        auto firstclass = returnValue->getSpecialization<FirstClassValue>();
        llvm::Type *type = nullptr;

        if (firstclass) {
            type = firstclass->value()->getType();
            llvmval = convertTo(firstclass->value(), firstclass->type(), retType);

        } else if (auto tupleType = retType.getSpecialization<TupleType>()) {
            auto types = tupleType->unpack();

            auto vals = returnValue->getSpecialization<PackValue>()->unpack();

            if (types.size() == vals.size()) {
                auto typeIt = types.begin();
                std::vector<llvm::Value *> values;
                for (auto &&v: vals) {
                    auto fc = v.getSpecialization<FirstClassValue>();
                    auto value = convertTo(fc->value(), fc->type(), *typeIt++);
                    if (!value) {
                        break;
                    }
                    values.push_back(value);
                }
                llvmval = makeTupleValue(values, "ret");
                type = llvmval->getType();
                llvmval = convertTo(llvmval, llvmType(*this, type), retType);


            }
        }
        if (!llvmval) {
            err(ret.token(), "wrong return type for function. found '{}', expected '{}'", typeName(type), retType.name());
        }
    }

    auto v = builder().CreateRet(llvmval);
    return simpleValue(v, llvmType(*this, v->getType()));
}

Optional<Value> CodeGenContext::visit(NVariableDeclaration &var, int pass)
{
    std::cout << "Creating variable declaration " << var.id.name() << " "<< var.id.isMutable()<< '\n';

    debug().setLocation(var.token());

    if (!var.expression && !var.declaredType.isValid()) {
        err(var.token(), "missing type or initializer when declaring variable '{}'", var.id.name());
    }
    auto init = var.expression->visit(*this, pass);

    StackAllocator allocator(*this);

    Type varType = var.declaredType.isValid() ? var.declaredType : init->type();

    Value value = [&]() {
        try {
            auto value = varType.create(*this, &allocator, var.id.name(), init);
            return value;
        } catch (const CreateError &error) {
            switch (error.error) {
                case CreateError::Err::TypeError:
                    err(var.token(), "cannot create a variable of type '{}'", varType.name());
                case CreateError::Err::StoreError:
                    err(var.token(), "mismatched type: assigning value of type '{}' to a variable of type '{}'", init->type().name(), varType.name());
            }
            err(var.token(), "unk");
        }
        return Value();
    }();

    if (var.declaredType.isValid()) {
        auto bp = init->bindingPoint()->type();
        bp.setTypeConstraint(TypeConstraint());

        if (bp.isValid() && !canStoreInto(*this, bp, var.declaredType)) {
            err(var.token(), "cannot take a pointer of type '{}' to type '{}': type constraints not respected", var.declaredType.name(), bp.name());
        }
        value.setBindingPoint(ValueBindingPoint(var.declaredType));
    }

    value.setMutable(var.id.isMutable());
    debug().createVariable(var.token(), var.id.name(), value);

    storeLocal(var.id.name(), value);

    return value;
}

Optional<Value> CodeGenContext::visit(NMultiVariableDeclaration &var, int pass)
{
    fmt::print("\n\nMULTI\n\n");
    auto source = var.expression()->visit(*this, pass);
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

    StackAllocator allocator(*this);
    size_t i = 0;
    for (auto &&name: var.names()) {
        auto val = getValue(i);
        bool lastName = i == var.names().size() - 1;

        Value value;
        if (val) {
            if (lastName && values.size() > i + 1) {

                std::vector<Value> vals;
                for (size_t j = i; j < values.size(); ++j) {
                    vals.push_back(values[j].type().create(*this, &allocator, name.name(), values[j]));
                }
                value = valuePack(vals);
                value.setMutable(name.isMutable());
            } else {
                value = val->type().create(*this, &allocator, name.name(), *val);
                value.setMutable(name.isMutable());
            }
        } else {
            std::vector<Value> vec;
            value = valuePack(vec);
        }

        fmt::print("NEW MULTI {}\n",name.name());

        storeLocal(name.name(), value);
        ++i;
    }
    return {};
}

Optional<Value> CodeGenContext::visit(NExternDeclaration &ex, int pass)
{
    if (pass == 1) {
        return {};
    }

    if (pass == 2) {
        std::vector<llvm::Type *> argTypes;
        argTypes.reserve(ex.arguments().size());
        for (auto &&arg: ex.arguments()) {
            argTypes.push_back(arg.type().get(*this));
        }
        llvm::FunctionType *ftype = llvm::FunctionType::get(ex.returnType().get(*this), llvm::makeArrayRef(argTypes), ex.isVarargs());
        auto function = llvm::Function::Create(ftype, llvm::GlobalValue::ExternalLinkage, ex.name().c_str(), &module());

        auto info = addFunctionInfo(function);
        for (auto &&arg: ex.arguments()) {
            info->argTypes.push_back(arg.type());
        }
        info->returnType = ex.returnType();
    }

    return {};
}

Optional<Value> CodeGenContext::visit(NFunctionDeclaration &fun, int pass)
{
    if (pass == 1) {
        return {};
    }

    std::cout << "Creating function: " << fun.id << pass << '\n';

    std::vector<llvm::Type *> argTypes;
    for (auto it = fun.arguments.begin(); it != fun.arguments.end(); it++) {
        if (auto pack = it->type().getSpecialization<ArgumentPackType>()) {
            for (auto &&t: pack->types()) {
                argTypes.push_back(t.get(*this));
            }
        } else {
            argTypes.push_back(it->type().get(*this));
        }
    }

    if (pass == 2) {
        auto retType = fun.type.get(*this);
        llvm::FunctionType *ftype = llvm::FunctionType::get(retType, llvm::makeArrayRef(argTypes), false);
        llvm::Function *function = llvm::Function::Create(ftype, llvm::GlobalValue::ExternalLinkage, fun.id.c_str(), &module());

        auto info = addFunctionInfo(function);
        for (auto &&arg: fun.arguments) {
            if (auto pack = arg.type().getSpecialization<ArgumentPackType>()) {
                for (auto &&t: pack->types()) {
                    info->argTypes.push_back(t);
                }
            } else {
                info->argTypes.push_back(arg.type());
            }
        }
        info->returnType = fun.type;

        return {};
    }
    auto function = module().getFunction(fun.id.c_str());

    auto subprogram = debug().createFunction(fun.token(), fun.id, fun.arguments);
    function->setSubprogram(subprogram);
    debug().pushScope(subprogram);
    debug().setLocation(fun.token());

    StackAllocator allocator(*this);
    auto allocBlock = allocationBlock();

    if (fun.block) {
        llvm::BasicBlock *bblock = llvm::BasicBlock::Create(context(), "entry", function, 0);

        pushBlock(bblock, function, nullptr);

        llvm::Function::arg_iterator argsValues = function->arg_begin();
        auto typesIt = argTypes.begin();
        for (auto it = fun.arguments.begin(); it != fun.arguments.end(); it++, typesIt++) {
            if (auto pack = it->type().getSpecialization<ArgumentPackType>()) {
                int num = pack->types().size();
                std::vector<Value> values;
                for (int i = 0; typesIt != argTypes.end() && i < num; ++i, ++typesIt) {
                    auto argumentValue = &*argsValues++;
                    argumentValue->setName(it->name().c_str());

                    Type type = llvmType(*this, *typesIt);
                    values.push_back(type.create(*this, &allocator, it->name(), createValue(*this, argumentValue, type)));
                }
                auto value = valuePack(values);
                storeLocal(it->name(), value);
                break;
            } else {
                auto argumentValue = &*argsValues++;
                argumentValue->setName(it->name().c_str());

                auto value = it->type().create(*this, &allocator, it->name(), createValue(*this, argumentValue, it->type()));
                value.setBindingPoint(ValueBindingPoint(it->type()));
                value.setMutable(it->isMutable());
                debug().createVariable(fun.token(), it->name(), value);
                storeLocal(it->name(), value);
            }
        }

        fun.block->visit(*this, pass);
        if (!currentBlock()->returned) {
            builder().CreateRetVoid();
        }

        popBlock();
    }

    debug().popScope();
    setAllocationBlock(allocBlock);
    return {};
}

NStructDeclaration::NStructDeclaration(const std::string &id, Flags f)
                  : id(id)
                  , m_type(StructType(id))
                  , m_hasBody(false)
                  , m_flags(f)
{
    init(this);
}

Optional<Value> CodeGenContext::visit(NStructDeclaration &str, int pass)
{
    auto type = static_cast<llvm::StructType *>(str.m_type.get(*this));
    if (pass != 2 || !str.m_hasBody) {
        return {};
    }

    std::cout << "Creating struct declaration " << str.id << '\n';

    if (structInfo(str.id)) {
        err(str.token(), "struct '{}' was already declared", str.id);
    }

    std::vector<llvm::Type *> argTypes;
    for (auto it = str.elements.begin(); it != str.elements.end(); it++) {
        auto t = it->type;
        argTypes.push_back(t.get(*this));
        std::cout<<"    with arg " << it->type.name() << " " <<it->name<<"\n";
    }
    type->setBody(argTypes);

    auto info = newStructType(type);
    info->type = str.m_type;
    info->fields.reserve(str.elements.size());
    for (auto &&el: str.elements) {
        info->fields.push_back({ el.name, el.mut, el.type });
    }

    if (str.m_flags & NStructDeclaration::Flags::AbiSafe) {
        auto sizeType = Type(IntegerType(true, 32));
        llvm::Constant *sizeValue = nullptr;
        if (str.m_flags & NStructDeclaration::Flags::MasterDefinition) {
            llvm::DataLayout layout(&module());
            int s = layout.getTypeSizeInBits(type) / 8;
            sizeValue = llvm::ConstantInt::get(sizeType.get(*this), s, true);
        }
        using namespace std::string_literals;
        auto name = "__struct__"s + str.id + "__size"s;
        auto size = new llvm::GlobalVariable(module(), sizeType.get(*this), false, llvm::GlobalValue::ExternalLinkage, sizeValue, name.c_str());
        info->sizeValue = createValue(*this, size, sizeType);
    }

    return {};
}

NUnionDeclaration::NUnionDeclaration(const std::string &id, std::vector<Field> &e)
                 : id(id)
                 , m_type(StructType(id))
{
    init(this);
    std::swap(e, elements);
}

Optional<Value> CodeGenContext::visit(NUnionDeclaration &un, int pass)
{
    std::cout << "Creating union declaration " << " " << un.id << '\n';

    auto type = static_cast<llvm::StructType *>(un.m_type.get(*this));
    if (pass != 2) {
        return {};
    }

    std::vector<llvm::Type *> argTypes;
    int size = 0;
    llvm::Type *argType = nullptr;
    llvm::DataLayout layout(&module());
    for (auto it = un.elements.begin(); it != un.elements.end(); it++) {
        auto t = it->type.get(*this);
        if (t->isSized()) {
            int s = layout.getTypeSizeInBits(t);
            if (s > size) {
                size = s;
                argType = t;
            }
        }
    }

    argTypes.push_back(argType);


    type->setBody(argTypes);
    auto info = newUnionType(type);
    info->type = type;
    info->fields.reserve(un.elements.size());
    for (auto &&el: un.elements) {
        info->fields.push_back({ el.name, el.mut, el.type });
    }
    return {};
}

Optional<Value> CodeGenContext::visit(NIfaceDeclaration &iface, int pass)
{
    addInterface(&iface);
    if (iface.prototypes.size() == 0) {
        error("cannot create an empty interface");
    }
    for (auto &&p: iface.prototypes) {
        p->iface = &iface;
    }

    return {};
}

Optional<Value> CodeGenContext::visit(NImplDeclaration &impl, int pass)
{
    if (pass == 1) {

    NIfaceDeclaration *iface = interface(impl.name);
    if (!iface) {
        error("trying to implement unknown interface '{}'", impl.name);
    }

    for (auto &&p: iface->prototypes) {
        bool hasProto = false;
        for (auto &&f: impl.functions) {
            if (f->id == p->name) {
                hasProto = true;
                break;
            }
        }
        if (!hasProto) {
            error("missing implementation in interface '{}' for function '{}'", impl.name, p->name);
        }
    }
    for (auto &&f: impl.functions) {
        bool hasProto = false;
        for (auto &&p: iface->prototypes) {
            if (f->id == p->name) {
                hasProto = true;
                break;
            }
        }
        if (!hasProto) {
            error("unrelated function '{}' in implementation of interface '{}'", f->id, impl.name);
        }
    }

    iface->implementations.push_back(&impl);

    llvm::raw_string_ostream stream(impl.id);
    for (auto &&par: impl.parameters) {
        auto t = par.get(*this);
        t->print(stream);
        impl.parameterTypes.push_back(t);
    }
    stream.flush();

    for (auto &&func: impl.functions) {
        func->id = mangleFuncName(func->id, iface->name, impl.id);
    }
    }

    for (auto &&func: impl.functions) {
        func->visit(*this, pass);
    }

    return {};
}

Optional<Value> CodeGenContext::visit(NIfStatement &ifs, int pass)
{
    auto cond = ifs.condition()->visit(*this, pass);
    auto condfc = cond->getSpecialization<FirstClassValue>();
    auto condValue = condfc->load(*this);

    if (condValue->getType()->isPointerTy()) {
        condValue = builder().CreateIsNotNull(condValue);
    }

    auto curBlock = currentBlock();
    llvm::BasicBlock *ifblock = llvm::BasicBlock::Create(context(), "if", curBlock->function, 0);
    llvm::BasicBlock *elseblock = ifs.elseBlock() ? llvm::BasicBlock::Create(context(), "else", curBlock->function, 0) : nullptr;
    llvm::BasicBlock *afterblock = llvm::BasicBlock::Create(context(), "endif", curBlock->function, 0);

    pushBlock(ifblock, curBlock->function, curBlock);
    ifs.condition()->pushConstraints(false);
    ifs.block()->visit(*this, pass);
    bool ifReturned = currentBlock()->returned;
    if (!ifReturned) {
        builder().CreateBr(afterblock);
    }
    ifs.condition()->popConstraints();
    popBlock();

    if (ifReturned) {
        ifs.condition()->pushConstraints(true);
    }

    if (ifs.elseBlock()) {
        pushBlock(elseblock, curBlock->function, curBlock);
        if (!ifReturned) {
            ifs.condition()->pushConstraints(true);
        }
        ifs.elseBlock()->visit(*this, pass);
        if (!currentBlock()->returned) {
            builder().CreateBr(afterblock);
        }
        if (!ifReturned) {
            ifs.condition()->popConstraints();
        }
        popBlock();
    }

    builder().CreateCondBr(condValue, ifblock, elseblock ? elseblock : afterblock);

    currentBlock()->block = afterblock;
    builder().SetInsertPoint(afterblock);

    return {};
}

Optional<Value> CodeGenContext::visit(NWhileStatement &ws, int pass)
{
    auto curBlock = currentBlock();
    llvm::BasicBlock *condblock = llvm::BasicBlock::Create(context(), "cond", curBlock->function, 0);
    llvm::BasicBlock *whileblock = llvm::BasicBlock::Create(context(), "while", curBlock->function, 0);
    llvm::BasicBlock *afterblock = llvm::BasicBlock::Create(context(), "endwhile", curBlock->function, 0);

    pushBlock(condblock, curBlock->function, curBlock);
    debug().setLocation(ws.token());
    auto cond = ws.condition()->visit(*this, pass)->getSpecialization<FirstClassValue>()->load(*this);
    builder().CreateCondBr(cond, whileblock, afterblock);
    popBlock();

    pushBlock(whileblock, curBlock->function, curBlock);
    ws.condition()->pushConstraints(false);
    bool rootBlock = allocationBlock() == nullptr;
    if (rootBlock) {
        setAllocationBlock(curBlock);
    }
    ws.block()->visit(*this, pass);
    if (rootBlock) {
        setAllocationBlock(nullptr);
    }
    if (!currentBlock()->returned) {
        builder().CreateBr(condblock);
    }
    ws.condition()->popConstraints();
    popBlock();

    builder().CreateBr(condblock);

    currentBlock()->block = afterblock;
    builder().SetInsertPoint(afterblock);

    return {};
}

Optional<Value> CodeGenContext::visit(NExternVariableDeclaration &var, int pass)
{
    if (pass != 2) {
        return {};
    }

    if (auto v = global(var.name())) {
        return v;
    }
    auto ty = var.type().get(*this);
    auto val = new llvm::GlobalVariable(module(), ty, false, llvm::GlobalValue::ExternalLinkage, nullptr, var.name().c_str());
    auto value = createValue(*this, val, var.type());
    storeGlobal(var.name(), value);
    return value;
}

Optional<Value> CodeGenContext::visit(NEnumDeclaration &en, int pass)
{
    if (pass != 2) {
        return {};
    }

    class EnumValue : public ValueSpecialization, public NamedAggregateValue
    {
    public:
        std::vector<Value> values;
        std::unordered_map<std::string, int> names;
        Type m_type;

        EnumValue(CodeGenContext &ctx, NEnumDeclaration *decl)
            : m_type(decl->type())
        {
            int i = 0;
            for (auto &&e: decl->entries()) {
                values.push_back(FirstClassValue(llvm::ConstantInt::get(decl->type().get(ctx), e.value, true), decl->type()));
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

    if (en.name().empty()) {
        for (auto &&e: en.entries()) {
            storeGlobal(e.name, constantInteger(*this, e.value, en.type()));
        }
    } else {
        storeGlobal(en.name(), EnumValue(*this, &en));
    }

    return {};
}

Optional<Value> CodeGenContext::visit(NCastExpression &cast, int pass)
{
    auto value = cast.expression()->visit(*this, pass);
    fmt::print("cast {} to {}\n", value->type().name(), cast.type().name());

    if (auto firstclass = value->getSpecialization<FirstClassValue>()) {
        auto type = firstclass->type().get(*this);
        auto castType = cast.type().get(*this);

        if (type->isIntegerTy()) {
            if (castType->isIntegerTy()) {
                if (static_cast<llvm::IntegerType *>(type)->getBitWidth() == 1) {
                    return createValue(*this, builder().CreateZExt(firstclass->load(*this), cast.type().get(*this)), cast.type());
                } else {
                    fmt::print("int \n");
                    return createValue(*this, builder().CreateTrunc(firstclass->load(*this), cast.type().get(*this)), cast.type());
                }
            } else if (castType->isFloatingPointTy()) {
                auto val = builder().CreateSIToFP(firstclass->load(*this), cast.type().get(*this));
                return createValue(*this, val, cast.type());
            }
        } else if (type->isFloatingPointTy()) {
            if (castType->isIntegerTy()) {
                auto val = builder().CreateFPToSI(firstclass->load(*this), cast.type().get(*this));
                return createValue(*this, val, cast.type());
            }
        } else if (type->isPointerTy() && castType->isPointerTy()) {
            auto val = builder().CreateBitCast(firstclass->load(*this), cast.type().get(*this));
            return createValue(*this, val, cast.type());
        }
    }

    err(cast.token(), "cannot cast value from type '{}' to '{}'", value->type().name(), cast.type().name());
    return {};
}

NForStatement::NForStatement(const Token &itTok, const Token &exprTok, std::unique_ptr<NExpression> arrExpr, NBlock *block)
             : NStatement(itTok)
             , m_itToken(itTok)
             , m_exprToken(exprTok)
             , m_arrayExpr(std::move(arrExpr))
             , m_block(block)
{
    init(this);
}

Optional<Value> CodeGenContext::visit(NForStatement &fs, int pass)
{
    auto curBlock = currentBlock();

    auto arrayValue = fs.arrayExpr()->visit(*this, pass);
    auto arrType = arrayValue->type().getSpecialization<DynamicArrayType>();
    if (!arrType) {
        err(fs.exprToken(), "cannot loop over the given expression");
    }

    StackAllocator alloc(*this);

    auto elmType = arrType->elementType();
    auto it = elmType.create(*this, &alloc, fs.itToken().text());
    auto itValue = it.getSpecialization<FirstClassValue>()->value();
    storeLocal(fs.itToken().text(), it);

    //get a pointer to the start of the array and calculate a pointer to the end
    auto ptr = elmType.getPointerTo().create(*this, &alloc, "", arrType->dataPointer(*this, arrayValue));
    auto ptrValue = ptr.getSpecialization<FirstClassValue>()->value();
    auto count = arrType->count(*this, arrayValue);
    auto endPtr = builder().CreateInBoundsGEP(builder().CreateLoad(ptrValue), count.getSpecialization<FirstClassValue>()->value());

    llvm::BasicBlock *condblock = llvm::BasicBlock::Create(context(), "cond", curBlock->function, 0);
    llvm::BasicBlock *forblock = llvm::BasicBlock::Create(context(), "for", curBlock->function, 0);
    llvm::BasicBlock *afterblock = llvm::BasicBlock::Create(context(), "endfor", curBlock->function, 0);

    pushBlock(condblock, curBlock->function, curBlock);

    // if the pointer is equal to the end exit the loop
    auto cond = builder().CreateICmpNE(builder().CreateLoad(ptrValue), endPtr);
    builder().CreateCondBr(cond, forblock, afterblock);
    popBlock();

    pushBlock(forblock, curBlock->function, curBlock);

    //store the value into the iterator variable
    auto currPtr = builder().CreateLoad(ptrValue);
    builder().CreateStore(builder().CreateLoad(currPtr), itValue);

    bool rootBlock = allocationBlock() == nullptr;
    if (rootBlock) {
        setAllocationBlock(curBlock);
    }
    fs.block()->visit(*this, pass);
    if (rootBlock) {
        setAllocationBlock(nullptr);
    }
    // advance the pointer to the next element
    auto newPtr = builder().CreateInBoundsGEP(currPtr, builder().getInt32(1));
    ptrValue = builder().CreateStore(newPtr, ptrValue);
    if (!currentBlock()->returned) {
        builder().CreateBr(condblock);
    }

    popBlock();

    builder().CreateBr(condblock);

    currentBlock()->block = afterblock;
    builder().SetInsertPoint(afterblock);

    return {};
}

void CodeGenContext::inject(Node *node, InjectScope scope)
{
    if (scope == InjectScope::Global) {
        debug().setGlobalScope();

    }
    node->visit(*this, 2);
    if (scope == InjectScope::Global) {
        debug().restoreScope();
    }
}



NInitializerListExpression::NInitializerListExpression(const Token &tok, std::vector<Initializer> &inits)
                          : NExpression(tok)
{
    init(this);
    std::swap(m_initializers, inits);
}

Optional<Value> CodeGenContext::visit(NInitializerListExpression &in, int pass)
{
    std::vector<InitializerListValue::Initializer> inits;
    for (auto &&i: in.initializers()) {
        inits.push_back({ i.name, i.value->visit(*this, pass) });
    }
    return Value(InitializerListValue(inits));
}

Optional<Value> CodeGenContext::visit(NSizeofExpression &so, int pass)
{
    auto size = typeSize(so.type);
    return constantInteger(*this, size / 8);
}


llvm::Value *StackAllocator::allocate(llvm::Type *ty, const std::string &name)
{
    if (m_ctx.allocationBlock()) {
        if (auto val = m_ctx.allocationBlock()->local(name)) {
            return val->getSpecialization<FirstClassValue>()->value();
        }
        return new llvm::AllocaInst(ty, 0, name.c_str(), m_ctx.allocationBlock()->block);
    }

    return m_ctx.builder().CreateAlloca(ty, nullptr, name.c_str());
}

llvm::Value *StackAllocator::allocateSized(llvm::Type *ty, llvm::Value *sizeValue, const std::string &name)
{
    llvm::Value *alloc = m_ctx.builder().CreateAlloca(llvm::IntegerType::get(m_ctx.context(), 8), sizeValue);
    return m_ctx.builder().CreateBitCast(alloc, ty->getPointerTo(), name.c_str());
}
