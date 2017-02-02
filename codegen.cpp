#include <fstream>

#include <llvm/Support/raw_os_ostream.h>

#include "node.h"
#include "codegen.h"
// #include "parser.hpp"
#include "common.h"

CodeGenContext::CodeGenContext(const std::string &name)
              : m_module(std::make_unique<llvm::Module>(name.c_str(), m_context))
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
llvm::Type *CodeGenContext::typeOf(const std::string &type)
{
    if (m_structInfo.find(type) != m_structInfo.end()) {
        return m_structInfo[type].type;
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
    return simpleValue(v, LlvmType(v->getType()));
}

Optional<Value> NDouble::codeGen(CodeGenContext &context)
{
    std::cout << "Creating double: " << value << '\n';

    auto v = llvm::ConstantFP::get(llvm::Type::getDoubleTy(context.context()), value);
    return simpleValue(v, LlvmType(v->getType()));
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

    auto v = llvm::GetElementPtrInst::CreateInBounds(var, {id1, id2}, "", context.currentBlock()->block);
    return simpleValue(v, LlvmType(v->getType()));
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
        return Value(createValue(context, v->value(), v->type().getPointerTo()));
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

    auto typeIt = argTypes.begin();
    for (auto it = func->arguments.begin(); it != func->arguments.end(); ++it, ++typeIt) {
        if (it->type().getSpecialization<ArgumentPackType>()) {
            std::vector<Value> values;
            for (; typeIt != argTypes.end(); ++typeIt) {
                auto argumentValue = &*argsValues++;
                argumentValue->setName(it->name().c_str());

                auto alloc = allocate(*typeIt, it->name(), argumentValue);
                values.push_back(createValue(*this, alloc, LlvmType(*typeIt)));

                info->argTypes.push_back(LlvmType(*typeIt));
            }
            auto value = valuePack(values);
            storeLocal(it->name(), value);
            break;
        }

        auto argumentValue = &*argsValues++;
        argumentValue->setName(it->name().c_str());

        auto alloc = allocate(*typeIt, it->name(), argumentValue);
        auto value = createValue(*this, alloc, LlvmType(*typeIt));
        value.setMutable(it->isMutable());

        storeLocal(it->name(), value);

        info->argTypes.push_back(it->type());
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

static bool canStoreInto(const Token &token, CodeGenContext &ctx, const Type &lhs, const Type &rhs)
{
    if (lhs.get(ctx) != rhs.get(ctx)) {
        return false;
    }

    if (!rhs.typeConstraint().isCompatibleWith(lhs.typeConstraint())) {
        err(token, "cannot assign a value of type '{}' to a variable of type '{}': type constraint not respected", rhs.name(), lhs.name());
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
        return constantInteger(context, values.size());
    }

    llvm::Function *function = context.module().getFunction(name.c_str());

    if (!function) {
        function = findInterfaceImpl(token(), name, firstclasses, context);
    }

    if (!function) {
        function = context.functionTemplate(name, firstclasses);
    }

    if (function == NULL) {
        err(token(), "no such function '{}'", name);
    }

    std::vector<llvm::Value *> vals;

    size_t i = 0;
    auto info = context.functionInfo(function);
    for (auto &&arg: function->getArgumentList()) {
        auto argTy = arg.getType();

        auto converted = context.convertTo(firstclasses[i]->value(), firstclasses[i]->type(), info->argTypes[i]);
        if (!converted) {
            err(token(), "wrong argument type to function; expected '{}', found '{}'", typeName(argTy), typeName(firstclasses[i]->value()->getType()));
        }

        canStoreInto(token(), context, info->argTypes[i], firstclasses[i]->type());

        vals.push_back(converted);
        i++;
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

    llvm::Value *call = llvm::CallInst::Create(function, llvm::makeArrayRef(vals), "", context.currentBlock()->block);
    if (function->getReturnType()->isStructTy()) {
        llvm::AllocaInst *alloc = new llvm::AllocaInst(call->getType(), "", context.currentBlock()->block);
        new llvm::StoreInst(call, alloc, false, context.currentBlock()->block);
        call = alloc;
    }
    return createValue(context, call, LlvmType(function->getReturnType()));
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

    new llvm::StoreInst(rhsFirstClass->value(), lhsFirstClass->value(), false, context.currentBlock()->block);
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
    fmt::print("CONVERT {} to {}\n",from.name(), to.name());

    auto baseDest = to.get(*this);
    auto baseSrc = value->getType();

    if (baseDest == baseSrc) {
        return value;
    }

    int destPointer = 0;
    while (baseDest->isPointerTy()) {
        baseDest = baseDest->getPointerElementType();
        destPointer++;
    }
    int srcPointer = 0;
    while (baseSrc->isPointerTy()) {
        baseSrc = baseSrc->getPointerElementType();
        srcPointer++;
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
                return new llvm::TruncInst(value, to.get(*this), "", currentBlock()->block);
            } else {
                err({}, "cannot truncate from type '{}' to type '{}': value must be between {} and {}",  from.name(), to.name(), min, max);
            }
        }

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
                llvmval = context.convertTo(llvmval, LlvmType(type), retType);


            }
        }
        if (!llvmval) {
            err(token(), "wrong return type for function. found '{}', expected '{}'", typeName(type), retType.name());
        }
    }

    auto v = llvm::ReturnInst::Create(context.context(), llvmval, context.currentBlock()->block);
    return simpleValue(v, LlvmType(v->getType()));
}

Value NVarExpressionInitializer::init(CodeGenContext &ctx, const std::string &name)
{
    if (!expression) {
        err(token, "missing type or initializer when declaring variable '{}'", name);
    }
    auto init = expression->codeGen(ctx);

    if (type.isValid()) {
        auto t = type.get(ctx);
        llvm::AllocaInst *alloc = new llvm::AllocaInst(t, name.c_str(), ctx.currentBlock()->block);
        auto value = createValue(ctx, alloc, type);

        auto toStore = init->getSpecialization<FirstClassValue>();
        if (!canStoreInto(token, ctx, type, toStore->type())) {
            err(token, "mismatched type: assigning value of type '{}' to a variable of type '{}'", toStore->type().name(), type.name());
        }

        new llvm::StoreInst(toStore->value(), alloc, false, ctx.currentBlock()->block);
        return value;
    } else {
        std::vector<Value> sourceValues;
        std::vector<FirstClassValue *> sourceFirstclasses;

        std::function<void (const Value &)> flatten = [&](const Value &v) {
            if (auto pack = v.getSpecialization<PackValue>()) {
                for (auto &&val: pack->unpack()) {
                    flatten(val);
                }
            } else if (auto firstclass = v.getSpecialization<FirstClassValue>()) {
                sourceValues.push_back(v);
                sourceFirstclasses.push_back(firstclass);
            }
        };
        flatten(init);

        std::vector<Value> values;
        for (auto &&val: sourceFirstclasses) {
            auto alloc = ctx.allocate(val->type().get(ctx), name, val->load(ctx));
            values.push_back(createValue(ctx, alloc, val->type()));
        }

        if (values.size() == 1) {
            return values.front();
        }
        return valuePack(values);
    }
}

Value NVarStructInitializer::init(CodeGenContext &ctx, const std::string &name)
{
    auto t = type.get(ctx);
    size_t numFields = 0;
    if (auto info = ctx.structInfo(t)) {
        numFields = info->fields.size();
    } else if (auto i = ctx.unionInfo(t)) {
        numFields = i->fields.size();
    } else {
        error("boo {}", name);
    }

    llvm::AllocaInst *alloc = new llvm::AllocaInst(t, name.c_str(), ctx.currentBlock()->block);
    auto value = createValue(ctx, alloc, type);

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

        new llvm::StoreInst(srcValue, destfirstclass->value(), false, ctx.currentBlock()->block);
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

    size_t i = 0;
    for (auto &&name: names()) {
        auto val = getValue(i);
        bool lastName = i == names().size() - 1;

        Value value;
        if (val) {
            if (lastName && values.size() > i + 1) {

                std::vector<Value> vals;
                for (size_t j = i; j < values.size(); ++j) {
                    auto firstclass = values[j].getSpecialization<FirstClassValue>();
                    auto alloc = context.allocate(firstclass->type().get(context), name.name(), firstclass->load(context));
                    vals.push_back(createValue(context, alloc, firstclass->type()));
                }
                value = valuePack(vals);
                value.setMutable(name.isMutable());
            } else {
                auto firstclass = val->getSpecialization<FirstClassValue>();

                auto alloc = context.allocate(firstclass->type().get(context), name.name(), firstclass->load(context));
                value = createValue(context, alloc, firstclass->type());
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

    if (block) {
        llvm::BasicBlock *bblock = llvm::BasicBlock::Create(context.context(), "entry", function, 0);

        context.pushBlock(bblock, function, nullptr);

        llvm::Function::arg_iterator argsValues = function->arg_begin();
        auto typesIt = argTypes.begin();
        for (auto it = arguments.begin(); it != arguments.end(); it++, typesIt++) {
            auto argumentValue = &*argsValues++;
            argumentValue->setName(it->name().c_str());

            auto alloc = context.allocate(*typesIt, it->name(), argumentValue);
            auto value = createValue(context, alloc, LlvmType(*typesIt));
            value.setMutable(it->isMutable());
            context.storeLocal(it->name(), value);
        }

        block->codeGen(context);
        if (!context.currentBlock()->returned) {
            llvm::ReturnInst::Create(context.context(), nullptr, context.currentBlock()->block);
        }

        context.popBlock();
    }
    return {};
}

class StructType
{
    TYPE_SPECIALIZATION
public:
    StructType(const std::string &n) : m_name(n), m_type(nullptr) {}

    llvm::Type *get(CodeGenContext &context) const
    {
        if (!m_type) {
            if (auto t = context.declaredType(m_name)) {
                m_type = t;
            }
            if (!m_type) {
                fmt::print("CREATE TYPE {}\n",m_name);
                m_type = llvm::StructType::create(context.context(), m_name.c_str());
                context.addDeclaredType(m_name, m_type);
            }
        }
        return m_type;
    }
    std::string name() const { return m_name; }

    std::string m_name;
    mutable llvm::Type *m_type;
};

NStructDeclaration::NStructDeclaration(const std::string &id)
                  : id(id)
                  , m_type(StructType(id))
                  , m_hasBody(false)
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
    info->type = type;
    info->fields.reserve(elements.size());
    for (auto &&el: elements) {
        info->fields.push_back({ el.name, el.mut, el.type });
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
        auto t = static_cast<llvm::PointerType *>(condValue->getType());
        condValue = new llvm::ICmpInst(*ctx.currentBlock()->block, llvm::CmpInst::ICMP_NE, condValue, llvm::ConstantPointerNull::get(t));
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
        llvm::BranchInst::Create(afterblock, ctx.currentBlock()->block);
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
            llvm::BranchInst::Create(afterblock, ctx.currentBlock()->block);
        }
        if (!ifReturned) {
            condition()->popConstraints();
        }
        ctx.popBlock();
    }

    llvm::BranchInst::Create(ifblock, elseblock ? elseblock : afterblock, condValue, ctx.currentBlock()->block);

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
    auto cond = condition()->codeGen(ctx)->getSpecialization<FirstClassValue>()->load(ctx);
    llvm::BranchInst::Create(whileblock, afterblock, cond, ctx.currentBlock()->block);
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
