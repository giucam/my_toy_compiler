
#include "checker.h"
#include "node.h"

Checker *g_checker = nullptr;

Checker::Instance::Instance(Type t)
                 : m_type(std::move(t))
                 , m_kind(Void {})
{
    m_type.initialize(*g_checker);

    auto initTuple = [this](const std::vector<Type> &types) {
        m_kind = Tuple {};
        auto &tuple = std::get<Tuple>(m_kind);

        for (auto &&t: types) {
            tuple.m_fields.push_back(std::make_shared<Instance>(t));
        }
    };

    if (m_type.getSpecialization<StructType>()) {
        m_kind = Aggregate(m_type.typeName());
    } else if (auto pt = m_type.getSpecialization<PointerType>()) {
        m_kind = Pointer(pt->pointerElementType());
    } else if (m_type.getSpecialization<VoidType>()) {
        m_kind = Void {};
    } else if (m_type.getSpecialization<IntegerType>()) {
        m_kind = Primitive {};
    } else if (m_type.getSpecialization<FloatingType>()) {
        m_kind = Primitive {};
    } else if (m_type.getSpecialization<DynamicArrayType>()) {
        m_kind = DynArray {};
    } else if (m_type.getSpecialization<ArrayType>()) {
        m_kind = Array {};
    } else if (m_type.getSpecialization<CustomType>()) {
        if (g_checker->structInfo(m_type.typeName())) {
            m_kind = Aggregate(m_type.typeName());
        } else {
            m_kind = DeclaredAggregate {};
        }
    } else if (auto pack = m_type.getSpecialization<ArgumentPackType>()) {
        initTuple(pack->types());
    } else if (auto tt = m_type.getSpecialization<TupleType>()) {
        initTuple(tt->unpack());
    } else if (m_type.getSpecialization<TemplateType>()) {
        m_kind = Templated {};
    } else {
        fmt::print("unhandled {}\n",m_type.name());
        abort();
    }
}

std::shared_ptr<Checker::Instance> Checker::Instance::Aggregate::field(const std::string &name)
{
    auto info = g_checker->structInfo(m_name);
    if (!info) {
        return nullptr;
    }

    int i = 0;
    for (auto &&f: info->fields) {
        if (f.name == name) {
            return field(i);
        }
        ++i;
    }
    return nullptr;
}

std::shared_ptr<Checker::Instance> Checker::Instance::Aggregate::field(int index)
{
    auto info = g_checker->structInfo(m_name);
    if (!info) {
        return nullptr;
    }

    if (m_fields.empty()) {
        for (auto &&f: info->fields) {
            m_fields.push_back(std::make_shared<Instance>(f.type));
        }
    }

    if (index >= 0 && (size_t)index < m_fields.size()) {
        return m_fields[index];
    }
    return nullptr;
}

std::shared_ptr<Checker::Instance> Checker::Instance::Tuple::field(int index)
{
    if (index < 0 || (size_t)index >= m_fields.size()) {
        return nullptr;
    }

    return m_fields[index];
}

Checker::Instance::Pointer::Pointer(Type t)
                          : m_pointed(std::make_shared<Instance>(std::move(t)))
{
}

std::shared_ptr<Checker::Instance> Checker::Instance::Pointer::pointed() const
{
    return m_pointed;
}

Checker::Checker()
       : Stage()
{
    g_checker = this;
    pushBlock(nullptr);

    m_currentBlock = m_rootBlock = new NBlock;
}

const Checker::StructInfo *Checker::structInfo(const std::string &name) const
{
    auto it = m_structInfo.find(name);
    if (it != m_structInfo.end()) {
        return &it->second;
    }
    return nullptr;
}

Checker::StructInfo *Checker::newStructType(const std::string &name)
{
    auto info = &m_structInfo[name];
    return info;
}

static bool canStoreInto(const Type &lhs, const Type &rhs)
{
    if (lhs.typeName() != rhs.typeName() || !rhs.typeConstraint().isCompatibleWith(lhs.typeConstraint())) {
        return false;
    }
    if (auto lp = lhs.getSpecialization<PointerType>(); auto rp = rhs.getSpecialization<PointerType>()) {
        return canStoreInto(lp->pointerElementType(), rp->pointerElementType());
    }
    return true;
}

struct ConvertResult
{
    bool success;
    operator bool() const { return success; }
    std::string reason;
};

static ConvertResult canConvertTo(const Type &from, const Type &to)
{
    fmt::print("CONVERT {} to {}\n", from.name(), to.name());

    auto baseTypeFrom = from;
    int fromPointer = 0;
    while (auto pt = baseTypeFrom.getSpecialization<PointerType>()) {
        baseTypeFrom = pt->pointerElementType();
        fromPointer++;
    }

    auto checkConstraints = [&]() {
        int ptr = 0;
        Type t = from;
        while (auto p = t.getSpecialization<PointerType>()) {
            t = p->pointerElementType();
            ++ptr;
        }

        t = from;
        while (ptr > fromPointer) {
            t = t.getSpecialization<PointerType>()->pointerElementType();
            --ptr;
        }
        while (ptr < fromPointer) {
            t = t.getPointerTo();
            ++ptr;
        }

        return canStoreInto(to, t);
    };

    if (from.typeName() == to.typeName()) {
        return { checkConstraints(), "" };
    }

    auto baseTypeTo = to;
    int toPointer = 0;
    while (auto pt = baseTypeTo.getSpecialization<PointerType>()) {
        baseTypeTo = pt->pointerElementType();
        toPointer++;
    }

    if (baseTypeFrom.typeName() != baseTypeTo.typeName()) {
        if (to.getSpecialization<IntegerType>() && from.getSpecialization<IntegerType>()) {
            auto intTy = to.getSpecialization<IntegerType>();

            auto max = intTy->maxValue();
            auto min = intTy->minValue();
            TypeConstraint constraint;
            constraint.addConstraint(TypeConstraint::Operator::LesserEqual, max);
            constraint.addConstraint(TypeConstraint::Operator::GreaterEqual, min);
            if (from.typeConstraint().isCompatibleWith(constraint)) {
                return { true, "" };
            } else {
                return { false, fmt::format("cannot truncate from type '{}' to type '{}': value must be between {} and {}",  from.name(), to.name(), min, max) };
            }
        }

        return { false, "" };
    }

    fromPointer = toPointer;
    return { checkConstraints(), "" };
}

void Checker::storeLocal(const std::string &name, std::shared_ptr<Instance> inst)
{
    currentBlock()->locals.insert(std::make_pair(name, inst));
}

std::shared_ptr<Checker::Instance> Checker::CodeGenBlock::local(const std::string &name)
{
    auto it = locals.find(name);
    if (it == locals.end()) {
        return nullptr;
    }
    return it->second;
}

std::shared_ptr<Checker::Instance> Checker::local(const std::string &name)
{
    auto block = currentBlock();
    do {
        auto val = block->local(name);
        if (val) {
            return val;
        }
        block = block->parent;
    } while (block);

    return nullptr;
}

void Checker::pushBlock(CodeGenBlock *parent)
{
    m_blocks.push(CodeGenBlock{parent, {}, false});
}

void Checker::popBlock()
{
    m_blocks.pop();
}

Checker::FunctionInfo *Checker::addFunctionInfo(const std::string &name)
{
    auto it = m_functionInfo.insert({ name, {} });
    return &it->second;
}

Checker::FunctionInfo *Checker::functionInfo(const std::string &name, const std::vector<Type> &args)
{
    FunctionInfo *candidate = nullptr;
    auto range = m_functionInfo.equal_range(name);
    for (auto it = range.first; it != range.second; ++it) {
        auto info = &it->second;

        if (args.size() > info->args.size() && !info->varargs) {
            continue;
        }

        bool perfect = true;
        bool convertable = true;
        int i = 0;
        for (; i < (int)args.size(); ++i) {
            if (i >= (int)info->args.size()) {
                break;
            }
            if (perfect && args[i].typeName() != info->args[i].type.typeName()) {
                perfect = false;
            }
            auto t = info->args[i].type;
            if (auto p = t.getSpecialization<ArgumentPackType>(); p && p->types().size() == 1) {
                t = p->types()[0];
            }
            if (!canConvertTo(args[i], t)) {
                convertable = false;
                break;
            }
        }
        //if the function has one more arg, which is an empty arg pack, that's ok
        if (i < (int)info->args.size()) {
            if (auto p = info->args[i].type.getSpecialization<ArgumentPackType>(); !p || !p->types().empty()) {
                continue;
            }
        }
        //if there are still more args, discard the candidate
        if (++i < (int)info->args.size()) {
            continue;
        }

        if (perfect) {
            return info;
        }
        if (convertable) {
            candidate = info;
        }
    }
    return candidate;

    auto it = m_functionInfo.find(name);
    if (it == m_functionInfo.end()) {
        return nullptr;
    }
    return &it->second;
}

void Checker::addFunctionTemplate(NFunctionDeclaration *func)
{
    m_functionTemplates[func->id] = func;
}

Checker::FunctionInfo *Checker::functionTemplate(const std::string &name, const std::vector<std::shared_ptr<Instance>> args)
{
    auto it = m_functionTemplates.find(name);
    if (it != m_functionTemplates.end()) {
        return makeConcreteFunction(it->second, args);
    }
    return nullptr;
}

Checker::FunctionInfo *Checker::makeConcreteFunction(NFunctionDeclaration *func, const std::vector<std::shared_ptr<Instance>> args)
{
    std::vector<Type> argTypes;
    auto valueIt = args.begin();

    auto templateId = std::string(func->id);

    auto addType = [&](const Type &t) {
        argTypes.push_back(t);
        templateId += "_" + t.typeName();
    };

    for (auto it = func->arguments.begin(); it != func->arguments.end(); ++it, ++valueIt) {
        Type valueType;
        if (it->type().getSpecialization<ArgumentPackType>()) {
            for (auto it = valueIt; it < args.end(); ++it) {
                auto ty = (*it)->type();
                if (structInfo(ty.name())) {
                    ty = ty.getPointerTo();
                }
                addType(ty);
            }
            break;
        } else if (it->type().getSpecialization<TemplateType>()) {
            valueType = (*valueIt)->type();
        } else {
            valueType = it->type();
        }

        addType(valueType);
    }

    if (auto info = functionInfo(func->id, argTypes)) {
        assert(0);
        return info;
    }

    std::vector<NFunctionArgumentDeclaration> arguments;
    auto typeIt = argTypes.begin();
    for (auto it = func->arguments.begin(); it != func->arguments.end(); ++it, ++typeIt) {
        if (it->type().getSpecialization<ArgumentPackType>()) {
            std::vector<Type> types;
            for (; typeIt != argTypes.end(); ++typeIt) {
                types.push_back(*typeIt);
            }
            arguments.push_back(NFunctionArgumentDeclaration{ {}, it->name(), ArgumentPackType(types), true });
            break;
        } else if (it->type().getSpecialization<TemplateType>()) {
            arguments.push_back(NFunctionArgumentDeclaration{ {}, it->name(), *typeIt, it->isMutable() });
        } else {
            arguments.push_back(NFunctionArgumentDeclaration{ {}, it->name(), it->type(), it->isMutable() });
        }
    }

    NFunctionDeclaration *concreteDecl = new NFunctionDeclaration({}, func->id, func->type, arguments, func->block);
    inject(concreteDecl, InjectScope::Global);

    auto info = functionInfo(func->id, argTypes);
    assert(info);
    return info;
}

void Checker::storeGlobal(const std::string &name, const std::shared_ptr<Instance> &value)
{
    m_globals[name] = value;
}

std::shared_ptr<Checker::Instance> Checker::global(const std::string &name) const
{
    auto it = m_globals.find(name);
    if (it != m_globals.end()) {
        return it->second;
    }
    return nullptr;
}

void Checker::addStatement(NStatement *s)
{
    m_currentBlock->statements.push_back(s);
}

Checker::ReturnType Checker::visit(Node &, const Type &hint)
{
    abort();
    return DummyType();
}

Checker::ReturnType Checker::visit(NInteger &integer, const Type &hint)
{
    Type t = IntegerType(true, 32);
    t.setTypeConstraint(TypeConstraint(TypeConstraint::Operator::Equal, integer.value));

    m_expression = std::make_unique<NInteger>(integer.token(), integer.value);
    return std::make_shared<Instance>(t);
}

Checker::ReturnType Checker::visit(NDouble &d, const Type &hint)
{
    Type t = FloatingType(64);
    m_expression = std::make_unique<NDouble>(d.token(), d.value);
    return std::make_shared<Instance>(t);
}

Checker::ReturnType Checker::visit(NBoolean &b, const Type &hint)
{
    Type t = IntegerType(false, 1);
    t.setTypeConstraint(TypeConstraint(TypeConstraint::Operator::Equal, b.value));
    m_expression = std::make_unique<NBoolean>(b.token(), b.value);
    return std::make_shared<Instance>(t);
}

Checker::ReturnType Checker::visit(NString &str, const Type &)
{
    auto t = Type(IntegerType(true, 8)).getPointerTo();
    m_expression = std::make_unique<NString>(str.token(), str.value);
    return std::make_shared<Instance>(t);
}

static std::shared_ptr<Checker::Instance> extractInstance(Checker::ReturnType &r)
{
    if (auto ptr = std::get_if<std::shared_ptr<Checker::Instance>>(&r)) {
        return *ptr;
    }
    return nullptr;
}

Checker::ReturnType Checker::visit(NVariableDeclaration &decl, const Type &hint)
{
    auto expressionInst = [&]() -> std::shared_ptr<Instance> {
        auto ret = decl.expression->visit(*this, decl.declaredType);
        assert(m_expression);
        auto inst = extractInstance(ret);
        if (!inst)
            return nullptr;
        return inst;
    }();
    if (!expressionInst)
        abort();

    auto type = decl.declaredType.isValid() ? decl.declaredType : expressionInst->type();

    if (decl.declaredType.isValid()) {
        if (auto result = canConvertTo(expressionInst->type(), decl.declaredType); !result) {
            err(decl.token(), "cannot initialize variable '{}' of type '{}' with an expression of type '{}'; {}",
                decl.id.name(), decl.declaredType.name(), expressionInst->type().name(), result.reason);
        }
    }

    storeLocal(decl.id.name(), std::make_shared<Instance>(type));

    addStatement(new NVariableDeclaration(decl.token(), decl.id, decl.declaredType, std::move(m_expression)));
    return Declaration();
}

Checker::ReturnType Checker::visit(NMultiVariableDeclaration &var, const Type &hint)
{
    auto ret = var.expression()->visit(*this);
    auto source = extractInstance(ret);
    if (!source) {
        abort();
    }

    std::vector<std::shared_ptr<Instance>> values;
    if (auto tuple = source->kind<Instance::Tuple>()) {
        auto &vec = tuple->fields();
        values.insert(values.end(), vec.begin(), vec.end());
    } else if (auto pack = source->kind<Instance::Tuple>()) {
        auto &vec = pack->fields();
        values.insert(values.end(), vec.begin(), vec.end());
    }

    auto getValue = [&](size_t i) -> std::shared_ptr<Instance> {
        if (values.size() <= i) {
            return nullptr;
        }
        return values[i];
    };

    size_t i = 0;
    for (auto &&name: var.names()) {
        auto val = getValue(i);
        bool lastName = i == var.names().size() - 1;

        std::shared_ptr<Instance> instance;
        if (val) {
            if (lastName && values.size() > i + 1) {

                std::vector<Type> types;
                for (size_t j = i; j < values.size(); ++j) {
                    types.push_back(values[j]->type());
                }
                instance = std::make_shared<Instance>(TupleType(types));
            } else {
                instance = std::make_shared<Instance>(val->type());
            }
        } else {
            std::vector<Type> types;
            instance = std::make_shared<Instance>(TupleType(types));
        }

        storeLocal(name.name(), instance);
        ++i;
    }

    auto names = var.names();
    addStatement(new NMultiVariableDeclaration(var.token(), names, std::move(m_expression)));
    return Declaration();
}

Checker::ReturnType Checker::visit(NExternVariableDeclaration &var, const Type &hint)
{
    if (auto v = global(var.name())) {
        return v;
    }
    auto instance = std::make_shared<Instance>(var.type());
    storeGlobal(var.name(), instance);
    addStatement(new NExternVariableDeclaration(var.token(), var.name(), var.type()));
    return instance;
}

Checker::ReturnType Checker::visit(NIdentifier &ident, const Type &hint)
{
    std::unique_ptr<NExpression> newContext;
    auto instance = [&]() -> std::shared_ptr<Instance> {
        if (ident.context()) {
            auto ret = ident.context()->visit(*this);
            auto parentInstance = extractInstance(ret);
            if (!parentInstance)
                abort();

            newContext = std::move(m_expression);
            while (auto pointed = parentInstance->kind<Instance::Pointer>()) {
                parentInstance = pointed->pointed();
            }

            if (ident.type == NIdentifier::Index) {
                if (structInfo(parentInstance->type().typeName())) {
                    auto aggregate = parentInstance->kind<Instance::Aggregate>();
                    if (auto field = aggregate->field(ident.index)) {
                        return field;
                    }
                    err(ident.token(), "invalid index '{}' when accessing aggregate of type '{}' with {} elements",
                                    ident.index, parentInstance->type().name(), aggregate->fields().size());
                } else if (auto tuple = parentInstance->kind<Instance::Tuple>()) {
                    if (auto field = tuple->field(ident.index)) {
                        return field;
                    }
                    err(ident.token(), "invalid index '{}' when accessing aggregate of type '{}' with {} elements",
                                    ident.index, parentInstance->type().name(), tuple->fields().size());
                } else {
                    err(ident.token(), "'{}' is not an aggregate type", parentInstance->type().name());
                }
            } else {
                if (structInfo(parentInstance->type().typeName())) {
                    if (auto field = parentInstance->kind<Instance::Aggregate>()->field(ident.name)) {
                        return field;
                    }
                    err(ident.token(), "aggregate type '{}' has no field name '{}'", parentInstance->type().name(), ident.name);
                } else {
                    err(ident.token(), "'{}' is not a named aggregate type", parentInstance->type().name());
                }
            }
        }

        auto instance = local(ident.name);
        if (!instance) {
            instance = global(ident.name);
        }
        return instance;
    }();

    if (!instance) {
        err(ident.token(), "'{}' was not declared in this scope", ident.name);
    }

    if (ident.type == NIdentifier::Index) {
        m_expression = std::make_unique<NIdentifier>(ident.token(), ident.index);
    } else {
        m_expression = std::make_unique<NIdentifier>(ident.token(), ident.name);
    }
    m_expression->pushContext(newContext.get());
    m_expression->attach(std::move(newContext));
    return instance;
}

Checker::ReturnType Checker::visit(NAssignment &ass, const Type &hint)
{
    auto lhsRet = ass.lhs->visit(*this);
    auto newLhs = std::move(m_expression);
    auto rhsRet = ass.rhs->visit(*this);
    auto newRhs = std::move(m_expression);

    m_expression = std::make_unique<NAssignment>(ass.token(), std::move(newLhs), std::move(newRhs));
    return DummyType();
}

Checker::ReturnType Checker::visit(NBlock &block, const Type &hint)
{
    for (auto &&s: block.statements) {
        s->visit(*this);
    }
    return DummyType();
}


Checker::ReturnType Checker::visit(NFunctionDeclaration &decl, const Type &hint)
{
    pushBlock(currentBlock());

    for (auto &&arg: decl.arguments) {
        if (auto pack = arg.type().getSpecialization<ArgumentPackType>(); (pack && pack->isVariadic()) || arg.type().getSpecialization<TemplateType>()) {
            addFunctionTemplate(&decl);
            return Declaration();
        }
    }

    std::string mangledName = decl.id;
    if (decl.id != "main") { //HACK
        if (decl.arguments.size() == 0) {
            mangledName += "_void";
        }
        for (auto &&arg: decl.arguments) {
            if (auto p = arg.type().getSpecialization<ArgumentPackType>()) {
                for (auto &&t: p->types()) {
                    mangledName += "_" + t.typeName();
                }
            } else {
                mangledName += "_" + arg.type().typeName();
            }
        }
    }

    auto info = addFunctionInfo(decl.id);
    info->returnType = decl.type;
    info->defined = true;
    info->name = mangledName;
    info->varargs = false;

    for (auto &&arg: decl.arguments) {
        storeLocal(arg.name(), std::make_shared<Instance>(arg.type()));

        if (auto p = arg.type().getSpecialization<ArgumentPackType>()) {
            for (auto &&t: p->types()) {
                info->args.push_back({ arg.name(), t });
            }
        } else {
            info->args.push_back({ arg.name(), arg.type() });
        }
    }

    NBlock *newBlock = nullptr;
    if (decl.block) {
        auto oldBlock = m_currentBlock;
        newBlock = m_currentBlock = new NBlock;
        decl.block->visit(*this);
        m_currentBlock = oldBlock;
    }

    auto newDecl = new NFunctionDeclaration(decl.token(), info->name, decl.type, decl.arguments, newBlock);
    m_rootBlock->statements.push_back(newDecl);

    popBlock();
    return Declaration();
}

Checker::ReturnType Checker::visit(NExternDeclaration &decl, const Type &hint)
{
    auto info = addFunctionInfo(decl.name());
    info->defined = false;
    info->returnType = decl.returnType();
    info->name = decl.name();
    info->varargs = decl.isVarargs();

    for (auto &&arg: decl.arguments()) {
        info->args.push_back({ arg.name(), arg.type() });
    }

    auto newArgs = decl.arguments();
    m_rootBlock->statements.push_back(new NExternDeclaration(decl.name(), decl.returnType(), newArgs, decl.isVarargs()));

    return Declaration();
}

Checker::ReturnType Checker::visit(NStructDeclaration &decl, const Type &hint)
{
    if (!decl.m_hasBody) {
        return Declaration();
    }

    std::cout << "Creating struct declaration " << decl.id << '\n';

    if (structInfo(decl.id)) {
        err(decl.token(), "struct '{}' was already declared", decl.id);
    }

    auto info = newStructType(decl.id);
    info->type = decl.m_type;
    info->fields.reserve(decl.elements.size());
    for (auto &&el: decl.elements) {
        info->fields.push_back(StructInfo::FieldInfo{ el.name, el.mut, el.type });
    }

    auto newStruct = new NStructDeclaration(decl.id, decl.m_flags);
    auto newFields = decl.elements;
    newStruct->setFields(newFields);
    addStatement(newStruct);

    return Declaration();
}

Checker::ReturnType Checker::visit(NUnionDeclaration &decl, const Type &hint)
{
    auto info = newStructType(decl.id);
    info->type = decl.m_type;
    info->fields.reserve(decl.elements.size());
    for (auto &&el: decl.elements) {
        info->fields.push_back(StructInfo::FieldInfo{ el.name, el.mut, el.type });
    }

    auto newFields = decl.elements;
    auto newUnion = new NUnionDeclaration(decl.id, newFields);

    addStatement(newUnion);

    return Declaration();
}

Checker::ReturnType Checker::visit(NEnumDeclaration &decl, const Type &hint)
{
    auto newEntries = decl.entries();
    addStatement(new NEnumDeclaration(decl.name(), decl.type(), newEntries));

    if (decl.name().empty()) {
        for (auto &&e: decl.entries()) {
            storeGlobal(e.name, std::make_shared<Instance>(decl.type()));
        }
    } else {
        //make a fake struct with the values

        auto info = newStructType(decl.name());
        for (auto &&e: decl.entries()) {
            info->fields.push_back({ e.name, false, decl.type() });
        }

        storeGlobal(decl.name(), std::make_shared<Instance>(StructType(decl.name())));
    }

    return Declaration();
}

Checker::ReturnType Checker::visit(NIfaceDeclaration &decl, const Type &hint)
{
    auto newPro = decl.prototypes;
    auto newPar = decl.parameters;
    addStatement(new NIfaceDeclaration(decl.name, newPar, newPro));
    return Declaration();
}

Checker::ReturnType Checker::visit(NImplDeclaration &decl, const Type &hint)
{
    FuncDeclarationList newFuncs = decl.functions;
    auto newPar = decl.parameters;
    addStatement(new NImplDeclaration(decl.name, newPar, newFuncs));
    return Declaration();
}

static void pushIntegerConstraints(std::shared_ptr<Checker::Instance> lhs, std::shared_ptr<Checker::Instance> rhs, NBinaryOperator &op, bool negate)
{
    auto &lt = lhs->type();
    auto &rt = rhs->type();

    if ((!negate && op.op == NBinaryOperator::OP::Equal) ||
        (negate && op.op == NBinaryOperator::OP::NotEqual)) {
        lt.typeConstraint().add(rt.typeConstraint(), &op);
    } else if ((!negate && op.op == NBinaryOperator::OP::NotEqual) ||
            (negate && op.op == NBinaryOperator::OP::Equal)) {
        lt.typeConstraint().addNegate(rt.typeConstraint(), &op);
    } else if ((!negate && op.op == NBinaryOperator::OP::GreaterEqual) ||
            (negate && op.op == NBinaryOperator::OP::Lesser)) {
        lt.typeConstraint().addGreaterEqual(rt.typeConstraint(), &op);
    } else if ((!negate && op.op == NBinaryOperator::OP::Greater) ) {
        lt.typeConstraint().addGreater(rt.typeConstraint(), &op);
    }
}

Checker::ReturnType Checker::visit(NBinaryOperator &op, const Type &hint)
{
    auto lhsResult = op.lhs->visit(*this);
    std::unique_ptr<NExpression> newLhs = std::move(m_expression);
    auto rhsResult = op.rhs->visit(*this);
    std::unique_ptr<NExpression> newRhs = std::move(m_expression);

    m_expression = std::make_unique<NBinaryOperator>(op.token(), std::move(newLhs), op.op, std::move(newRhs));

    auto lhs = extractInstance(lhsResult);
    auto rhs = extractInstance(rhsResult);

    if (!lhs || !rhs)
        abort();

    if (lhs->type().getSpecialization<IntegerType>() && rhs->type().getSpecialization<IntegerType>()) {
        auto pushFunc = [lhs, rhs, &op](bool negate) { pushIntegerConstraints(lhs, rhs, op, negate); };
        auto popFunc = [lhs, &op]() { lhs->type().typeConstraint().removeFromSource(&op); };
        op.m_constraints.push_back({ pushFunc, popFunc });

        switch (op.op) {
            case NBinaryOperator::OP::Remainder:
            case NBinaryOperator::OP::Div: {
                if (!rhs->type().typeConstraint().isCompatibleWith(TypeConstraint(TypeConstraint::Operator::NotEqual, 0))) {
                    err(op.token(), "divisor value is of type '{}' and may be 0; guard the operation with an if statement", rhs->type().name());
                }
            }
            default:
                break;
        }

        Type retType = rhs->type();
        retType.setTypeConstraint([&]() {
            switch (op.op) {
                case NBinaryOperator::OP::Div:
                    return lhs->type().typeConstraint() / rhs->type().typeConstraint();
                case NBinaryOperator::OP::Mul:
                    return lhs->type().typeConstraint() * rhs->type().typeConstraint();
                case NBinaryOperator::OP::Add:
                    return lhs->type().typeConstraint() + rhs->type().typeConstraint();
                default:
                    break;
            }
            return TypeConstraint();
        }());

        return std::make_shared<Instance>(retType);
    } else if (lhs->type().getSpecialization<PointerType>() && rhs->type().getSpecialization<PointerType>()) {
        switch (op.op) {
            case NBinaryOperator::OP::Add:
            case NBinaryOperator::OP::Mul:
            case NBinaryOperator::OP::Sub:
            case NBinaryOperator::OP::Div:
            case NBinaryOperator::OP::Remainder:
                return std::make_shared<Instance>(IntegerType(true, 32));
            case NBinaryOperator::OP::Or:
            case NBinaryOperator::OP::Equal:
            case NBinaryOperator::OP::NotEqual:
            case NBinaryOperator::OP::Lesser:
            case NBinaryOperator::OP::Greater:
            case NBinaryOperator::OP::GreaterEqual:
            case NBinaryOperator::OP::LesserEqual:
                return std::make_shared<Instance>(IntegerType(false, 1));
        }
    } else if (auto lf = lhs->type().getSpecialization<FloatingType>(); lf && rhs->type().getSpecialization<FloatingType>()) {
        switch (op.op) {
            case NBinaryOperator::OP::Add:
            case NBinaryOperator::OP::Mul:
            case NBinaryOperator::OP::Sub:
            case NBinaryOperator::OP::Div:
            case NBinaryOperator::OP::Remainder:
                return std::make_shared<Instance>(FloatingType(lf->bits()));
            case NBinaryOperator::OP::Or:
            case NBinaryOperator::OP::Equal:
            case NBinaryOperator::OP::NotEqual:
            case NBinaryOperator::OP::Lesser:
            case NBinaryOperator::OP::Greater:
            case NBinaryOperator::OP::GreaterEqual:
            case NBinaryOperator::OP::LesserEqual:
                return std::make_shared<Instance>(IntegerType(false, 1));
        }
    } else if (lhs->type().getSpecialization<PointerType>() && rhs->type().getSpecialization<IntegerType>()) {
        switch (op.op) {
            case NBinaryOperator::OP::Add:
            case NBinaryOperator::OP::Sub:
                return std::make_shared<Instance>(lhs->type());
            default:
                break;
        }
    }

    err(op.token(), "invalid operands '{}' and '{}' for binary expression", lhs->type().name(), rhs->type().name());
    return DummyType();
}

Checker::ReturnType Checker::visit(NMethodCall &call, const Type &hint)
{
    std::vector<std::shared_ptr<Instance>> args;

    std::function<void (std::shared_ptr<Instance>)> add = [&](std::shared_ptr<Instance> inst) {
        if (auto &&t = inst->kind<Instance::Tuple>()) {
            for (auto &&v: t->fields()) {
                add(v);
            }
        } else {
            args.push_back(inst);
        }
    };

    std::unique_ptr<NExpression> newContext = nullptr;
    if (call.context()) {
        auto ret = call.context()->visit(*this);
        auto inst = extractInstance(ret);
        if (!inst)
            abort();
        add(inst);
        newContext = std::move(m_expression);
    }
    ExpressionList newExpressionList;
    for (auto &&arg: call.arguments) {
        auto ret = arg->visit(*this);
        auto inst = extractInstance(ret);
        if (!inst)
            abort();
        add(inst);
        newExpressionList.push_back(std::move(m_expression));
    }

    std::vector<Type> types;
    for (auto &&a: args) {
        types.push_back(a->type());
    }
    auto func = functionInfo(call.name, types);

    if (!func) {
        func = functionTemplate(call.name, args);
    }

    if (!func && call.name == "count") {
        Type t = IntegerType(true, 32);
        t.setTypeConstraint(TypeConstraint(TypeConstraint::Operator::Equal, args.size()));

        m_expression = std::make_unique<NInteger>(Token(), args.size());
        return std::make_shared<Instance>(t);
    }

    if (!func) {
        err(call.token(), "No such function '{}'", call.name);
    }

    m_expression = std::make_unique<NMethodCall>(call.token(), func->name, newExpressionList);
    m_expression->pushContext(newContext.get());
    m_expression->attach(std::move(newContext));

    if (func->args.size() < args.size() && !func->varargs) {
        err(call.token(), "too many arguments to function");
    }

    return std::make_shared<Instance>(func->returnType);
}

Checker::ReturnType Checker::visit(NAddressOfExpression &addr, const Type &hint)
{
    auto ret = addr.expression()->visit(*this);
    auto instance = extractInstance(ret);
    if (!instance)
        abort();

    m_expression = std::make_unique<NAddressOfExpression>(addr.token(), std::move(m_expression));
    return std::make_shared<Instance>(instance->type().getPointerTo());
}

Checker::ReturnType Checker::visit(NExpressionPack &pack, const Type &hint)
{
    if (pack.expressionList().size() == 1) {
        return pack.expressionList().front()->visit(*this);
    }

    ExpressionList newList;
    std::vector<Type> args;
    for (auto &&e: pack.expressionList()) {
        auto ret = e->visit(*this);
        auto inst = extractInstance(ret);
        if (!inst) {
            abort();
        }

        newList.push_back(std::move(m_expression));
        args.push_back(inst->type());
    }

    m_expression = std::make_unique<NExpressionPack>(pack.token(), newList);
    return std::make_shared<Instance>(TupleType(args));
}

Checker::ReturnType Checker::visit(NIfStatement &ifs, const Type &hint)
{
    auto curBlock = currentBlock();
    ifs.condition()->visit(*this);
    auto newCondition = std::move(m_expression);

    pushBlock(curBlock);
    ifs.condition()->pushConstraints(false);

    auto oldBlock = m_currentBlock;
    m_currentBlock = new NBlock;

    ifs.block()->visit(*this);

    auto ifBlock = m_currentBlock;
    m_currentBlock = oldBlock;

    bool ifReturned = currentBlock()->returned;
    ifs.condition()->popConstraints();
    popBlock();

    if (ifReturned) {
        ifs.condition()->pushConstraints(true);
    }

    NBlock *elseBlock = nullptr;
    if (ifs.elseBlock()) {
        pushBlock(curBlock);
        if (!ifReturned) {
            ifs.condition()->pushConstraints(true);
        }

        m_currentBlock = elseBlock = new NBlock;
        ifs.elseBlock()->visit(*this);
        m_currentBlock = oldBlock;

        if (!ifReturned) {
            ifs.condition()->popConstraints();
        }
        popBlock();
    }

    addStatement(new NIfStatement(std::move(newCondition), ifBlock, elseBlock));
    return DummyType();
}

Checker::ReturnType Checker::visit(NReturnStatement &ret, const Type &hint)
{
    currentBlock()->returned = true;
    if (ret.expression()) {
        ret.expression()->visit(*this);
        assert(m_expression.get());
    }
    addStatement(new NReturnStatement(std::move(m_expression)));
    return DummyType();
}

Checker::ReturnType Checker::visit(NInitializerListExpression &init, const Type &type)
{
    std::vector<NInitializerListExpression::Initializer> newInitializers;
    for (auto &&in: init.initializers()) {
        auto info = structInfo(type.typeName());
        if (!info) {
            err(in.value->token(), "type '{}' is not a named aggregate type", type.name());
        }
        for (auto &&field: info->fields) {
            if (field.name == in.name) {
                auto t = in.value->visit(*this, field.type);
                if (auto inst = extractInstance(t)) {
                    if (auto result = canConvertTo(inst->type(), field.type); !result) {
                        err(in.value->token(), "cannot initialize field '{}' of type '{}' with an expression of type '{}'; {}",
                            in.name, field.type.name(), inst->type().name(), result.reason);
                    }
                } else {
                    abort();
                }

                newInitializers.push_back({ in.token, in.name, std::move(m_expression) });
            }
        }
    }

    auto instance = std::make_shared<Instance>(type);
    m_expression = std::make_unique<NInitializerListExpression>(init.token(), newInitializers);
    return instance;
}

Checker::ReturnType Checker::visit(NCastExpression &cast, const Type &hint)
{
    cast.expression()->visit(*this);
    m_expression = std::make_unique<NCastExpression>(cast.token(), std::move(m_expression), cast.type());
    return std::make_shared<Instance>(cast.type());
}

Checker::ReturnType Checker::visit(NExpressionStatement &s, const Type &hint)
{
    s.expression()->visit(*this);
    assert(m_expression.get());
    addStatement(new NExpressionStatement(std::move(m_expression)));
    return DummyType();
}

Checker::ReturnType Checker::visit(NForStatement &fs, const Type &hint)
{
    auto ret = fs.arrayExpr()->visit(*this);
    auto arrayValue = extractInstance(ret);
    if (!arrayValue)
        abort();

    if (!arrayValue->kind<Instance::DynArray>()) {
        err(fs.exprToken(), "cannot loop over the given expression");
    }

    auto newArrayExpr = std::move(m_expression);
    assert(newArrayExpr);

    auto arrType = arrayValue->type().getSpecialization<DynamicArrayType>();
    storeLocal(fs.itToken().text(), std::make_shared<Instance>(arrType->elementType()));

    auto oldBlock = m_currentBlock;
    m_currentBlock = new NBlock;
    fs.block()->visit(*this);

    auto newFor = new NForStatement(fs.itToken(), fs.exprToken(), std::move(newArrayExpr), m_currentBlock);
    m_currentBlock = oldBlock;

    addStatement(newFor);
    return Declaration();
}

Checker::ReturnType Checker::visit(NWhileStatement &ws, const Type &hint)
{
    auto ret = ws.condition()->visit(*this);
    auto condition = extractInstance(ret);
    if (!condition)
        abort();

    auto newCondition = std::move(m_expression);
    assert(newCondition);

    auto oldBlock = m_currentBlock;
    m_currentBlock = new NBlock;
    ws.block()->visit(*this);

    auto newWhile = new NWhileStatement(ws.token(), std::move(newCondition), m_currentBlock);
    m_currentBlock = oldBlock;

    addStatement(newWhile);
    return Declaration();
}

Checker::ReturnType Checker::visit(NSizeofExpression &so, const Type &hint)
{
    m_expression = std::make_unique<NSizeofExpression>(so.token(), so.type);
    return std::make_shared<Instance>(IntegerType(true, 32));
}

void Checker::inject(Node *node, InjectScope scope)
{
    node->visit(*this);
}

bool Checker::isFunctionDefined(const std::string &name, const std::vector<Type> &args) const
{
    return const_cast<Checker *>(this)->functionInfo(name, args);
}
