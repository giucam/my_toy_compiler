
#include "checker.h"
#include "node.h"

Checker *g_checker = nullptr;

Checker::Instance::Instance(Type t)
                 : m_type(std::move(t))
                 , m_kind(Void {})
{
    m_type.initialize(*g_checker);

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
    } else if (m_type.getSpecialization<ArgumentPackType>()) {
        m_kind = ArgPack {};
    } else if (m_type.getSpecialization<DynamicArrayType>()) {
        m_kind = DynArray {};
    } else if (m_type.getSpecialization<CustomType>()) {
        if (g_checker->structInfo(m_type.typeName())) {
            m_kind = Aggregate(m_type.typeName());
        } else {
            m_kind = DeclaredAggregate {};
        }
    } else if (m_type.getSpecialization<TupleType>()) {
        m_kind = Tuple {};
        auto &tuple = std::get<Tuple>(m_kind);

        auto tt = m_type.getSpecialization<TupleType>();
        for (auto &&t: tt->unpack()) {
            tuple.m_fields.push_back(std::make_shared<Instance>(t));
        }
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

static bool canConvertTo(const Type &from, const Type &to, const Token &errTok)
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
        return checkConstraints();
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
                return true;
            } else {
                err(errTok, "cannot truncate from type '{}' to type '{}': value must be between {} and {}",  from.name(), to.name(), min, max);
                return false;
            }
        }

        return false;
    }

    fromPointer = toPointer;
    return checkConstraints();
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
    if (m_functionInfo.find(name) != m_functionInfo.end())
        abort();
    return &m_functionInfo[name];
}

Checker::FunctionInfo *Checker::functionInfo(const std::string &name)
{
    auto it = m_functionInfo.find(name);
    if (it == m_functionInfo.end()) {
        return nullptr;
    }
    return &it->second;
}


Checker::ReturnType Checker::visit(Node &, const Type &hint)
{
    return DummyType();
}

Checker::ReturnType Checker::visit(NInteger &integer, const Type &hint)
{
    Type t = IntegerType(true, 32);
    t.setTypeConstraint(TypeConstraint(TypeConstraint::Operator::Equal, integer.value));
    return std::make_shared<Instance>(t);
}

Checker::ReturnType Checker::visit(NDouble &d, const Type &hint)
{
    Type t = FloatingType(64);
    return std::make_shared<Instance>(t);
}

Checker::ReturnType Checker::visit(NBoolean &b, const Type &hint)
{
    Type t = IntegerType(false, 1);
    t.setTypeConstraint(TypeConstraint(TypeConstraint::Operator::Equal, b.value));
    return std::make_shared<Instance>(t);
}

Checker::ReturnType Checker::visit(NString &str, const Type &)
{
    auto t = Type(IntegerType(true, 8)).getPointerTo();
    return std::make_shared<Instance>(t);
}

static std::shared_ptr<Checker::Instance> extractInstance(Checker::ReturnType &r)
{
    if (auto ptr = std::get_if<std::shared_ptr<Checker::Instance>>(&r)) {
        return *ptr;
    }
    return nullptr;
}

static void checkInitializerList(Checker &checker, const Type &type, NInitializerListExpression *il)
{
    for (auto &&in: il->initializers()) {
        auto info = checker.structInfo(type.typeName());
        for (auto &&field: info->fields) {
            if (field.name == in.name) {
                auto t = in.value->visit(checker, field.type);
                if (auto inst = extractInstance(t)) {
                    if (!canConvertTo(inst->type(), field.type, in.value->token())) {
                        err(in.value->token(), "cannot initialize field '{}' of type '{}' with an expression of type '{}'", in.name, field.type.name(), inst->type().name());
                    }
                } else {
                    abort();
                }
            }
        }
    }
}

Checker::ReturnType Checker::visit(NVariableDeclaration &decl, const Type &hint)
{
    auto &expr = static_cast<NVarExpressionInitializer *>(decl.m_init.get())->expression;

    auto declaredType = static_cast<NVarExpressionInitializer *>(decl.m_init.get())->type;
    auto expressionInst = [&]() -> std::shared_ptr<Instance> {
        auto ret = expr->visit(*this, declaredType);
        auto inst = extractInstance(ret);
        if (!inst)
            return nullptr;
        return inst;
    }();
    if (!expressionInst)
        abort();

    auto type = declaredType.isValid() ? declaredType : expressionInst->type();

    if (auto il = dynamic_cast<NInitializerListExpression *>(expr.get())) {
        checkInitializerList(*this, type, il);
    } else if (declaredType.isValid()) {
        if (!canConvertTo(expressionInst->type(), declaredType, decl.token())) {
            err(decl.token(), "cannot initialize variable '{}' of type '{}' with an expression of type '{}'", decl.id.name(), declaredType.name(), expressionInst->type().name());
        }
    }

    storeLocal(decl.id.name(), std::make_shared<Instance>(type));
    return Declaration();
}

Checker::ReturnType Checker::visit(NIdentifier &ident, const Type &hint)
{
    if (ident.context()) {
        auto ret = ident.context()->visit(*this);
        auto parentInstance = extractInstance(ret);
        if (!parentInstance)
            abort();

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
        err(ident.token(), "'{}' was not declared in this scope", ident.name);
    }

    return instance;
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

    auto info = functionInfo(decl.id);
    if (info && info->defined) {
        err(decl.token(), "function '{}' already defined", decl.id);
    }
    if (!info) {
        info = addFunctionInfo(decl.id);
    }
    info->returnType = decl.type;
    info->defined = true;

    for (auto &&arg: decl.arguments) {
        storeLocal(arg.name(), std::make_shared<Instance>(arg.type()));
        info->args.push_back({ arg.name(), local(arg.name()) });
    }
    decl.block->visit(*this);

    popBlock();
    return Declaration();
}

Checker::ReturnType Checker::visit(NExternDeclaration &decl, const Type &hint)
{
    auto info = functionInfo(decl.name());
    if (!info) {
        info = addFunctionInfo(decl.name());
    }
    info->defined = false;
    info->returnType = decl.returnType();

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
    auto rhsResult = op.rhs->visit(*this);

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
    }

    err(op.token(), "invalid operands '{}' and '{}' for binary expression", lhs->type().name(), rhs->type().name());
    return DummyType();
}

Checker::ReturnType Checker::visit(NMethodCall &call, const Type &hint)
{
    std::vector<std::shared_ptr<Instance>> args;

    std::function<void (std::shared_ptr<Instance>)> add = [&](std::shared_ptr<Instance> inst) {
        if (auto &&t = inst->kind<Instance::Aggregate>()) {
            for (auto &&v: t->fields()) {
                add(v);
            }
        } else {
            args.push_back(inst);
        }
    };

    if (call.context()) {
        auto ret = call.context()->visit(*this);
        auto inst = extractInstance(ret);
        if (!inst)
            abort();
        add(inst);
    }
    for (auto &&arg: call.arguments) {
        auto ret = arg->visit(*this);
        auto inst = extractInstance(ret);
        if (!inst)
            abort();
        add(inst);
    }

    auto func = functionInfo(call.name);
    if (!func) {
        auto mangledName = call.name;
        for (auto &&t: args) {
            mangledName += "_";
            mangledName += t->type().typeName();
        }

        func = functionInfo(mangledName);
    }

    if (!func && call.name == "count") {
        Type t = IntegerType(true, 32);
        t.setTypeConstraint(TypeConstraint(TypeConstraint::Operator::Equal, args.size()));

        return std::make_shared<Instance>(t);
    }

    if (!func) {
        err(call.token(), "No such function '{}'", call.name);
    }

    return std::make_shared<Instance>(func->returnType);
}

Checker::ReturnType Checker::visit(NAddressOfExpression &addr, const Type &hint)
{
    auto ret = addr.expression()->visit(*this);
    auto instance = extractInstance(ret);
    if (!instance)
        abort();

    return std::make_shared<Instance>(instance->type().getPointerTo());
}

Checker::ReturnType Checker::visit(NExpressionPack &pack, const Type &hint)
{
    if (pack.expressionList().size() == 1) {
        return pack.expressionList().front()->visit(*this);
    }

    std::vector<Type> args;
    for (auto &&e: pack.expressionList()) {
        auto ret = e->visit(*this);
        auto inst = extractInstance(ret);
        if (!inst) {
            abort();
        }

        args.push_back(inst->type());
    }
    return std::make_shared<Instance>(TupleType(args));
}

Checker::ReturnType Checker::visit(NIfStatement &ifs, const Type &hint)
{
    auto curBlock = currentBlock();
    ifs.condition()->visit(*this);

    pushBlock(curBlock);
    ifs.condition()->pushConstraints(false);
    ifs.block()->visit(*this);
    bool ifReturned = currentBlock()->returned;
    ifs.condition()->popConstraints();
    popBlock();

    if (ifReturned) {
        ifs.condition()->pushConstraints(true);
    }

    if (ifs.elseBlock()) {
        pushBlock(curBlock);
        if (!ifReturned) {
            ifs.condition()->pushConstraints(true);
        }
        ifs.elseBlock()->visit(*this);
        if (!ifReturned) {
            ifs.condition()->popConstraints();
        }
        popBlock();
    }

    return DummyType();
}

Checker::ReturnType Checker::visit(NReturnStatement &, const Type &hint)
{
    currentBlock()->returned = true;
    return DummyType();
}

Checker::ReturnType Checker::visit(NInitializerListExpression &init, const Type &hint)
{
    checkInitializerList(*this, hint, &init);
    return std::make_shared<Instance>(hint);
}

Checker::ReturnType Checker::visit(NCastExpression &cast, const Type &hint)
{
    return std::make_shared<Instance>(cast.type());
}


void Checker::inject(Node *node, InjectScope scope)
{
    node->visit(*this);
}

int Checker::typeSize(const Type &t)
{
    return 0;
}

bool Checker::isFunctionDefined(const std::string &name) const
{
    return m_functionInfo.find(name) != m_functionInfo.end();
}
