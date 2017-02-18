
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>

#include "types.h"
#include "codegen.h"
#include "common.h"

Type Type::getPointerTo() const
{
    return PointerType(*this);
}

std::string Type::name() const
{
    return m_iface->name() + m_constraint.name();
}

std::string Type::typeName() const
{
    return m_iface->name();
}

void Type::setTypeConstraint(const TypeConstraint &c)
{
    m_constraint = c;
}

TypeConstraint::TypeConstraint()
{
}

TypeConstraint::TypeConstraint(TypeConstraint::Operator op, int v)
{
    m_constraints.push_back({ op, v, nullptr });
}

std::string TypeConstraint::name() const
{
    std::string n;

    int i = 0;
    for (auto &&c: m_constraints) {
        n += (i++ == 0) ? "|$ " : ", $ ";
        switch (c.op) {
            case Operator::Equal:
                n += "== ";
                break;
            case Operator::NotEqual:
                n += "!= ";
                break;
            case Operator::Greater:
                n += "> ";
                break;
            case Operator::Lesser:
                n += "< ";
                break;
            case Operator::GreaterEqual:
                n += ">= ";
                break;
            case Operator::LesserEqual:
                n += "<= ";
                break;
        }
        n += std::to_string(c.value);
    }
    return n;
}

bool TypeConstraint::isCompatibleWith(const TypeConstraint &c) const
{
    enum {
        Compatible,
        NotCompatible,
        Orthogonal,
    };

    auto compatibility = [&](const Constraint &c1, const Constraint &c2) -> int {
        switch (c2.op) {
            case Operator::Equal:
                if (c1.op == Operator::Equal && c1.value == c2.value) return Compatible;
                break;
            case Operator::NotEqual:
                if (c1.op == Operator::NotEqual && c1.value != c2.value) return Orthogonal;
                if ((c1.op == Operator::NotEqual && c1.value == c2.value) ||
                    (c1.op == Operator::Equal && c1.value != c2.value) ||
                    (c1.op == Operator::Greater && c1.value >= c2.value) ||
                    (c1.op == Operator::Lesser && c1.value < c2.value) ||
                    (c1.op == Operator::GreaterEqual && c1.value > c2.value) ||
                    (c1.op == Operator::LesserEqual && c1.value < c2.value)) return Compatible;
                break;
            case Operator::Greater:
                if ((c1.op == Operator::Equal && c1.value > c2.value) ||
                    (c1.op == Operator::Greater && c1.value >= c2.value) ||
                    (c1.op == Operator::GreaterEqual && c1.value > c2.value)) return Compatible;
                break;
            case Operator::Lesser:
                if ((c1.op == Operator::Equal && c1.value < c2.value) ||
                    (c1.op == Operator::Lesser && c1.value <= c2.value)) return Compatible;
                break;
            case Operator::GreaterEqual:
                if ((c1.op == Operator::Equal && c1.value >= c2.value) ||
                    (c1.op == Operator::Greater && c1.value >= c2.value) ||
                    (c1.op == Operator::GreaterEqual && c1.value >= c2.value)) return Compatible;
                break;
            case Operator::LesserEqual:
                if ((c1.op == Operator::Equal && c1.value <= c2.value) ||
                    (c1.op == Operator::Lesser && c1.value <= c2.value) ||
                    (c1.op == Operator::LesserEqual && c1.value <= c2.value)) return Compatible;
                break;
        }
        return NotCompatible;
    };

    if (m_constraints.empty() && !c.m_constraints.empty()) {
        return false;
    } else if (c.m_constraints.empty()) {
        return true;
    }

    bool compat = false;
    for (auto &&c1: m_constraints) {
        for (auto &&c2: c.m_constraints) {
            auto c = compatibility(c1, c2);
            switch (c) {
                case NotCompatible:
                    return false;
                case Compatible:
                    compat = true;
                case Orthogonal:
                    break;
            }
        }
    }
    return compat;
}

void TypeConstraint::addConstraint(Operator op, int v)
{
    m_constraints.push_back({ op, v, nullptr });
}

void TypeConstraint::add(const TypeConstraint &constraint, const void *source)
{
    for (auto &&c: constraint.m_constraints) {
        m_constraints.push_back({ c.op, c.value, source });
    }
}

void TypeConstraint::addNegate(const TypeConstraint &constraint, const void *source)
{
    for (auto &&c: constraint.m_constraints) {
        switch (c.op) {
            case Operator::Equal:
                m_constraints.push_back({ Operator::NotEqual, c.value, source });
                break;
            default:
                break;
        }
    }
}

void TypeConstraint::addGreater(const TypeConstraint &constraint, const void *source)
{
    for (auto &&c: constraint.m_constraints) {
        switch (c.op) {
            case Operator::Equal:
                m_constraints.push_back({ Operator::GreaterEqual, c.value, source });
                break;
            default:
                break;
        }
    }
}

void TypeConstraint::removeFromSource(void *source)
{
    m_constraints.erase(std::remove_if(m_constraints.begin(), m_constraints.end(),
                                       [&](const Constraint &c) { return c.source == source; }),
                        m_constraints.end());
}

TypeConstraint TypeConstraint::operator/(const TypeConstraint &other) const
{
    auto mergedConstraints = [](const Constraint &c1, const Constraint &c2) -> Optional<Constraint>
    {
        switch (c1.op) {
            case Operator::Equal:
                if (c2.op == Operator::Equal) return Constraint { Operator::Equal, c1.value / c2.value };
                break;
            case Operator::GreaterEqual:
                if (c2.op == Operator::Equal) return Constraint { Operator::GreaterEqual, c1.value / c2.value };
                break;
            case Operator::Greater:
                if (c2.op == Operator::Equal) return Constraint { Operator::GreaterEqual, (c1.value + 1) / c2.value };
                break;
            default:
                break;
        }
        return {};
    };

    TypeConstraint result;
    for (auto &&c1: m_constraints) {
        for (auto &&c2: other.m_constraints) {
            auto c = mergedConstraints(c1, c2);
            if (c) {
                result.m_constraints.push_back(c);
            }
        }
    }
    return result;
}

TypeConstraint TypeConstraint::operator*(const TypeConstraint &other) const
{
    auto mergedConstraints = [](const Constraint &c1, const Constraint &c2) -> Optional<Constraint>
    {
        switch (c1.op) {
            case Operator::Equal:
                if (c2.op == Operator::Equal) return Constraint { Operator::Equal, c1.value * c2.value };
                break;
            case Operator::GreaterEqual:
                if (c2.op == Operator::Equal) return Constraint { Operator::GreaterEqual, c1.value * c2.value };
                break;
            case Operator::Greater:
                if (c2.op == Operator::Equal) return Constraint { Operator::Greater, c1.value * c2.value };
                break;
            default:
                break;
        }
        return {};
    };

    TypeConstraint result;
    for (auto &&c1: m_constraints) {
        for (auto &&c2: other.m_constraints) {
            auto c = mergedConstraints(c1, c2);
            if (c) {
                result.m_constraints.push_back(c);
            }
        }
    }
    return result;
}

TypeConstraint TypeConstraint::operator+(const TypeConstraint &other) const
{
    auto mergedConstraints = [](const Constraint &c1, const Constraint &c2) -> Optional<Constraint>
    {
        switch (c1.op) {
            case Operator::NotEqual:
                if (c2.op == Operator::Equal) return Constraint { Operator::NotEqual, c1.value + c2.value };
                break;
            case Operator::Equal:
                if (c2.op == Operator::Equal) return Constraint { Operator::Equal, c1.value + c2.value };
                break;
            case Operator::GreaterEqual:
                if (c2.op == Operator::Equal) return Constraint { Operator::GreaterEqual, c1.value + c2.value };
                break;
            default:
                break;
        }
        return {};
    };

    TypeConstraint result;
    for (auto &&c1: m_constraints) {
        for (auto &&c2: other.m_constraints) {
            auto c = mergedConstraints(c1, c2);
            if (c) {
                result.m_constraints.push_back(c);
            }
        }
    }
    return result;
}



Value Type::create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Value &storeValue) const
{
    return m_iface->create(ctx, alloc, name, *this, storeValue);
}

Value Type::create(CodeGenContext &ctx, Allocator *alloc, const std::string &name) const
{
    return m_iface->create(ctx, alloc, name, *this, Value());
}




llvm::Type *VoidType::get(CodeGenContext &ctx) const
{
    return llvm::Type::getVoidTy(ctx.context());
}

std::string VoidType::name() const { return "void"; }

Value VoidType::create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const
{
    throw CreateError { CreateError::Err::TypeError };
}



llvm::Type *IntegerType::get(CodeGenContext &ctx) const
{
    return llvm::IntegerType::get(ctx.context(), m_bits);
}

std::string IntegerType::name() const
{
    return std::string(m_signed ? "i" : "u") + std::to_string(m_bits);
}

long long IntegerType::maxValue() const
{
    int shifts = m_bits - m_signed * 1;
    return (1ll << shifts) - 1;
}

long long IntegerType::minValue() const
{
    if (m_signed) {
        return - (1ll << m_bits);
    }
    return 0;
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

static Value createFirstClassValue(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue)
{
    auto t = type.get(ctx);
    auto val = alloc->allocate(t, name);
    if (storeValue.isValid()) {
        auto fc = storeValue.getSpecialization<FirstClassValue>();

        auto storeTy = fc->type();
        while (storeTy.get(ctx) != fc->value()->getType()) {
            storeTy = storeTy.getPointerTo();
        }
        auto store = ctx.convertTo(fc->value(), storeTy, type);
        if (!store || !canStoreInto(ctx, type, fc->type())) {
            throw CreateError { CreateError::Err::StoreError };
        }
        ctx.builder().CreateStore(store, val);
    }

    return createValue(ctx, val, type);
}

Value IntegerType::create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const
{
    return createFirstClassValue(ctx, alloc, name, type, storeValue);
}


llvm::Type *FloatingType::get(CodeGenContext &ctx) const
{
    switch (m_bits) {
        case 16:
            return llvm::Type::getHalfTy(ctx.context());
        case 32:
            return llvm::Type::getFloatTy(ctx.context());
        case 64:
            return llvm::Type::getDoubleTy(ctx.context());
        case 128:
            return llvm::Type::getFP128Ty(ctx.context());
        default:
            break;
    }
    error("unsupported bits size for floating point type: {}", m_bits);
    return nullptr;
}

std::string FloatingType::name() const
{
    return std::string("f") + std::to_string(m_bits);
}

Value FloatingType::create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const
{
    return createFirstClassValue(ctx, alloc, name, type, storeValue);
}



llvm::Type *PointerType::get(CodeGenContext &ctx) const
{
    return m_type.get(ctx)->getPointerTo();
}

std::string PointerType::name() const
{
    return std::string("*(") + m_type.name() + ")";
}

Type PointerType::pointerElementType() const
{
    return m_type;
}

Value PointerType::create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const
{
    return createFirstClassValue(ctx, alloc, name, type, storeValue);
}


llvm::Type *FunctionPointerType::get(CodeGenContext &ctx) const
{
    std::vector<llvm::Type *> argTypes;
    for (auto &&a: m_args) {
        argTypes.push_back(a.get(ctx));
    }
    auto retType = m_ret.get(ctx);
    return llvm::FunctionType::get(retType, llvm::makeArrayRef(argTypes), false);
}

std::string FunctionPointerType::name() const
{
    std::string n("func (");
    int i = 0;
    for (auto &&a: m_args) {
        if (i++ > 0) {
            n += ", ";
        }
        n += a.name();
    }
    n += "): ";
    n += m_ret.name();
    return n;
}

Value FunctionPointerType::create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const
{
    abort();
}



llvm::Type *ArrayType::get(CodeGenContext &ctx) const
{
    auto t = m_type.get(ctx);
    return llvm::ArrayType::get(t, m_num);
}

std::string ArrayType::name() const
{
    return m_type.name() + "[" + std::to_string(m_num) + "]";
}

Value ArrayType::create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const
{
    abort();
}



llvm::Type *ArgumentPackType::get(CodeGenContext &ctx) const
{
    return nullptr;
}

std::string ArgumentPackType::name() const
{
    return "(...)";
}

Value ArgumentPackType::create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const
{
    abort();
}


class LlvmType
{
    TYPE_SPECIALIZATION
public:
    LlvmType(llvm::Type *t) : m_type(t) {}

    llvm::Type *get(CodeGenContext &ctx) const { return m_type; }
    std::string name() const;

    Value create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const
    {
        return createFirstClassValue(ctx, alloc, name, type, storeValue);
    }

private:
    llvm::Type *m_type;
};

llvm::Type *TupleType::get(CodeGenContext &ctx) const
{
    std::vector<llvm::Type *> ty;
    for (auto &&t: m_types) {
        ty.push_back(t.get(ctx));
    }
    return ctx.tupleType(ty);
}

std::string TupleType::name() const
{
    std::string n = "(";
    int i = 0;
    for (auto &&t: m_types) {
        if (i++ > 0) {
            n += ", ";
        }
        n += t.name();
    }
    n += ")";
    return n;
}

Value TupleType::create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const
{
    std::vector<Value> values;
    auto pack = storeValue.getSpecialization<PackValue>();
    if (!pack) {
        throw CreateError { CreateError::Err::StoreError };
    }

    auto &storeValues = pack->unpack();
    for (size_t i = 0; i < m_types.size(); ++i) {
        values.push_back(m_types[i].create(ctx, alloc, name + "#" + std::to_string(i), storeValues[i]));
    }

    return TupleValue(values);
}

StructType::StructType(const std::string &n)
          : m_name(n)
          , m_type(nullptr)
{
}

StructType::StructType(llvm::StructType *type)
          : m_name(type->getName())
          , m_type(type)
{
}

llvm::Type *StructType::get(CodeGenContext &ctx) const
{
    if (!m_type) {
        if (auto t = ctx.declaredType(m_name)) {
            m_type = static_cast<llvm::StructType *>(t);
        }
        if (!m_type) {
            fmt::print("CREATE TYPE {}\n",m_name);
            m_type = llvm::StructType::create(ctx.context(), m_name.c_str());
            ctx.addDeclaredType(m_name, m_type);
        }
    }
    return m_type;
}

std::string StructType::name() const
{
    return m_name;
}

Value StructType::create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const
{
    auto info = ctx.structInfo(get(ctx));

    llvm::Value *sizeValue = nullptr;
    if (info->sizeValue.isValid()) {
        sizeValue = info->sizeValue.getSpecialization<FirstClassValue>()->load(ctx);
    }

    llvm::Value *val;
    if (sizeValue) {
        val = alloc->allocateSized(m_type, sizeValue, name);
    } else {
        val = alloc->allocate(m_type, name);
    }

    if (storeValue.isValid()) {
        auto fc = storeValue.getSpecialization<FirstClassValue>();

        auto storeTy = fc->type();
        while (storeTy.get(ctx) != fc->value()->getType()) {
            storeTy = storeTy.getPointerTo();
        }
        auto store = ctx.convertTo(fc->value(), storeTy, type);
        if (!store || !canStoreInto(ctx, type, fc->type())) {
            throw CreateError { CreateError::Err::StoreError };
        }
        ctx.builder().CreateStore(store, val);
    }

    return structValue(type, val, info, ctx);
}

llvm::Type *CustomType::get(CodeGenContext &ctx) const
{
    auto t = ctx.declaredType(m_name);
    if (!t) {
        err(m_token, "type '{}' was not declared in this scope", m_name);
    }
    return t;
}

std::string CustomType::name() const
{
    return m_name;
}

Value CustomType::create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const
{
    auto t = get(ctx);
    return llvmType(ctx, t).create(ctx, alloc, name, storeValue);
}


std::string LlvmType::name() const
{
    return typeName(m_type);
}

Type llvmType(CodeGenContext &ctx, llvm::Type *t)
{
    int ptr = 0;
    while (t->isPointerTy()) {
        ++ptr;
        t = t->getPointerElementType();
    }

    Type result = [&]() -> Type {
        if (t->isStructTy()) {
            auto st = static_cast<llvm::StructType *>(t);
            if (ctx.structInfo(st)) {
                return StructType(st);
            }
        }
        return LlvmType(t);
    }();

    while (ptr > 0) {
        result = result.getPointerTo();
        --ptr;
    }
    return result;
}



DynamicArrayType::DynamicArrayType(const Type &t)
                : m_elmType(t)
{
}

std::string DynamicArrayType::name() const
{
    return std::string("[") + m_elmType.name() + std::string("]");
}

llvm::Type *DynamicArrayType::get(CodeGenContext &ctx) const
{
    auto elmType = m_elmType.get(ctx);
    llvm::Type *types[] = { elmType->getPointerTo(), //data pointer
                            ctx.builder().getInt32Ty(), //used
                            ctx.builder().getInt32Ty() //allocated
                          };
    auto type = llvm::StructType::get(ctx.context(), llvm::makeArrayRef(types, 3));

    auto appendName = std::string("append_") + name() + "_" + m_elmType.name();
    if (!ctx.module().getFunction(appendName.c_str())) {
        llvm::DataLayout layout(&ctx.module());
        int elmSize = layout.getTypeSizeInBits(elmType) / 8;

        auto callBaseLibFunc = [&](const char *name, const std::vector<llvm::Value *> &args, llvm::Type *retType) -> llvm::Value * {
            auto func = ctx.module().getFunction(name);
            if (!func) {
                std::vector<llvm::Type *> types;
                for (auto &&a: args) {
                    types.push_back(a->getType());
                }
                llvm::FunctionType *ftype = llvm::FunctionType::get(retType, llvm::makeArrayRef(types), false);
                func = llvm::Function::Create(ftype, llvm::GlobalValue::ExternalLinkage, name, &ctx.module());
            }
            return ctx.builder().CreateCall(func, llvm::makeArrayRef(args));
        };

        auto builtinFunction = [&](const std::string &name, const std::vector<Type> types, const Type &retType, const std::function<void (const std::vector<Value> &values)> &block)
        {
            std::vector<llvm::Type *> argTypes;
            for (auto &&t: types) {
                argTypes.push_back(t.get(ctx));
            }

            llvm::FunctionType *ftype = llvm::FunctionType::get(retType.get(ctx), llvm::makeArrayRef(argTypes), false);
            llvm::Function *function = llvm::Function::Create(ftype, llvm::GlobalValue::InternalLinkage, name.c_str(), &ctx.module());

            auto info = ctx.addFunctionInfo(function);
            info->argTypes = types;
            info->returnType = retType;

            StackAllocator allocator(ctx);
            llvm::BasicBlock *bblock = llvm::BasicBlock::Create(ctx.context(), "entry", function, 0);
            ctx.pushBlock(bblock, function, nullptr);

            llvm::Function::arg_iterator argsValues = function->arg_begin();
            std::vector<Value> values;
            for (auto &&t: types) {
                values.push_back(t.create(ctx, &allocator, "", createValue(ctx, &*argsValues++, t)));
            }

            block(values);

            ctx.popBlock();
        };

        // we use llvmType here instead of *this, because otherwise when calling get() on Type(*this) inside builtinFunction we
        // would reenter this function, and since the append function is not created yet we would start an infinite loop
        builtinFunction(appendName, { llvmType(ctx, type).getPointerTo(), m_elmType }, VoidType(), [&](const std::vector<Value> &values) {
            std::vector<llvm::Value *> vals;
            vals.push_back(values[0].getSpecialization<FirstClassValue>()->load(ctx));
            vals.push_back(values[1].getSpecialization<FirstClassValue>()->value());
            vals.push_back(ctx.builder().getInt32(elmSize));

            callBaseLibFunc("__base_lib__vector_append", vals, ctx.builder().getVoidTy());
            ctx.builder().CreateRetVoid();
        });

        auto clearName = std::string("clear_") + name();
        builtinFunction(clearName, { Type(*this).getPointerTo() }, VoidType(), [&](const std::vector<Value> &values) {
            std::vector<llvm::Value *> vals;
            vals.push_back(values[0].getSpecialization<FirstClassValue>()->load(ctx));

            callBaseLibFunc("__base_lib__vector_clear", vals, ctx.builder().getVoidTy());
            ctx.builder().CreateRetVoid();
        });

        auto opName = std::string("operator[]_") + name() + "_i32";
        builtinFunction(opName, { Type(*this).getPointerTo(), IntegerType(true, 32) }, m_elmType.getPointerTo(), [&](const std::vector<Value> &values) {
            std::vector<llvm::Value *> vals;
            vals.push_back(values[0].getSpecialization<FirstClassValue>()->load(ctx));
            vals.push_back(ctx.builder().getInt32(elmSize));
            vals.push_back(values[1].getSpecialization<FirstClassValue>()->load(ctx));

            llvm::Value *ret = callBaseLibFunc("__base_lib__vector_element", vals, ctx.builder().getInt8PtrTy());
            ret = ctx.builder().CreateBitCast(ret, elmType->getPointerTo());
            ctx.builder().CreateRet(ret);
        });

        auto countName = std::string("count_") + name();
        builtinFunction(countName, { Type(*this).getPointerTo() }, IntegerType(true, 32), [&](const std::vector<Value> &values) {
            std::vector<llvm::Value *> vals;
            vals.push_back(values[0].getSpecialization<FirstClassValue>()->load(ctx));

            llvm::Value *ret = callBaseLibFunc("__base_lib__vector_count", vals, ctx.builder().getInt32Ty());
            ctx.builder().CreateRet(ret);
        });
    }

    return type;
}

Value DynamicArrayType::create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const
{
    auto t = static_cast<llvm::StructType *>(get(ctx));
    auto val = alloc->allocate(t, name);

    auto init = llvm::ConstantAggregateZero::get(t);
    ctx.builder().CreateStore(init, val);

    return simpleValue(val, type);
}
