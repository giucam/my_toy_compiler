
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

void Type::setTypeConstraint(const TypeConstraint &c)
{
    m_constraint = c;
}

TypeConstraint::TypeConstraint()
{
}

TypeConstraint::TypeConstraint(TypeConstraint::Operator op, int v)
{
    m_constraints.push_back({op, v, nullptr});
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
    auto isCompat = [&](const Constraint &c1, const Constraint &c2) {
        switch (c2.op) {
            case Operator::Equal:
                return c1.op == Operator::Equal && c1.value == c2.value;
            case Operator::NotEqual:
                return (c1.op == Operator::NotEqual && c1.value == c2.value) ||
                       (c1.op == Operator::Equal && c1.value != c2.value) ||
                       (c1.op == Operator::Greater && c1.value >= c2.value) ||
                       (c1.op == Operator::Lesser && c1.value <= c2.value);
            case Operator::Greater:
                return (c1.op == Operator::Equal && c1.value > c2.value) ||
                       (c1.op == Operator::Greater && c1.value >= c2.value);
            case Operator::Lesser:
                return (c1.op == Operator::Equal && c1.value < c2.value) ||
                       (c1.op == Operator::Lesser && c1.value <= c2.value);
            case Operator::GreaterEqual:
                return (c1.op == Operator::Equal && c1.value >= c2.value) ||
                       (c1.op == Operator::Greater && c1.value >= c2.value) ||
                       (c1.op == Operator::GreaterEqual && c1.value >= c2.value);
            case Operator::LesserEqual:
                return (c1.op == Operator::Equal && c1.value <= c2.value) ||
                       (c1.op == Operator::Lesser && c1.value <= c2.value) ||
                       (c1.op == Operator::LesserEqual && c1.value <= c2.value);
        }
        return false;
    };

    if (m_constraints.empty() && !c.m_constraints.empty()) {
        return false;
    }

    for (auto &&c1: m_constraints) {
        for (auto &&c2: c.m_constraints) {
            if (!isCompat(c1, c2)) {
                return false;
            }
        }
    }
    return true;
}

void TypeConstraint::addConstraint(Operator op, int v)
{
    m_constraints.push_back({ op, v, nullptr });
}

void TypeConstraint::add(const TypeConstraint &constraint, void *source)
{
    for (auto &&c: constraint.m_constraints) {
        m_constraints.push_back({c.op, c.value, source});
    }
}

void TypeConstraint::addNegate(const TypeConstraint &constraint, void *source)
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

void TypeConstraint::removeFromSource(void *source)
{
    m_constraints.erase(std::remove_if(m_constraints.begin(), m_constraints.end(),
                                       [&](const Constraint &c) { return c.source == source; }),
                        m_constraints.end());
}

llvm::Type *VoidType::get(CodeGenContext &ctx) const
{
    return llvm::Type::getVoidTy(ctx.context());
}

std::string VoidType::name() const { return "void"; }


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
    return (1 << shifts) - 1;
}

long long IntegerType::minValue() const
{
    if (m_signed) {
        return - (1 << m_bits);
    }
    return 0;
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


llvm::Type *PointerType::get(CodeGenContext &ctx) const
{
    return m_type.get(ctx)->getPointerTo();
}

std::string PointerType::name() const
{
    return std::string("*") + m_type.name();
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


llvm::Type *ArrayType::get(CodeGenContext &ctx) const
{
    auto t = m_type.get(ctx);
    return llvm::ArrayType::get(t, m_num);
}

std::string ArrayType::name() const
{
    return m_type.name() + "[" + std::to_string(m_num) + "]";
}


llvm::Type *ArgumentPackType::get(CodeGenContext &ctx) const
{
    return nullptr;
}

std::string ArgumentPackType::name() const
{
    return "(...)";
}


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
        if (i > 0) {
            n += ", ";
        }
        n += t.name();
    }
    n += ")";
    return n;
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


std::string LlvmType::name() const
{
    return typeName(m_type);
}
