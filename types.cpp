
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>

#include "types.h"
#include "codegen.h"
#include "common.h"

Type Type::getPointerTo()
{
    return PointerType(*this);
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


llvm::Type *FloatingType::get(CodeGenContext &ctx) const
{
    switch (m_bits) {
        case 32:
            return llvm::Type::getFloatTy(ctx.context());
        case 64:
            return llvm::Type::getDoubleTy(ctx.context());
        default:
            break;
    }
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
        if (i > 0) {
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
    auto t = ctx.typeOf(m_name);
    if (!t) {
        err(m_token, "type '{}' was not declared in this scope", m_name);
    }
    return t;
}

std::string CustomType::name() const
{
    return m_name;
}
