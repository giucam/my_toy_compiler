
#include "node.h"
#include "codegen.h"
#include "common.h"

static Value integerBinOp(const Token &token, llvm::Value *lhsValue, const Type &lhsType, llvm::Value *rhsValue, const Type &rhsType, NBinaryOperator::OP op, CodeGenContext &ctx)
{
    auto instr = [&]() -> int {
        switch (op) {
            case NBinaryOperator::OP::Add: return llvm::Instruction::Add;
            case NBinaryOperator::OP::Mul: return llvm::Instruction::Mul;
            case NBinaryOperator::OP::Sub: return llvm::Instruction::Sub;
            case NBinaryOperator::OP::Div: return llvm::Instruction::SDiv;
            case NBinaryOperator::OP::Remainder: return llvm::Instruction::SRem;
            case NBinaryOperator::OP::Equal: return llvm::CmpInst::ICMP_EQ;
            case NBinaryOperator::OP::NotEqual: return llvm::CmpInst::ICMP_NE;
            case NBinaryOperator::OP::Lesser: return llvm::CmpInst::ICMP_SLT;
            case NBinaryOperator::OP::Greater: return llvm::CmpInst::ICMP_SGT;
            case NBinaryOperator::OP::GreaterEqual: return llvm::CmpInst::ICMP_SGE;
            case NBinaryOperator::OP::LesserEqual: return llvm::CmpInst::ICMP_SLE;
            case NBinaryOperator::OP::Or: return llvm::Instruction::Or;
        }
        return -1;
    }();

    switch (op) {
        case NBinaryOperator::OP::Remainder:
        case NBinaryOperator::OP::Div:
        case NBinaryOperator::OP::Or:
        case NBinaryOperator::OP::Add:
        case NBinaryOperator::OP::Mul:
        case NBinaryOperator::OP::Sub: {
            auto result = ctx.builder().CreateBinOp((llvm::Instruction::BinaryOps)instr, lhsValue, rhsValue);
            Type t = rhsType;
            t.setTypeConstraint([&]() {
                switch (op) {
                    case NBinaryOperator::OP::Div:
                        return lhsType.typeConstraint() / rhsType.typeConstraint();
                    case NBinaryOperator::OP::Mul:
                        return lhsType.typeConstraint() * rhsType.typeConstraint();
                    case NBinaryOperator::OP::Add:
                        return lhsType.typeConstraint() + rhsType.typeConstraint();
                    default:
                        break;
                }
                return TypeConstraint();
            }());
            return simpleValue(result, t);
        }
        case NBinaryOperator::OP::Equal:
        case NBinaryOperator::OP::NotEqual:
        case NBinaryOperator::OP::Lesser:
        case NBinaryOperator::OP::Greater:
        case NBinaryOperator::OP::GreaterEqual:
        case NBinaryOperator::OP::LesserEqual: {
            auto cmp = ctx.builder().CreateICmp((llvm::CmpInst::Predicate)instr, lhsValue, rhsValue);
            return simpleValue(cmp, llvmType(ctx, cmp->getType()));
//                 return CastInst::CreateIntegerCast(cmp, Type::getInt8Ty(context.TheContext), false, "", context.currentBlock()->block);
        }
    }
    return {};
}

static llvm::Value *floatBinOp(llvm::Value *lhsValue, llvm::Value *rhsValue, NBinaryOperator::OP op, CodeGenContext &ctx)
{
    auto instr = [&]() -> int {
        switch (op) {
            case NBinaryOperator::OP::Add: return llvm::Instruction::FAdd;
            case NBinaryOperator::OP::Mul: return llvm::Instruction::FMul;
            case NBinaryOperator::OP::Sub: return llvm::Instruction::FSub;
            case NBinaryOperator::OP::Div: return llvm::Instruction::FDiv;
            case NBinaryOperator::OP::Remainder: return llvm::Instruction::FRem;
            default:
                break;
        }
        return -1;
    }();

    switch (op) {
        case NBinaryOperator::OP::Add:
        case NBinaryOperator::OP::Mul:
        case NBinaryOperator::OP::Sub:
        case NBinaryOperator::OP::Div:
        case NBinaryOperator::OP::Remainder:
        case NBinaryOperator::OP::Or:
            return ctx.builder().CreateBinOp((llvm::Instruction::BinaryOps)instr, lhsValue, rhsValue);
        case NBinaryOperator::OP::Equal:
        case NBinaryOperator::OP::NotEqual:
        case NBinaryOperator::OP::Lesser:
        case NBinaryOperator::OP::Greater:
        case NBinaryOperator::OP::GreaterEqual:
        case NBinaryOperator::OP::LesserEqual: {
            auto cmp = ctx.builder().CreateFCmp((llvm::CmpInst::Predicate)instr, lhsValue, rhsValue);
            return cmp;
//                 return CastInst::CreateIntegerCast(cmp, Type::getInt8Ty(context.TheContext), false, "", context.currentBlock()->block);
        }
    }
    return nullptr;
}

static llvm::Value *pointerBinOp(llvm::Value *lhsValue, llvm::Value *rhsValue, NBinaryOperator::OP op, CodeGenContext &ctx)
{
    lhsValue = ctx.builder().CreatePtrToInt(lhsValue, llvm::Type::getInt32Ty(ctx.context()));
    rhsValue = ctx.builder().CreatePtrToInt(rhsValue, llvm::Type::getInt32Ty(ctx.context()));
    auto instr = [&]() -> int {
        switch (op) {
            case NBinaryOperator::OP::Add: return llvm::Instruction::Add;
            case NBinaryOperator::OP::Sub: return llvm::Instruction::Sub;
            default:
                break;
        }
        return -1;
    }();

    switch (op) {
        case NBinaryOperator::OP::Add:
        case NBinaryOperator::OP::Mul:
        case NBinaryOperator::OP::Sub:
        case NBinaryOperator::OP::Div:
        case NBinaryOperator::OP::Remainder:
        case NBinaryOperator::OP::Or:
            return ctx.builder().CreateBinOp((llvm::Instruction::BinaryOps)instr, lhsValue, rhsValue);
        case NBinaryOperator::OP::Equal:
        case NBinaryOperator::OP::NotEqual:
        case NBinaryOperator::OP::Lesser:
        case NBinaryOperator::OP::Greater:
        case NBinaryOperator::OP::GreaterEqual:
        case NBinaryOperator::OP::LesserEqual: {
            auto cmp = ctx.builder().CreateFCmp((llvm::CmpInst::Predicate)instr, lhsValue, rhsValue);
            return cmp;
//                 return CastInst::CreateIntegerCast(cmp, Type::getInt8Ty(context.TheContext), false, "", context.currentBlock()->block);
        }
    }
    return nullptr;
}

static llvm::Value *pointerIntBinOp(llvm::Value *lhsValue, llvm::Value *rhsValue, NBinaryOperator::OP op, CodeGenContext &ctx)
{
    switch (op) {
        case NBinaryOperator::OP::Add:
        case NBinaryOperator::OP::Sub: {
            return ctx.builder().CreateInBoundsGEP(lhsValue, { rhsValue });
        }
        default:
            break;
    }
    return nullptr;
}

Optional<Value> CodeGenContext::visit(NBinaryOperator &op, int pass)
{
    auto rhsVal = op.rhs->visit(*this, pass);
    auto rhsExprs = rhsVal->getSpecialization<FirstClassValue>();
    auto lhsVal = op.lhs->visit(*this, pass);
    auto lhsExprs = lhsVal->getSpecialization<FirstClassValue>();

    fmt::print("binop {} {} - {}\n",lhsVal->type().name(), rhsVal->type().name(), op.token().lineNo());

//     if (rhsExprs.size() != lhsExprs.size()) {
//         err(token(), "both operands must have the same cardinality");
//     }
//     assert(rhsExprs.size() == 1);
//     for (size_t i = 0; i < rhsExprs.size(); ++i) {
        auto lhsValue = lhsExprs->load(*this);
        auto rhsValue = rhsExprs->load(*this);
        auto lhst = lhsValue->getType();
        auto rhst = rhsValue->getType();
        bool sameType = lhst == rhst;

        llvm::Value *value = nullptr;
        if (lhst->isIntegerTy() && rhst->isIntegerTy()) {
            return integerBinOp(op.rhs->token(), lhsValue, lhsExprs->type(), rhsValue, rhsExprs->type(), op.op, *this);
        } else if (lhst->isPointerTy() && sameType) {
            value = pointerBinOp(lhsValue, rhsValue, op.op, *this);
        } else if (lhst->isFloatingPointTy() && sameType) {
            value = floatBinOp(lhsValue, rhsValue, op.op, *this);
        } else if (lhst->isPointerTy() && rhst->isIntegerTy()) {
            value = pointerIntBinOp(lhsValue, rhsValue, op.op, *this);
        }
        if (!value) {
            err(op.token(), "invalid operands '{}' and '{}' for binary expression", lhsExprs->type().name(), rhsExprs->type().name());
        }
        return simpleValue(value, llvmType(*this, value->getType()));
//     }
    return {};
}

void NBinaryOperator::pushConstraints(bool negate)
{
    for (auto &&c: m_constraints) {
        c.pushFunc(negate);
    }
}

void NBinaryOperator::popConstraints()
{
    for (auto &&c: m_constraints) {
        c.popFunc();
    }
}
