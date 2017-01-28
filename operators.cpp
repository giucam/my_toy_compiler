
#include "node.h"
#include "codegen.h"
#include "common.h"

static llvm::Value *integerBinOp(const Token &token, llvm::Value *lhsValue, llvm::Value *rhsValue, const Type &rhsType, NBinaryOperator::OP op, CodeGenContext &ctx)
{
    auto instr = [&]() -> int {
        switch (op) {
            case NBinaryOperator::OP::Add: return llvm::Instruction::Add;
            case NBinaryOperator::OP::Mul: return llvm::Instruction::Mul;
            case NBinaryOperator::OP::Sub: return llvm::Instruction::Sub;
            case NBinaryOperator::OP::Div: return llvm::Instruction::SDiv;
            case NBinaryOperator::OP::Equal: return llvm::CmpInst::ICMP_EQ;
            case NBinaryOperator::OP::NotEqual: return llvm::CmpInst::ICMP_NE;
            case NBinaryOperator::OP::Lesser: return llvm::CmpInst::ICMP_SLT;
            case NBinaryOperator::OP::Greater: return llvm::CmpInst::ICMP_SGT;
        }
        return -1;
    }();

    switch (op) {
        case NBinaryOperator::OP::Div: {
            if (!rhsType.typeConstraint().isCompatibleWith(TypeConstraint(TypeConstraint::Operator::NotEqual, 0))) {
                err(token, "divisor value is of type '{}' and may be 0; guard the operation with an if statement", rhsType.name());
            }
        } //fallthrough
        case NBinaryOperator::OP::Add:
        case NBinaryOperator::OP::Mul:
        case NBinaryOperator::OP::Sub:
            return llvm::BinaryOperator::Create((llvm::Instruction::BinaryOps)instr, lhsValue, rhsValue, "", ctx.currentBlock()->block);
        case NBinaryOperator::OP::Equal:
        case NBinaryOperator::OP::NotEqual:
        case NBinaryOperator::OP::Lesser:
        case NBinaryOperator::OP::Greater: {
            auto cmp = new llvm::ICmpInst(*ctx.currentBlock()->block, (llvm::CmpInst::Predicate)instr, lhsValue, rhsValue);
            return cmp;
//                 return CastInst::CreateIntegerCast(cmp, Type::getInt8Ty(context.TheContext), false, "", context.currentBlock()->block);
        }
    }
    return nullptr;
}

static llvm::Value *floatBinOp(llvm::Value *lhsValue, llvm::Value *rhsValue, NBinaryOperator::OP op, CodeGenContext &ctx)
{
    auto instr = [&]() -> int {
        switch (op) {
            case NBinaryOperator::OP::Add: return llvm::Instruction::FAdd;
            case NBinaryOperator::OP::Mul: return llvm::Instruction::FMul;
            case NBinaryOperator::OP::Sub: return llvm::Instruction::FSub;
            case NBinaryOperator::OP::Div: return llvm::Instruction::FDiv;
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
            return llvm::BinaryOperator::Create((llvm::Instruction::BinaryOps)instr, lhsValue, rhsValue, "", ctx.currentBlock()->block);
        case NBinaryOperator::OP::Equal:
        case NBinaryOperator::OP::NotEqual:
        case NBinaryOperator::OP::Lesser:
        case NBinaryOperator::OP::Greater: {
            auto cmp = new llvm::FCmpInst(*ctx.currentBlock()->block, (llvm::CmpInst::Predicate)instr, lhsValue, rhsValue);
            return cmp;
//                 return CastInst::CreateIntegerCast(cmp, Type::getInt8Ty(context.TheContext), false, "", context.currentBlock()->block);
        }
    }
    return nullptr;
}

static llvm::Value *pointerBinOp(llvm::Value *lhsValue, llvm::Value *rhsValue, NBinaryOperator::OP op, CodeGenContext &ctx)
{
    lhsValue = new llvm::PtrToIntInst(lhsValue, llvm::Type::getInt32Ty(ctx.context()), "", ctx.currentBlock()->block);
    rhsValue = new llvm::PtrToIntInst(rhsValue, llvm::Type::getInt32Ty(ctx.context()), "", ctx.currentBlock()->block);
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
            return llvm::BinaryOperator::Create((llvm::Instruction::BinaryOps)instr, lhsValue, rhsValue, "", ctx.currentBlock()->block);
        case NBinaryOperator::OP::Equal:
        case NBinaryOperator::OP::NotEqual:
        case NBinaryOperator::OP::Lesser:
        case NBinaryOperator::OP::Greater: {
            auto cmp = new llvm::FCmpInst(*ctx.currentBlock()->block, (llvm::CmpInst::Predicate)instr, lhsValue, rhsValue);
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
            return llvm::GetElementPtrInst::CreateInBounds(lhsValue, { rhsValue }, "", ctx.currentBlock()->block);
        }
        default:
            break;
    }
    return nullptr;
}

Optional<Value> NBinaryOperator::codeGen(CodeGenContext &context)
{
    auto rhsVal = rhs->codeGen(context);
    auto rhsExprs = rhsVal->unpack();
    auto lhsVal = lhs->codeGen(context);
    auto &lhsExprs = lhsVal->unpack();
    if (rhsExprs.size() != lhsExprs.size()) {
        err(token(), "both operands must have the same cardinality");
    }
    assert(rhsExprs.size() == 1);
    for (size_t i = 0; i < rhsExprs.size(); ++i) {
        auto lhsValue = lhsExprs[i].load(context);
        auto rhsValue = rhsExprs[i].load(context);
        auto lhst = lhsValue->getType();
        auto rhst = rhsValue->getType();
        bool sameType = lhst == rhst;

        lhsExprs[i].value->dump();
        rhsExprs[i].value->dump();

        llvm::Value *value = nullptr;
        if (lhst->isIntegerTy() && sameType) {
            value = integerBinOp(rhs->token(), lhsValue, rhsValue, rhsExprs[i].type, op, context);

            m_constraints.push_back({ lhsVal, rhsVal });
        } else if (lhst->isPointerTy() && sameType) {
            value = pointerBinOp(lhsValue, rhsValue, op, context);
        } else if (lhst->isFloatingPointTy() && sameType) {
            value = floatBinOp(lhsValue, rhsValue, op, context);
        } else if (lhst->isPointerTy() && rhst->isIntegerTy()) {
            value = pointerIntBinOp(lhsValue, rhsValue, op, context);
        }
        if (!value) {
            err(token(), "invalid operands for binary expression");
        }
        return simpleValue(value, LlvmType(value->getType()));
    }
    return {};
}

void NBinaryOperator::pushConstraints(bool negate)
{
    for (auto &&c: m_constraints) {
        auto &lt = c.lVal.unpack()[0].type;
        auto &rt = c.rVal.unpack()[0].type;
        if ((!negate && op == NBinaryOperator::OP::Equal) ||
            (negate && op == NBinaryOperator::OP::NotEqual)) {
            lt.typeConstraint().add(rt.typeConstraint(), this);
        } else if ((!negate && op == NBinaryOperator::OP::NotEqual) ||
                   (negate && op == NBinaryOperator::OP::Equal)) {
            lt.typeConstraint().addNegate(rt.typeConstraint(), this);
        }
    }
}

void NBinaryOperator::popConstraints()
{
    for (auto &&c: m_constraints) {
        auto &lt = c.lVal.unpack()[0].type;
        lt.typeConstraint().removeFromSource(this);
    }
}
