
#include "node.h"
#include "codegen.h"
#include "common.h"

static Value *integerBinOp(Value *lhsValue, Value *rhsValue, NBinaryOperator::OP op, CodeGenContext &ctx)
{
    auto instr = [&]() -> int {
        switch (op) {
            case NBinaryOperator::OP::Add: return Instruction::Add;
            case NBinaryOperator::OP::Mul: return Instruction::Mul;
            case NBinaryOperator::OP::Sub: return Instruction::Sub;
            case NBinaryOperator::OP::Div: return Instruction::SDiv;
            case NBinaryOperator::OP::Equal: return CmpInst::ICMP_EQ;
            case NBinaryOperator::OP::Lesser: return CmpInst::ICMP_SLT;
            case NBinaryOperator::OP::Greater: return CmpInst::ICMP_SGT;
        }
        return -1;
    }();

    switch (op) {
        case NBinaryOperator::OP::Add:
        case NBinaryOperator::OP::Mul:
        case NBinaryOperator::OP::Sub:
        case NBinaryOperator::OP::Div:
            return BinaryOperator::Create((Instruction::BinaryOps)instr, lhsValue, rhsValue, "", ctx.currentBlock()->block);
        case NBinaryOperator::OP::Equal:
        case NBinaryOperator::OP::Lesser:
        case NBinaryOperator::OP::Greater: {
            auto cmp = new ICmpInst(*ctx.currentBlock()->block, (CmpInst::Predicate)instr, lhsValue, rhsValue);
            return cmp;
//                 return CastInst::CreateIntegerCast(cmp, Type::getInt8Ty(context.TheContext), false, "", context.currentBlock()->block);
        }
    }
    return nullptr;
}

static Value *floatBinOp(Value *lhsValue, Value *rhsValue, NBinaryOperator::OP op, CodeGenContext &ctx)
{
    auto instr = [&]() -> int {
        switch (op) {
            case NBinaryOperator::OP::Add: return Instruction::FAdd;
            case NBinaryOperator::OP::Mul: return Instruction::FMul;
            case NBinaryOperator::OP::Sub: return Instruction::FSub;
            case NBinaryOperator::OP::Div: return Instruction::FDiv;
        }
        return -1;
    }();

    switch (op) {
        case NBinaryOperator::OP::Add:
        case NBinaryOperator::OP::Mul:
        case NBinaryOperator::OP::Sub:
        case NBinaryOperator::OP::Div:
            return BinaryOperator::Create((Instruction::BinaryOps)instr, lhsValue, rhsValue, "", ctx.currentBlock()->block);
        case NBinaryOperator::OP::Equal:
        case NBinaryOperator::OP::Lesser:
        case NBinaryOperator::OP::Greater: {
            auto cmp = new FCmpInst(*ctx.currentBlock()->block, (CmpInst::Predicate)instr, lhsValue, rhsValue);
            return cmp;
//                 return CastInst::CreateIntegerCast(cmp, Type::getInt8Ty(context.TheContext), false, "", context.currentBlock()->block);
        }
    }
    return nullptr;
}

static Value *pointerBinOp(Value *lhsValue, Value *rhsValue, NBinaryOperator::OP op, CodeGenContext &ctx)
{
    lhsValue = new PtrToIntInst(lhsValue, Type::getInt32Ty(ctx.TheContext), "", ctx.currentBlock()->block);
    rhsValue = new PtrToIntInst(rhsValue, Type::getInt32Ty(ctx.TheContext), "", ctx.currentBlock()->block);
    auto instr = [&]() -> int {
        switch (op) {
            case NBinaryOperator::OP::Add: return Instruction::Add;
            case NBinaryOperator::OP::Sub: return Instruction::Sub;
        }
        return -1;
    }();

    switch (op) {
        case NBinaryOperator::OP::Add:
        case NBinaryOperator::OP::Mul:
        case NBinaryOperator::OP::Sub:
        case NBinaryOperator::OP::Div:
            return BinaryOperator::Create((Instruction::BinaryOps)instr, lhsValue, rhsValue, "", ctx.currentBlock()->block);
        case NBinaryOperator::OP::Equal:
        case NBinaryOperator::OP::Lesser:
        case NBinaryOperator::OP::Greater: {
            auto cmp = new FCmpInst(*ctx.currentBlock()->block, (CmpInst::Predicate)instr, lhsValue, rhsValue);
            return cmp;
//                 return CastInst::CreateIntegerCast(cmp, Type::getInt8Ty(context.TheContext), false, "", context.currentBlock()->block);
        }
    }
    return nullptr;
}

static Value *pointerIntBinOp(Value *lhsValue, Value *rhsValue, NBinaryOperator::OP op, CodeGenContext &ctx)
{
    auto instr = [&]() -> int {
        switch (op) {
            case NBinaryOperator::OP::Add: return Instruction::Add;
            case NBinaryOperator::OP::Sub: return Instruction::Sub;
        }
        return -1;
    }();

    switch (op) {
        case NBinaryOperator::OP::Add:
        case NBinaryOperator::OP::Sub: {
            auto id1 = ConstantInt::get(ctx.TheContext, llvm::APInt(32, 0, false));
//             auto id2 = ConstantInt::get(context.TheContext, llvm::APInt(32, 0, false));

            return GetElementPtrInst::CreateInBounds(lhsValue, { rhsValue }, "", ctx.currentBlock()->block);
        }
        default:
            break;
    }
    return nullptr;
}

Value *NBinaryOperator::codeGen(CodeGenContext& context)
{
    auto rhsExprs = rhs->unpack(context);
    auto lhsExprs = lhs->unpack(context);
    if (rhsExprs.size() != lhsExprs.size()) {
        err(token(), "both operands must have the same cardinality");
    }
    assert(rhsExprs.size() == 1);
    for (size_t i = 0; i < rhsExprs.size(); ++i) {
        auto lhsValue = lhsExprs[i]->load(context);
        auto rhsValue = rhsExprs[i]->load(context);
        auto lhst = lhsValue->getType();
        auto rhst = rhsValue->getType();
        bool sameType = lhst == rhst;

        Value *value = nullptr;
        if (lhst->isIntegerTy() && sameType) {
            value = integerBinOp(lhsValue, rhsValue, op, context);
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
        return value;
    }
    return nullptr;
}
