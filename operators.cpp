
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
        }
        return -1;
    }();

    switch (op) {
        case NBinaryOperator::OP::Remainder:
        case NBinaryOperator::OP::Div: {
            if (!rhsType.typeConstraint().isCompatibleWith(TypeConstraint(TypeConstraint::Operator::NotEqual, 0))) {
                err(token, "divisor value is of type '{}' and may be 0; guard the operation with an if statement", rhsType.name());
            }
        } //fallthrough
        case NBinaryOperator::OP::Add:
        case NBinaryOperator::OP::Mul:
        case NBinaryOperator::OP::Sub: {
            auto result = llvm::BinaryOperator::Create((llvm::Instruction::BinaryOps)instr, lhsValue, rhsValue, "", ctx.currentBlock()->block);
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
            auto cmp = new llvm::ICmpInst(*ctx.currentBlock()->block, (llvm::CmpInst::Predicate)instr, lhsValue, rhsValue);
            return simpleValue(cmp, llvmType(cmp->getType()));
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
            return llvm::BinaryOperator::Create((llvm::Instruction::BinaryOps)instr, lhsValue, rhsValue, "", ctx.currentBlock()->block);
        case NBinaryOperator::OP::Equal:
        case NBinaryOperator::OP::NotEqual:
        case NBinaryOperator::OP::Lesser:
        case NBinaryOperator::OP::Greater:
        case NBinaryOperator::OP::GreaterEqual:
        case NBinaryOperator::OP::LesserEqual: {
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
        case NBinaryOperator::OP::Remainder:
            return llvm::BinaryOperator::Create((llvm::Instruction::BinaryOps)instr, lhsValue, rhsValue, "", ctx.currentBlock()->block);
        case NBinaryOperator::OP::Equal:
        case NBinaryOperator::OP::NotEqual:
        case NBinaryOperator::OP::Lesser:
        case NBinaryOperator::OP::Greater:
        case NBinaryOperator::OP::GreaterEqual:
        case NBinaryOperator::OP::LesserEqual: {
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
    auto rhsExprs = rhsVal->getSpecialization<FirstClassValue>();
    auto lhsVal = lhs->codeGen(context);
    auto lhsExprs = lhsVal->getSpecialization<FirstClassValue>();

    fmt::print("binop {} {}\n",lhsVal->type().name(), rhsVal->type().name());

//     if (rhsExprs.size() != lhsExprs.size()) {
//         err(token(), "both operands must have the same cardinality");
//     }
//     assert(rhsExprs.size() == 1);
//     for (size_t i = 0; i < rhsExprs.size(); ++i) {
        auto lhsValue = lhsExprs->load(context);
        auto rhsValue = rhsExprs->load(context);
        auto lhst = lhsValue->getType();
        auto rhst = rhsValue->getType();
        bool sameType = lhst == rhst;

        lhsExprs->value()->dump();
        rhsExprs->value()->dump();

        llvm::Value *value = nullptr;
        if (lhst->isIntegerTy() && rhst->isIntegerTy()) {
            auto value = integerBinOp(rhs->token(), lhsValue, lhsExprs->type(), rhsValue, rhsExprs->type(), op, context);

            m_constraints.push_back({ lhsVal, rhsVal });
            return value;

        } else if (lhst->isPointerTy() && sameType) {
            value = pointerBinOp(lhsValue, rhsValue, op, context);
        } else if (lhst->isFloatingPointTy() && sameType) {
            value = floatBinOp(lhsValue, rhsValue, op, context);
        } else if (lhst->isPointerTy() && rhst->isIntegerTy()) {
            value = pointerIntBinOp(lhsValue, rhsValue, op, context);
        }
        if (!value) {
            err(token(), "invalid operands '{}' and '{}' for binary expression", lhsExprs->type().name(), rhsExprs->type().name());
        }
        return simpleValue(value, llvmType(value->getType()));
//     }
    return {};
}

void NBinaryOperator::pushConstraints(bool negate)
{
    for (auto &&c: m_constraints) {
        auto &lt = c.lVal.type();
        auto &rt = c.rVal.type();
        if ((!negate && op == NBinaryOperator::OP::Equal) ||
            (negate && op == NBinaryOperator::OP::NotEqual)) {
            lt.typeConstraint().add(rt.typeConstraint(), this);
        } else if ((!negate && op == NBinaryOperator::OP::NotEqual) ||
                   (negate && op == NBinaryOperator::OP::Equal)) {
            lt.typeConstraint().addNegate(rt.typeConstraint(), this);
        } else if ((!negate && op == NBinaryOperator::OP::GreaterEqual) ||
                   (negate && op == NBinaryOperator::OP::Lesser)) {
            lt.typeConstraint().addGreater(rt.typeConstraint(), this);
        }
    }
}

void NBinaryOperator::popConstraints()
{
    for (auto &&c: m_constraints) {
        auto &lt = c.lVal.type();
        lt.typeConstraint().removeFromSource(this);
    }
}
