
#include <sstream>

#include "value.h"
#include "codegen.h"
#include "common.h"

Value createValue(CodeGenContext &ctx, llvm::Value *value, const Type &valueType, bool mut)
{
    auto type = valueType.get(ctx);
    while (type->isPointerTy()) {
        type = type->getPointerElementType();
    }

    Value v = [&]() {
        if (type->isStructTy()) {
            if (auto info = ctx.structInfo(type)) {
                return structValue(valueType, value, info, ctx);
            } else if (auto info = ctx.tupleInfo(type)) {
                return tupleValue(valueType, value, info, ctx);
            } else if (auto info = ctx.unionInfo(type)) {
                return unionValue(valueType, value, info, ctx);
            }
        }
        return simpleValue(value, valueType);
    }();
    v.setMutable(mut);
    return v;
}


llvm::Value *FirstClassValue::load(CodeGenContext &ctx) const
{
    return ctx.convertTo(m_value, LlvmType(m_value->getType()), m_type);
}


StructValue::StructValue(llvm::Value *val, const Type &ty, const StructInfo *info, CodeGenContext &ctx)
           : FirstClassValue(val, ty)
           , m_info(info)
           , m_ctx(ctx)
{
}

Value StructValue::extract(int id) const
{
    if (id < 0 || id >= (int)m_info->fields.size()) {
        throw OutOfRangeException(typeName(m_info->type), m_info->fields.size());
    }

    //cache the values
    std::stringstream ss;
    ss << "__struct_" << this << "_" << id;

    if (auto value = m_ctx.local(ss.str())) {
        return value;
    }

    auto v = value();
    while (v->getType()->isPointerTy() && v->getType()->getPointerElementType()->isPointerTy()) {
        v = new llvm::LoadInst(v, "", false, m_ctx.currentBlock()->block);
    }

    auto id1 = llvm::ConstantInt::get(m_ctx.context(), llvm::APInt(32, 0, false));
    auto id2 = llvm::ConstantInt::get(m_ctx.context(), llvm::APInt(32, id, false));

    bool mut = m_info->fields[id].mut;
    auto value = createValue(m_ctx, llvm::GetElementPtrInst::CreateInBounds(v, {id1, id2}, "", m_ctx.currentBlock()->block), m_info->fields[id].type, mut);
    m_ctx.storeLocal(ss.str(), value);
    return value;
}

Value StructValue::extract(const std::string &name) const
{
    int id = -1;
    for (size_t i = 0; i < m_info->fields.size(); ++i) {
        if (m_info->fields[i].name == name) {
            id = i;
            break;
        }
    }
    if (id == -1) {
        throw InvalidFieldException(typeName(m_info->type));
    }
    return extract(id);
}



UnionValue::UnionValue(llvm::Value *val, const Type &ty, const UnionInfo *info, CodeGenContext &ctx)
          : FirstClassValue(val, ty)
          , m_info(info)
          , m_ctx(ctx)
{
}

Value UnionValue::extract(int id) const
{
    if (id < 0 || id >= (int)m_info->fields.size()) {
        throw OutOfRangeException(typeName(m_info->type), m_info->fields.size());
    }

    auto v = value();
    while (v->getType()->isPointerTy() && v->getType()->getPointerElementType()->isPointerTy()) {
        v = new llvm::LoadInst(v, "", false, m_ctx.currentBlock()->block);
    }

    bool mut = m_info->fields[id].mut;
    auto type = m_info->fields[id].type.get(m_ctx)->getPointerTo();

    return createValue(m_ctx, new llvm::BitCastInst(v, type, "", m_ctx.currentBlock()->block), m_info->fields[id].type, mut);
}

Value UnionValue::extract(const std::string &name) const
{
    int id = -1;
    for (size_t i = 0; i < m_info->fields.size(); ++i) {
        if (m_info->fields[i].name == name) {
            id = i;
            break;
        }
    }
    if (id == -1) {
        throw InvalidFieldException(typeName(m_info->type));
    }
    return extract(id);
}


TupleValue::TupleValue(std::vector<Value> &values)
{
    std::swap(values, m_values);

    std::vector<Type> types;
    for (auto &&v: m_values) {
        types.push_back(v.type());
    }
    m_type = TupleType(types);
}

Type &TupleValue::type()
{
    return m_type;
}

Value TupleValue::extract(int id) const
{
    if (id < 0 || id >= (int)m_values.size()) {
        throw OutOfRangeException(m_type.name(), m_values.size());
    }
    return m_values[id];
}



TupleValueH::TupleValueH(llvm::Value *alloc, const Type &type, const TupleInfo *i, CodeGenContext &c)
    : FirstClassValue(alloc, type)
    , m_info(i)
    , m_ctx(c)
{
}

const std::vector<Value> &TupleValueH::unpack() const
{
    // TupleValues are only used when returning a tuple from a function. When we then clone it to
    // store it into a variable, create a ValuePack instead, with all the values in the original tuple.

    auto v = value();
    while (v->getType()->isPointerTy() && v->getType()->getPointerElementType()->isPointerTy()) {
        v = new llvm::LoadInst(v, "", false, m_ctx.currentBlock()->block);
    }

    values.clear();
    int s = static_cast<llvm::StructType *>(m_info->type)->elements().size();
    for (int id = 0; id < s; ++id) {

        llvm::Value *val;
        if (v->getType()->isPointerTy()) {
            auto id1 = llvm::ConstantInt::get(m_ctx.context(), llvm::APInt(32, 0, false));
            auto id2 = llvm::ConstantInt::get(m_ctx.context(), llvm::APInt(32, id, false));

            val = llvm::GetElementPtrInst::CreateInBounds(v, { id1, id2 }, "", m_ctx.currentBlock()->block);
        } else {
            val = llvm::ExtractValueInst::Create(v, llvm::makeArrayRef((unsigned)id), "", m_ctx.currentBlock()->block);
        }
        values.push_back(createValue(m_ctx, val, LlvmType(val->getType())));
    }

    return values;
}

Value TupleValueH::extract(int id) const
{
    return unpack()[id];
}



Value simpleValue(llvm::Value *val, const Type &type)
{
    return FirstClassValue(val, type);
}

Value valuePack(std::vector<Value> &vec)
{
    return TupleValue(vec);
}

Value structValue(const Type &t, llvm::Value *alloc, const StructInfo *i, CodeGenContext &c)
{
    return StructValue(alloc, t, i, c);
}

Value unionValue(const Type &t, llvm::Value *alloc, const UnionInfo *i, CodeGenContext &c)
{
    return UnionValue(alloc, t, i, c);
}

Value tupleValue(const Type &t, llvm::Value *alloc, const TupleInfo *i, CodeGenContext &c)
{
    return TupleValueH(alloc, t, i, c);
}

void Value::setMutable(bool m)
{
    if (m) {
        m_flags = m_flags | m;
    } else {
        m_flags = m_flags & ~m;
    }
}
