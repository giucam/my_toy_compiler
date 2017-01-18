
#include "value.h"
#include "codegen.h"
#include "common.h"

Value simpleValue(llvm::Value *val, llvm::Type *t)
{
    if (!t) {
        t = val->getType();
    }

    struct SimpleValH
    {
        SimpleValH(llvm::Value *val, llvm::Type *t) : values({{ val, t, true }}) {}

        const std::vector<Value::V> &unpack() const
        {
            return values;
        }
        Value::V extract(int id) const
        {
            if (id != 0) {
                throw OutOfRangeException(typeName(values[0].type), 1);
            }
            return values[0];
        }
        Value::V extract(const std::string &name) const
        {
            return { nullptr, nullptr };
        }

        SimpleValH clone(std::vector<Value::V> &values) const
        {
            return SimpleValH(values[0].value, values[0].type);
        }

        std::vector<Value::V> values;
    };
    SimpleValH h(val, t);
    return Value(std::move(h));
}

struct ValuePackH
{
    ValuePackH(std::vector<Value::V> &vec) { std::swap(vec, values); }
    std::vector<Value::V> values;
    const std::vector<Value::V> &unpack() const
    {
        return values;
    }
    Value::V extract(int id) const
    {
        if (id < 0 || id >= (int)values.size()) {
            std::string name = "(";
            for (auto it = values.begin(); it != values.end(); ++it) {
                auto &&v = *it;
                name += typeName(v.type);
                if (it + 1 != values.end()) {
                    name += ", ";
                }
            }
            name += ")";

            throw OutOfRangeException(name, values.size());
        }
        return unpack()[id];
    }
    Value::V extract(const std::string &name) const
    {
        return { nullptr, nullptr };
    }
    ValuePackH clone(std::vector<Value::V> &values) const
    {
        return ValuePackH(values);
    }
};

Value valuePack(std::vector<Value::V> &vec)
{
    ValuePackH h(vec);
    return Value(std::move(h));
}

Value structValue(llvm::Value *alloc, llvm::Type *type, const StructInfo *i, CodeGenContext &c)
{
    struct StructValueH
    {
        StructValueH(llvm::Value *alloc, llvm::Type *type, const StructInfo *i, CodeGenContext &c)
            : value({{alloc, type, true}}), info(i), ctx(c)
        {
        }

        const std::vector<Value::V> &unpack() const
        {
            return value;
        }
        Value::V extract(int id) const
        {
            if (id < 0 || id >= (int)info->fields.size()) {
                throw OutOfRangeException(typeName(info->type), info->fields.size());
            }

            auto v = value[0].value;
            while (v->getType()->isPointerTy() && v->getType()->getPointerElementType()->isPointerTy()) {
                v = new llvm::LoadInst(v, "", false, ctx.currentBlock()->block);
            }

            auto id1 = llvm::ConstantInt::get(ctx.context(), llvm::APInt(32, 0, false));
            auto id2 = llvm::ConstantInt::get(ctx.context(), llvm::APInt(32, id, false));

            auto st = static_cast<llvm::StructType *>(info->type);

            bool mut = info->fields[id].mut;
            return { llvm::GetElementPtrInst::CreateInBounds(v, {id1, id2}, "", ctx.currentBlock()->block), st->elements()[id], mut };
        }
        Value::V extract(const std::string &name) const
        {
            int id = -1;
            for (size_t i = 0; i < info->fields.size(); ++i) {
                if (info->fields[i].name == name) {
                    id = i;
                    break;
                }
            }
            if (id == -1) {
                throw InvalidFieldException(typeName(info->type));
            }
            return extract(id);
        }
        StructValueH clone(std::vector<Value::V> &values) const
        {
            return StructValueH(values[0].value, values[0].type, info, ctx);
        }

        std::vector<Value::V> value;
        const StructInfo *info;
        CodeGenContext &ctx;
    };
    StructValueH h(alloc, type, i, c);
    return Value(std::move(h));
}

Value tupleValue(llvm::Value *alloc, llvm::Type *type, const TupleInfo *i, CodeGenContext &c)
{
    struct TupleValueH
    {
        TupleValueH(llvm::Value *alloc, llvm::Type *type, const TupleInfo *i, CodeGenContext &c)
            : value({{alloc, type, true}}), info(i), ctx(c)
        {
        }

        const std::vector<Value::V> &unpack() const
        {
            return value;
        }
        Value::V extract(int id) const
        {
            if (id != 0) {
                throw OutOfRangeException(typeName(value[0].type), 1);
            }
            return value[0];
        }
        Value::V extract(const std::string &name) const
        {
            throw InvalidFieldException(typeName(info->type));
            return { nullptr, nullptr };
        }
        ValuePackH clone(std::vector<Value::V> &values) const
        {
            // TupleValues are only used when returning a tuple from a function. When we then clone it to
            // store it into a variable, create a ValuePack instead, with all the values in the original tuple.

            auto v = values[0].value;
            while (v->getType()->isPointerTy() && v->getType()->getPointerElementType()->isPointerTy()) {
                v = new llvm::LoadInst(v, "", false, ctx.currentBlock()->block);
            }

            values.clear();
            int s = static_cast<llvm::StructType *>(info->type)->elements().size();
            for (int id = 0; id < s; ++id) {

                auto id1 = llvm::ConstantInt::get(ctx.context(), llvm::APInt(32, 0, false));
                auto id2 = llvm::ConstantInt::get(ctx.context(), llvm::APInt(32, id, false));

                auto val = llvm::GetElementPtrInst::CreateInBounds(v, {id1, id2}, "", ctx.currentBlock()->block);
                values.push_back({val, val->getType()});
            }

            return ValuePackH(values);
        }

        std::vector<Value::V> value;
        const TupleInfo *info;
        CodeGenContext &ctx;
    };
    TupleValueH h(alloc, type, i, c);
    return Value(std::move(h));
}

llvm::Value *Value::V::load(CodeGenContext &ctx) const
{
    return ctx.convertTo(value, type);
}

void Value::setMutable(bool m)
{
    if (m) {
        m_flags = m_flags | m;
    } else {
        m_flags = m_flags & ~m;
    }
}
