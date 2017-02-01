
#include "value.h"
#include "codegen.h"
#include "common.h"

Value simpleValue(llvm::Value *val, const Type &type)
{
    struct SimpleValH
    {
        SimpleValH(llvm::Value *val, const Type &t) : values({{ val, t, true }}) {}

        const std::vector<Value::V> &unpack() const
        {
            return values;
        }
        std::vector<Value::V> &unpack()
        {
            return values;
        }
        Value::V extract(int id) const
        {
            if (id != 0) {
                throw OutOfRangeException(values[0].type.name(), 1);
            }
            return values[0];
        }
        Value::V extract(const std::string &name) const
        {
            return { nullptr, {} };
        }

        SimpleValH clone(std::vector<Value::V> &values) const
        {
            return SimpleValH(values[0].value, values[0].type);
        }

        std::vector<Value::V> values;
    };
    SimpleValH h(val, type);
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
    std::vector<Value::V> &unpack()
    {
        return values;
    }
    Value::V extract(int id) const
    {
        if (id < 0 || id >= (int)values.size()) {
            std::string name = "(";
            for (auto it = values.begin(); it != values.end(); ++it) {
                auto &&v = *it;
                name += v.type.name();
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
        return { nullptr,  {} };
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

Value structValue(const Type &t, llvm::Value *alloc, llvm::Type *type, const StructInfo *i, CodeGenContext &c)
{
    struct StructValueH
    {
        StructValueH(llvm::Value *alloc, llvm::Type *type, const StructInfo *i, CodeGenContext &c)
            : value({{alloc, LlvmType(type), true}}), info(i), ctx(c)
        {
        }

        const std::vector<Value::V> &unpack() const
        {
            return value;
        }
        std::vector<Value::V> &unpack()
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
            return { llvm::GetElementPtrInst::CreateInBounds(v, {id1, id2}, "", ctx.currentBlock()->block), LlvmType(st->elements()[id]), mut };
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
            return StructValueH(values[0].value, values[0].type.get(ctx), info, ctx);
        }

        std::vector<Value::V> value;
        const StructInfo *info;
        CodeGenContext &ctx;
    };
    StructValueH h(alloc, type, i, c);
    return Value(std::move(h));
}

Value unionValue(const Type &t, llvm::Value *alloc, llvm::Type *type, const UnionInfo *i, CodeGenContext &c)
{
    struct UnionValue
    {
        UnionValue(llvm::Value *alloc, llvm::Type *type, const UnionInfo *i, CodeGenContext &c)
            : value({{alloc, LlvmType(type), true}}), info(i), ctx(c)
        {
        }

        const std::vector<Value::V> &unpack() const
        {
            return value;
        }
        std::vector<Value::V> &unpack()
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

            bool mut = info->fields[id].mut;
            auto type = info->fields[id].type.get(ctx)->getPointerTo();

            return { new llvm::BitCastInst(v, type, "", ctx.currentBlock()->block), info->fields[id].type, mut };


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
        UnionValue clone(std::vector<Value::V> &values) const
        {
            return UnionValue(values[0].value, values[0].type.get(ctx), info, ctx);
        }

        std::vector<Value::V> value;
        const UnionInfo *info;
        CodeGenContext &ctx;
    };
    UnionValue h(alloc, type, i, c);
    return Value(std::move(h));
}

Value tupleValue(const Type &t, llvm::Value *alloc, llvm::Type *type, const TupleInfo *i, CodeGenContext &c)
{
    struct TupleValueH
    {
        TupleValueH(llvm::Value *alloc, llvm::Type *type, const TupleInfo *i, CodeGenContext &c)
            : value({{alloc, LlvmType(type), true}}), info(i), ctx(c)
        {
        }

        const std::vector<Value::V> &unpack() const
        {
            return value;
        }
        std::vector<Value::V> &unpack()
        {
            return value;
        }
        Value::V extract(int id) const
        {
            if (id != 0) {
                throw OutOfRangeException(value[0].type.name(), 1);
            }
            return value[0];
        }
        Value::V extract(const std::string &name) const
        {
            throw InvalidFieldException(typeName(info->type));
            return { nullptr, {} };
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
                values.push_back({val, LlvmType(val->getType())});
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
    return ctx.convertTo(value, type.get(ctx));
}

void Value::setMutable(bool m)
{
    if (m) {
        m_flags = m_flags | m;
    } else {
        m_flags = m_flags & ~m;
    }
}
