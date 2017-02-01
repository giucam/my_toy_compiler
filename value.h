
#ifndef VALUE_H
#define VALUE_H

#include <string>
#include <vector>
#include <memory>

#include "types.h"

namespace llvm {
    class Value;
    class Type;
}

class CodeGenContext;
class StructInfo;
class TupleInfo;
class UnionInfo;

class OutOfRangeException
{
public:
    OutOfRangeException(const std::string &type, int size) : type(type), size(size) {}

    std::string type;
    int size;
};

class InvalidFieldException
{
public:
    InvalidFieldException(const std::string &type) : type(type) {}
    std::string type;
};

class Value {
public:
    struct V {
        llvm::Value *value;
        Type type;
        bool mut;

        llvm::Value *load(CodeGenContext &ctx) const;
    };

private:
    struct IfaceBase
    {
        virtual ~IfaceBase() {}

        virtual const std::vector<Value::V> &unpack() const = 0;
        virtual std::vector<Value::V> &unpack() = 0;
        virtual Value::V extract(int id) const = 0;
        virtual Value::V extract(const std::string &name) const = 0;

        virtual std::shared_ptr<IfaceBase> clone(std::vector<V> &values) const = 0;
    };
    template<class T>
    struct Iface : IfaceBase
    {
        Iface(T h) : handler(std::move(h)) {}
        const std::vector<Value::V> &unpack() const override { return handler.unpack(); }
        std::vector<Value::V> &unpack() override { return handler.unpack(); }
        Value::V extract(int id) const override { return handler.extract(id); }
        Value::V extract(const std::string &name) const override { return handler.extract(name); }
        std::shared_ptr<IfaceBase> clone(std::vector<V> &values) const override { auto clone = handler.clone(values); return std::make_shared<Iface<decltype(clone)>>(clone); }
        T handler;
    };

public:
    enum class Flags
    {
        None = 0,
        Mutable = 1,
    };

    Value() {}
    template<class T>
    Value(T handler, Flags flags = Flags::None)
        : m_iface(std::make_shared<Iface<T>>(std::move(handler))), m_flags((int)flags) {}
    Value(const std::shared_ptr<IfaceBase> &iface, Flags flags = Flags::None)
        : m_iface(iface), m_flags((int)flags) {}

    inline const std::vector<V> &unpack() const { return m_iface->unpack(); }
    inline std::vector<V> &unpack() { return m_iface->unpack(); }
    inline Value::V extract(int id) const { return m_iface->extract(id); }
    inline Value::V extract(const std::string &name) const { return m_iface->extract(name); }

    Value clone(std::vector<V> &values) const { return Value(m_iface->clone(values)); }

    void setMutable(bool m);
    bool isMutable() const { return m_flags & (int)Flags::Mutable; }

private:
    std::shared_ptr<IfaceBase> m_iface;
    int m_flags;
};

Value simpleValue(llvm::Value *val, const Type &type);
Value valuePack(std::vector<Value::V> &vec);
Value structValue(const Type &t, llvm::Value *alloc, llvm::Type *type, const StructInfo *i, CodeGenContext &c);
Value unionValue(const Type &t, llvm::Value *alloc, llvm::Type *type, const UnionInfo *i, CodeGenContext &c);
Value tupleValue(const Type &t, llvm::Value *alloc, llvm::Type *type, const TupleInfo *i, CodeGenContext &c);

#endif
