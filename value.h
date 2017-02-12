
#ifndef VALUE_H
#define VALUE_H

#include <string>
#include <vector>
#include <memory>

#include "types.h"
#include "common.h"

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

class ValueBindingPoint
{
public:
    ValueBindingPoint() {}
    ValueBindingPoint(const Type &t) : m_type(t) {}

    const Type &type() const { return m_type; }

private:
    Type m_type;
};

class ValueSpecialization;

class Value
{
    struct IfaceBase
    {
        virtual ~IfaceBase() {}
        virtual Type &type() = 0;
        ValueSpecialization *handlerBase;
        Optional<ValueBindingPoint> bindingPoint;
    };
    template<class T>
    struct Iface : IfaceBase
    {
        Iface(T h) : handler(std::move(h)) { handlerBase = &handler; }
        Type &type() override { return handler.type(); }
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

    void setMutable(bool m);
    bool isMutable() const { return m_flags & (int)Flags::Mutable; }

    void setBindingPoint(const ValueBindingPoint &bp) { m_iface->bindingPoint = bp; }
    const Optional<ValueBindingPoint> &bindingPoint() const { return m_iface->bindingPoint; }

    inline Type &type() { return m_iface->type(); }

    template<class T>
    T *getSpecialization() const
    {
        if (!m_iface) {
            return nullptr;
        }
        return dynamic_cast<T *>(m_iface->handlerBase);
    }

    bool isValid() const { return m_iface.get(); }

private:
    std::shared_ptr<IfaceBase> m_iface;
    int m_flags;
};

#define VALUE_SPECIALIZATION \
static void magic() { } \
friend class Value;

class ValueSpecialization
{
public:
    virtual ~ValueSpecialization() {}
};

class FirstClassValue : public ValueSpecialization
{
public:
    FirstClassValue(llvm::Value *v, const Type &t) : m_value(v), m_type(t) {}

    llvm::Value *value() const { return m_value; }
    Type &type() { return m_type; }
    const Type &type() const { return m_type; }

    llvm::Value *load(CodeGenContext &ctx) const;

private:
    llvm::Value *m_value;
    Type m_type;
};

class AggregateValue
{
public:
    virtual Value extract(int id) const = 0;
};

class NamedAggregateValue : public AggregateValue
{
public:
    virtual Value extract(const std::string &name) const = 0;
};

class PackValue : public AggregateValue
{
public:
    virtual const std::vector<Value> &unpack() const = 0;
};

class TupleValue : public ValueSpecialization, public PackValue
{
public:
    TupleValue(std::vector<Value> &values);

    Type &type();

    const std::vector<Value> &unpack() const override { return m_values; }
    Value extract(int id) const override;

private:
    std::vector<Value> m_values;
    Type m_type;
};

class TupleValueH : public FirstClassValue, public PackValue
{
public:
    TupleValueH(llvm::Value *alloc, const Type &type, const TupleInfo *i, CodeGenContext &c);

    const std::vector<Value> &unpack() const override;
    Value extract(int id) const override;

private:
    const TupleInfo *m_info;
    CodeGenContext &m_ctx;
    mutable std::vector<Value> values;
};

class StructValue : public FirstClassValue, public NamedAggregateValue
{
public:
    StructValue(llvm::Value *val, const Type &ty, const StructInfo *info, CodeGenContext &ctx);

    Value extract(int id) const override;
    Value extract(const std::string &name) const override;

private:
    const StructInfo *m_info;
    CodeGenContext &m_ctx;
};

class UnionValue : public FirstClassValue, public NamedAggregateValue
{
public:
    UnionValue(llvm::Value *val, const Type &ty, const UnionInfo *info, CodeGenContext &ctx);

    Value extract(int id) const override;
    Value extract(const std::string &name) const override;

private:
    const UnionInfo *m_info;
    CodeGenContext &m_ctx;
};


Value createValue(CodeGenContext &ctx, llvm::Value *value, const Type &valueType, bool mut = false);

Value simpleValue(llvm::Value *val, const Type &type);
Value valuePack(std::vector<Value> &vec);
Value structValue(const Type &t, llvm::Value *alloc, const StructInfo *i, CodeGenContext &c);
Value unionValue(const Type &t, llvm::Value *alloc, const UnionInfo *i, CodeGenContext &c);
Value tupleValue(const Type &t, llvm::Value *alloc, const TupleInfo *i, CodeGenContext &c);

#endif
