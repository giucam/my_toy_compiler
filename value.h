
#ifndef VALUE_H
#define VALUE_H

#include <string>
#include <vector>
#include <memory>

namespace llvm {
    class Value;
    class Type;
}

class CodeGenContext;
class StructInfo;
class TupleInfo;

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
        llvm::Type *type;

        llvm::Value *load(CodeGenContext &ctx) const;
    };

private:
    struct IfaceBase
    {
        virtual ~IfaceBase() {}

        virtual const std::vector<Value::V> &unpack() const = 0;
        virtual llvm::Value *extract(int id) const = 0;
        virtual llvm::Value *extract(const std::string &name) const = 0;

        virtual std::shared_ptr<const IfaceBase> clone(std::vector<V> &values) const = 0;
    };
    template<class T>
    struct Iface : IfaceBase
    {
        Iface(T h) : handler(std::move(h)) {}
        const std::vector<Value::V> &unpack() const override { return handler.unpack(); }
        llvm::Value *extract(int id) const override { return handler.extract(id); }
        llvm::Value *extract(const std::string &name) const override { return handler.extract(name); }
        std::shared_ptr<const IfaceBase> clone(std::vector<V> &values) const override { auto clone = handler.clone(values); return std::make_shared<Iface<decltype(clone)>>(clone); }
        T handler;
    };

public:
    Value() {}
    template<class T>
    Value(T handler)
        : m_iface(std::make_shared<Iface<T>>(std::move(handler))) {}
    Value(const std::shared_ptr<const IfaceBase> &iface)
        : m_iface(iface) {}

    inline const std::vector<V> &unpack() const { return m_iface->unpack(); }
    inline llvm::Value *extract(int id) const { return m_iface->extract(id); }
    inline llvm::Value *extract(const std::string &name) const { return m_iface->extract(name); }

    Value clone(std::vector<V> &values) const { return Value(m_iface->clone(values)); }

    std::shared_ptr<const IfaceBase> m_iface;
};

Value simpleValue(llvm::Value *val, llvm::Type *t = nullptr);
Value valuePack(std::vector<Value::V> &vec);
Value structValue(llvm::Value *alloc, llvm::Type *type, const StructInfo *i, CodeGenContext &c);
Value tupleValue(llvm::Value *alloc, llvm::Type *type, const TupleInfo *i, CodeGenContext &c);

#endif
