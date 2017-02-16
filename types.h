
#ifndef TYPES_H
#define TYPES_H

#include <string>
#include <memory>
#include <vector>

#include "lexer.h"

namespace llvm {
    class Type;
    class StructType;
}

class Value;
class Allocator;
class StructInfo;
class CodeGenContext;

class TypeConstraint
{
public:
    enum class Operator {
        Equal,
        NotEqual,
        Greater,
        Lesser,
        GreaterEqual,
        LesserEqual,
    };
    TypeConstraint();
    TypeConstraint(Operator op, int v);

    bool isCompatibleWith(const TypeConstraint &c) const;
    std::string name() const;

    void addConstraint(Operator op, int v);
    void add(const TypeConstraint &c, const void *source);
    void addNegate(const TypeConstraint &c, const void *source);
    void addGreater(const TypeConstraint &c, const void *source);
    void removeFromSource(void *source);

    TypeConstraint operator/(const TypeConstraint &other) const;
    TypeConstraint operator*(const TypeConstraint &other) const;
    TypeConstraint operator+(const TypeConstraint &other) const;

private:
    struct Constraint {
        Operator op;
        int value;
        const void *source;
    };
    std::vector<Constraint> m_constraints;
};

class Type
{
public:
    Type() {}
    template<class T>
    Type(T handler);

    llvm::Type *get(CodeGenContext &ctx) const;
    std::string name() const;

    bool isValid() const { return m_iface.get(); }

    Type getPointerTo() const;

    template<class T>
    const T *getSpecialization() const;

    void setTypeConstraint(const TypeConstraint &c);
    const TypeConstraint &typeConstraint() const { return m_constraint; }
    TypeConstraint &typeConstraint() { return m_constraint; }

    Value create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Value &storeValue) const;
    Value create(CodeGenContext &ctx, Allocator *alloc, const std::string &name) const;

private:
    struct IfaceBase;
    template<class T> struct Iface;
    std::shared_ptr<const IfaceBase> m_iface;
    TypeConstraint m_constraint;
};
#define TYPE_SPECIALIZATION \
static void magic() { } \
friend class Type;

struct CreateError
{
    enum class Err {
        StoreError,
        TypeError,
    };
    Err error;
};

class IntegerType
{
    TYPE_SPECIALIZATION
public:
    IntegerType(bool sign, int bits) : m_signed(sign), m_bits(bits) {}

    llvm::Type *get(CodeGenContext &ctx) const;
    std::string name() const;

    long long maxValue() const;
    long long minValue() const;

    Value create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const;

private:
    bool m_signed;
    int m_bits;
};

class FloatingType
{
    TYPE_SPECIALIZATION
public:
    FloatingType(int bits) : m_bits(bits) {}

    llvm::Type *get(CodeGenContext &ctx) const;
    std::string name() const;

    Value create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const;

private:
    int m_bits;
};

class VoidType
{
    TYPE_SPECIALIZATION
public:
    VoidType() {}

    llvm::Type *get(CodeGenContext &ctx) const;
    std::string name() const;

    Value create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const;
};

class PointerType
{
    TYPE_SPECIALIZATION
public:
    PointerType(const Type t) : m_type(t) {}

    llvm::Type *get(CodeGenContext &ctx) const;
    std::string name() const;

    Value create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const;

    Type pointerElementType() const;

private:
    const Type m_type;
};

class FunctionPointerType
{
    TYPE_SPECIALIZATION
public:
    FunctionPointerType(Type ret, std::vector<Type> &args) : m_ret(ret) { std::swap(args, m_args); }

    llvm::Type *get(CodeGenContext &ctx) const;
    std::string name() const;

    Value create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const;

private:
    Type m_ret;
    std::vector<Type> m_args;
};

class ArrayType
{
    TYPE_SPECIALIZATION
public:
    ArrayType(Type t, int n) : m_type(t), m_num(n) {}

    llvm::Type *get(CodeGenContext &ctx) const;
    std::string name() const;

    Value create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const;

private:
    Type m_type;
    int m_num;
};

class ArgumentPackType
{
    TYPE_SPECIALIZATION
public:
    ArgumentPackType() {}

    llvm::Type *get(CodeGenContext &ctx) const;
    std::string name() const;

    Value create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const;
};

class TupleType
{
    TYPE_SPECIALIZATION
public:
    TupleType(std::vector<Type> &types) { std::swap(types, m_types); }

    const std::vector<Type> &unpack() const { return m_types; }

    llvm::Type *get(CodeGenContext &ctx) const;
    std::string name() const;

    Value create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const;

private:
    std::vector<Type> m_types;
};

class StructType
{
    TYPE_SPECIALIZATION
public:
    StructType(const std::string &name);
    StructType(llvm::StructType *type);

    llvm::Type *get(CodeGenContext &ctx) const;
    std::string name() const;

    Value create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const;

private:
    std::string m_name;
    mutable llvm::StructType *m_type;
};

class CustomType
{
    TYPE_SPECIALIZATION
public:
    CustomType(const Token &tok, const std::string &name) : m_token(tok), m_name(name) {}

    llvm::Type *get(CodeGenContext &ctx) const;
    std::string name() const;

    Value create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const;

private:
    Token m_token;
    std::string m_name;
};

Type llvmType(CodeGenContext &ctx, llvm::Type *t);


#include "value.h"

struct Type::IfaceBase {
    virtual ~IfaceBase() {}
    virtual llvm::Type *get(CodeGenContext &ctx) const = 0;
    virtual std::string name() const = 0;
    virtual Value create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const = 0;
    void (*magic)();
};
template<class T>
struct Type::Iface : Type::IfaceBase {
    Iface(T d) : data(std::move(d)) { magic = T::magic; }
    llvm::Type *get(CodeGenContext &ctx) const override { return data.get(ctx); }
    std::string name() const override { return data.name(); }
    Value create(CodeGenContext &ctx, Allocator *alloc, const std::string &name, const Type &type, const Value &storeValue) const override
    {
        return data.create(ctx, alloc, name, type, storeValue);
    }
    T data;
};

template<class T>
inline Type::Type(T handler)
    : m_iface(std::make_shared<Iface<T>>(std::move(handler))) {}

inline llvm::Type *Type::get(CodeGenContext &ctx) const { return m_iface->get(ctx); }

template<class T>
inline const T *Type::getSpecialization() const
{
    if (T::magic == m_iface->magic) {
        return &static_cast<const Iface<T> *>(m_iface.get())->data;
    }
    return nullptr;
}

#endif
