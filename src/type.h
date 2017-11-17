#ifndef TYPE_H
#define TYPE_H

#include <unordered_set>
#include <unordered_map>
#include <vector>
#include <limits>

#include "cast.h"
#include "hash.h"
#include "box.h"
#include "loc.h"

namespace artic {

class Printer;
class TypeTable;
class TypeVar;
class Type;

typedef std::unordered_map<const Type*, const Type*> TypeSubst;
typedef std::unordered_set<const TypeVar*>           TypeVars;

/// Base class for all types.
struct Type : public Cast<Type> {
    virtual ~Type() {}

    /// Returns true iff this type is a tuple.
    bool is_tuple() const;

    /// Returns true iff the type is nominally typed.
    virtual bool is_nominal() const { return false; }

    /// Applies a substitution to the inner part of this type.
    virtual const Type* substitute(TypeTable&, const TypeSubst&) const { return this; }
    /// Fills the given set with variables contained in this type.
    virtual void variables(TypeVars&) const {}

    /// Returns true iff the type has unknowns.
    virtual bool has_variables() const { return false; }
    /// Returns true iff the type has errors.
    virtual bool has_errors() const { return false; }

    /// Returns the set of unknowns contained in this type.
    TypeVars variables() const {
        TypeVars set;
        variables(set);
        return set;
    }

    /// Returns a hash for the kind of this type.
    uint32_t kind_hash() const;
    
    /// Computes a hash value for the type.
    virtual uint32_t hash() const = 0;
    /// Test for structural equality with another type.
    virtual bool equals(const Type*) const = 0;
    /// Prints the type with the given formatting parameters.
    virtual void print(Printer&) const = 0;

    /// Dumps the type on the console, for debugging purposes.
    void dump() const;
};

std::ostream& operator << (std::ostream&, const Type&);

/// Primitive type (integers/floats/...)
struct PrimType : public Type {
    enum Tag {
#define TAG(t, n, ty) t = Box::t,
        PRIM_TAGS(TAG)
#undef TAG
    };

    Tag tag;

    PrimType(Tag tag)
        : tag(tag)
    {}

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
    void print(Printer&) const override;
};

/// Type application (e.g. tuples, functions, ...).
struct TypeApp : public Type {
    typedef std::vector<const Type*> Args;

    std::string name;
    Args args;

    /// Structural type constructor
    TypeApp(Args&& args)
        : args(std::move(args))
    {}

    /// Nominal type constructor
    TypeApp(std::string&& name, Args&& args)
        : name(std::move(name)), args(std::move(args))
    {}

    const Type* substitute(TypeTable& table, const TypeSubst& map) const override;
    void variables(TypeVars&) const override;

    bool has_variables() const override;
    bool has_errors() const override;

    bool is_nominal() const;

    /// Rebuilds this type with different arguments.
    virtual const TypeApp* rebuild(TypeTable& table, Args&& new_args) const = 0;

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
};

/// Type of a tuple, made of the product of the types of its elements.
struct TupleType : public TypeApp {
    using TypeApp::Args;

    TupleType(Args&& args)
        : TypeApp(std::move(args))
    {}

    const TypeApp* rebuild(TypeTable&, Args&&) const override;
    void print(Printer&) const override;
};

/// Function type with domain and codomain types (multi-argument functions use tuple types for the domain).
struct FnType : public TypeApp {
    using TypeApp::Args;

    FnType(const Type* from, const Type* to)
        : TypeApp(Args{from, to})
    {}

    const Type* from() const { return args[0]; }
    const Type* to() const { return args[1]; }

    const Type* first_arg() const;
    size_t num_args() const;

    const TypeApp* rebuild(TypeTable&, Args&&) const override;
    void print(Printer&) const override;
};

/// Intersection type.
struct IntrType : public Type {
    typedef std::unordered_set<const Type*> Args;
    Args args;

    IntrType(Args&& args)
        : args(std::move(args))
    {}

    const Type* substitute(TypeTable& table, const TypeSubst& map) const override;
    void variables(TypeVars&) const override;

    bool has_variables() const override;
    bool has_errors() const override;

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
    void print(Printer&) const override;
};

/// Type variable, identifiable by a unique index.
struct TypeVar : public Type {
    uint32_t id;

    TypeVar(uint32_t id) : id(id) {}

    void variables(TypeVars&) const override;
    bool has_variables() const override;

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
    void print(Printer&) const override;
};

/// Expansion variable.
/// See "System E: Expansion Variables for Flexible Typing with Linear and Non-linear Types and Intersection Types"
struct ExpVar : public Type {
    uint32_t id;
    const Type* arg;

    ExpVar(uint32_t id, const Type* arg) : id(id), arg(arg) {}

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
    void print(Printer&) const override;
};

/// Type that represents type-checking/parsing errors.
struct ErrorType : public Type {
    Loc loc;
    ErrorType(const Loc& loc) : loc(loc) {}

    bool has_errors() const override;

    void print(Printer&) const override;
    uint32_t hash() const override;
    bool equals(const Type* t) const override;
};

/// Table containing all types. Types are hashed so that comparison of types can be done with pointer equality.
class TypeTable {
private:
    struct HashType {
        size_t operator () (const Type* t) const {
            return size_t(t->hash());
        }
    };

    struct CmpType {
        bool operator () (const Type* a, const Type* b) const {
            return a->equals(b);
        }
    };

    typedef std::unordered_set<const Type*, HashType, CmpType> TypeSet;

public:
    TypeTable() : tid_(0) {}
    TypeTable(const TypeTable&) = delete;

    ~TypeTable() {
        for (auto& t : types_) delete t;
    }

    const PrimType*     prim_type(PrimType::Tag);
    const TupleType*    tuple_type(TupleType::Args&&);
    const TupleType*    unit_type();
    const FnType*       fn_type(const Type*, const Type*);
    const Type*         intr_type(IntrType::Args&&);
    const TypeVar*      type_var();
    const ExpVar*       exp_var(const Type*);
    const ErrorType*    error_type(const Loc&);

    const TypeSet& types() const { return types_; }

private:
    template <typename T, typename... Args>
    const T* new_type(Args... args) {
        T t(std::forward<Args>(args)...);
        auto it = types_.find(&t);
        if (it != types_.end()) {
            assert(t.equals(*it));
            return (*it)->template as<T>();
        }
        const T* ptr = new T(std::move(t));
        types_.emplace(ptr);
        return ptr;
    }

    TypeSet types_;
    uint32_t tid_;
};

} // namespace artic

#endif // TYPE_H
