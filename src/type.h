#ifndef TYPE_H
#define TYPE_H

#include <unordered_set>
#include <unordered_map>
#include <functional>
#include <algorithm>
#include <vector>
#include <limits>

#include "cast.h"
#include "hash.h"
#include "box.h"
#include "loc.h"

namespace artic {

namespace log {
    struct Output;
}

namespace ast {
    class StructDecl;
    class TraitDecl;
    class ImplDecl;
}

class Printer;
class TypeTable;

/// Address space information for pointers/references.
struct AddrSpace {
    enum Locality : uint32_t {
        Generic,
        Private,
        Shared,
        Global
    };
    Locality locality;

    AddrSpace(Locality locality)
        : locality(locality)
    {}

    std::string to_string() const {
        switch (locality) {
            case Generic: return "";
            case Private: return "private";
            case Shared:  return "shared";
            case Global:  return "global";
            default:
                assert(false);
                return "";
        }
    }

    uint32_t hash() const {
        return uint32_t(locality);
    }

    bool operator == (const AddrSpace& other) const {
        return other.locality == locality;
    }

    bool operator < (const AddrSpace& other) const {
        return other.locality == Generic;
    }

    bool operator <= (const AddrSpace& other) const {
        return other.locality == locality ||
               other.locality == Generic;
    }
};

/// Base class for all types.
struct Type : public Cast<Type> {
    virtual ~Type() {}

    /// Returns true iff this type is a tuple.
    bool is_tuple() const;
    /// Returns the body of a polymorphic type, or the type itself if it is not polymorphic.
    const Type* inner() const;

    /// Returns true iff the type is nominally typed.
    bool is_nominal() const;

    /// Returns true if the type contains at least one type verifying the predicate.
    virtual bool has(const std::function<bool (const Type*)>& pred) const { return pred(this); };
    /// Returns the types contained in this type that verifies the predicate.
    virtual void all(std::unordered_set<const Type*>& set, const std::function<bool (const Type*)>& pred) const {
        if (pred(this))
            set.emplace(this);
    }
    /// Replaces type variables with the given set of arguments.
    virtual const Type* reduce(TypeTable&, size_t, const std::vector<const Type*>&) const { return this; }
    /// Shifts unbound type variables by the given amount.
    virtual const Type* shift(TypeTable&, size_t, int32_t) const { return this; }

    /// Returns true if the type contains a type of the given kind.
    template <typename T>
    bool has() const {
        return has([] (const Type* t) {
            return t->isa<T>();
        });
    }

    /// Extracts all types of the given kind from the type.
    template <typename T>
    std::vector<const T*> all() const {
        std::unordered_set<const Type*> set;
        all(set, [] (auto t) { return t->template isa<T>(); });
        std::vector<const T*> result(set.size());
        std::transform(set.begin(), set.end(), result.begin(), [] (auto t) {
            return t->template as<T>();
        });
        return result;
    }

    /// Computes a hash value for the type.
    virtual uint32_t hash() const = 0;
    /// Test for structural equality with another type.
    virtual bool equals(const Type*) const = 0;
    /// Prints the type with the given formatting parameters.
    virtual void print(Printer&) const = 0;

    /// Dumps the type on the console, for debugging purposes.
    void dump() const;
};

/// A trait (or type class). Such objects are not types, but represent a constraint on types.
struct Trait {
    typedef std::vector<const Type*> Args;

    Args args;
    const ast::TraitDecl* decl;

    mutable const class TraitSet* supers;

    Trait(Args&& args, const ast::TraitDecl* decl)
        : args(std::move(args)), decl(decl), supers(nullptr)
    {}

    /// Replaces type variables with the given set of arguments.
    const Trait* reduce(TypeTable&, size_t, const std::vector<const Type*>&) const;
    /// Shifts unbound type variables by the given amount.
    const Trait* shift(TypeTable&, size_t, int32_t) const;

    /// Rebuilds this trait with different type arguments.
    const Trait* rebuild(TypeTable&, Args&&) const;
    /// Returns true if this trait is a subtrait of another.
    bool subtrait(const Trait*) const;

    /// Computes a hash value for this trait.
    uint32_t hash() const;
    /// Test for structural equality with another trait.
    bool equals(const Trait*) const;
    /// Prints the trait with the given formatting parameters.
    void print(Printer&) const;
    /// Dumps the trait on the console, for debugging purposes.
    void dump() const;
};

/// A set of traits. Such sets are constructed by the TypeTable and hashed to speed up comparisons.
struct TraitSet {
    typedef std::unordered_set<const Trait*> Members;

    Members members;

    TraitSet(Members&& members)
        : members(std::move(members))
    {}

    /// Replaces type variables with the given set of arguments.
    const TraitSet* reduce(TypeTable&, size_t, const std::vector<const Type*>&) const;
    /// Shifts unbound type variables by the given amount.
    const TraitSet* shift(TypeTable&, size_t, int32_t) const;

    /// Computes a hash value for this trait set.
    uint32_t hash() const;
    /// Test for equality between two trait sets.
    bool equals(const TraitSet*) const;
    /// Prints the trait set with the given formatting parameters.
    void print(Printer&) const;
    /// Dumps the trait set on the console, for debugging purposes.
    void dump() const;
};

/// Base class for types made of multiple arguments.
struct CompoundType : public Type {
    typedef std::vector<const Type*> Args;

    Args args;

    CompoundType(Args&& args)
        : args(std::move(args))
    {}

    bool has(const std::function<bool (const Type*)>&) const override;
    void all(std::unordered_set<const Type*>&, const std::function<bool (const Type*)>&) const override;
    const Type* reduce(TypeTable&, size_t, const std::vector<const Type*>&) const override;
    const Type* shift(TypeTable&, size_t, int32_t) const override;

    /// Rebuilds this type with different arguments.
    virtual const CompoundType* rebuild(TypeTable& table, Args&& new_args) const = 0;
};

log::Output& operator << (log::Output&, const Type&);

/// Primitive type (integers/floats/...).
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
    bool equals(const Type*) const override;
    void print(Printer&) const override;
};

/// Type application (e.g. tuples, functions, ...).
struct TypeApp : public CompoundType {
    using CompoundType::Args;

    std::string name;

    /// Structural type constructor
    TypeApp(Args&& args)
        : CompoundType(std::move(args))
    {}

    /// Nominal type constructor
    TypeApp(std::string&& name, Args&& args)
        : CompoundType(std::move(args)), name(std::move(name))
    {}

    bool is_nominal() const;

    uint32_t hash() const override;
    bool equals(const Type*) const override;
};

/// Structure type.
struct StructType : public TypeApp {
    typedef std::unordered_map<std::string, const Type*> Members;
    using TypeApp::Args;

    const ast::StructDecl* decl;

    StructType(std::string&& name, Args&& args, const ast::StructDecl* decl)
        : TypeApp(std::move(name), std::move(args)), decl(decl)
    {}

    const CompoundType* rebuild(TypeTable&, Args&&) const override;
    void print(Printer&) const override;

    /// Lazily build the members of the structure.
    const Members& members(TypeTable&) const;

private:
    mutable Members members_;
};

/// The type of a trait implementation. Takes a trait and a the type of self as an argument.
struct ImplType : public Type {
    typedef std::unordered_map<std::string, const Type*> Members;

    mutable const ast::ImplDecl* impl;

    const Trait* trait;
    const Type*  self;

    ImplType(const Trait* trait, const Type* self)
        : trait(trait), self(self)
    {}

    const Type* reduce(TypeTable&, size_t, const std::vector<const Type*>&) const override;
    const Type* shift(TypeTable&, size_t, int32_t) const override;

    uint32_t hash() const override;
    bool equals(const Type*) const override;
    void print(Printer&) const override;

    /// Lazily build the members of the implementation (only based on the trait).
    const Members& members() const;

private:
    mutable Members members_;
};

/// Type of a tuple, made of the product of the types of its elements.
struct TupleType : public TypeApp {
    using TypeApp::Args;

    TupleType(Args&& args)
        : TypeApp(std::move(args))
    {}

    const CompoundType* rebuild(TypeTable&, Args&&) const override;

    void print(Printer&) const override;
};

/// Function type with domain and codomain types (multi-argument functions use tuple types for the domain).
struct FnType : public TypeApp {
    using TypeApp::Args;

    FnType(const Type* from, const Type* to)
        : TypeApp({ from, to })
    {}

    const Type* from() const { return args[0]; }
    const Type* to() const { return args[1]; }

    const Type* first_arg() const;
    size_t num_args() const;

    const CompoundType* rebuild(TypeTable&, Args&&) const override;

    void print(Printer&) const override;
};

/// Base class for types that represent an address.
struct AddrType : public CompoundType {
    AddrSpace addr_space;
    bool mut;

    AddrType(const Type* pointee, AddrSpace addr_space, bool mut)
        : CompoundType({ pointee }), addr_space(addr_space), mut(mut)
    {}

    const Type* pointee() const { return args[0]; }

    uint32_t hash() const override;
    bool equals(const Type*) const override;
};

/// Reference type. Used to keep track of address spaces and mutability.
struct RefType : public AddrType {
    RefType(const Type* pointee, AddrSpace addr_space, bool mut)
        : AddrType(pointee, addr_space, mut)
    {}

    const CompoundType* rebuild(TypeTable&, Args&&) const override;

    void print(Printer&) const override;
};

/// Pointer type.
struct PtrType : public AddrType {
    PtrType(const Type* pointee, AddrSpace addr_space, bool mut)
        : AddrType(pointee, addr_space, mut)
    {}

    const CompoundType* rebuild(TypeTable&, Args&&) const override;

    void print(Printer&) const override;
};

/// Type variable, identifiable by its DeBruijn index.
struct TypeVar : public Type {
    /// DeBruijn index of the type variable.
    uint32_t index;

    TypeVar(uint32_t index)
        : index(index)
    {}

    const Type* reduce(TypeTable&, size_t, const std::vector<const Type*>&) const override;
    const Type* shift(TypeTable&, size_t, int32_t) const override;

    uint32_t hash() const override;
    bool equals(const Type*) const override;
    void print(Printer&) const override;
};

/// Unknown type in a set of type equations.
struct UnknownType : public Type {
    /// Number that will be displayed when printing this type.
    uint32_t number;
    /// Set of traits attached to this unknown. Can be modified during type inference.
    mutable const TraitSet* traits;

    UnknownType(uint32_t number, const TraitSet* traits)
        : number(number), traits(traits)
    {}

    uint32_t hash() const override;
    bool equals(const Type*) const override;
    void print(Printer&) const override;
};

/// Polymorphic type with possibly several variables and a set of constraints.
struct PolyType : public CompoundType {
    typedef std::vector<const TraitSet*> VarTraits;

    /// Number of type variables in this polymorphic type.
    size_t num_vars;
    /// Traits associated with each type variable.
    VarTraits var_traits;

    PolyType(size_t num_vars, const Type* body, VarTraits&& var_traits)
        : CompoundType({ body }), num_vars(num_vars), var_traits(std::move(var_traits))
    {}

    const Type* body() const { return args[0]; }

    const Type* reduce(TypeTable&, size_t, const std::vector<const Type*>&) const override;
    const Type* shift(TypeTable&, size_t, int32_t) const override;

    const CompoundType* rebuild(TypeTable&, Args&&) const override;

    uint32_t hash() const override;
    bool equals(const Type*) const override;
    void print(Printer&) const override;
};

/// Base class for type errors.
struct ErrorType : public Type {
    Loc loc;

    ErrorType(const Loc& loc) : loc(loc) {}

    void print(Printer&) const override;
    uint32_t hash() const override;
    bool equals(const Type*) const override;
};

/// Error generated during type inference.
struct InferError : public ErrorType {
    const Type* left;
    const Type* right;

    InferError(const Loc& loc, const Type* left, const Type* right)
        : ErrorType(loc), left(left), right(right)
    {}

    bool has(const std::function<bool (const Type*)>& pred) const override;
    void all(std::unordered_set<const Type*>& set, const std::function<bool (const Type*)>& pred) const override;

    uint32_t hash() const override;
    bool equals(const Type*) const override;
};

/// Table containing all types. Types are hashed so that comparison of types can be done with pointer equality.
class TypeTable {
private:
    template <typename T>
    struct Hash {
        size_t operator() (const T* t) const {
            return size_t(t->hash());
        }
    };

    template <typename T>
    struct Cmp {
        bool operator () (const T* a, const T* b) const {
            return a->equals(b);
        }
    };

    typedef std::unordered_set<const Type*,     Hash<Type>,     Cmp<Type>>     Types;
    typedef std::unordered_set<const TraitSet*, Hash<TraitSet>, Cmp<TraitSet>> TraitSets;
    typedef std::vector<const UnknownType*> Unknowns;

public:
    TypeTable() : unknowns_(0) {}
    TypeTable(const TypeTable&) = delete;

    ~TypeTable() {
        for (auto t : trait_sets_)   delete t;
        for (auto t : types_)    delete t;
        for (auto u : unknowns_) delete u;
    }

    const PrimType*     prim_type(PrimType::Tag);
    const StructType*   struct_type(std::string&&, StructType::Args&&, const ast::StructDecl*);
    const ImplType*    impl_type(const Trait*, const Type*);
    const TupleType*    tuple_type(TupleType::Args&&);
    const TupleType*    unit_type();
    const FnType*       fn_type(const Type*, const Type*);
    const RefType*      ref_type(const Type*, AddrSpace, bool);
    const PtrType*      ptr_type(const Type*, AddrSpace, bool);
    const Type*         poly_type(size_t, const Type*, PolyType::VarTraits&&);
    const TypeVar*      type_var(uint32_t);
    const ErrorType*    error_type(const Loc&);
    const InferError*   infer_error(const Loc&, const Type*, const Type*);
    const UnknownType*  unknown_type(const TraitSet*);

    const Trait* trait(Trait::Args&&, const ast::TraitDecl*);

    const TraitSet* trait_set(TraitSet::Members&&);
    const TraitSet* trait_set(const TraitSet* a, const TraitSet* b) {
        TraitSet::Members u;
        u.insert(a->members.begin(), a->members.end());
        u.insert(b->members.begin(), b->members.end());
        return trait_set(std::move(u));
    }

    const TraitSets& trait_sets() const { return trait_sets_; }
    const Types& types() const { return types_; }
    const Unknowns& unknowns() const { return unknowns_; }

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

    Types     types_;
    TraitSets trait_sets_;
    Unknowns  unknowns_;
};

} // namespace artic

#endif // TYPE_H
