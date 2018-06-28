#include <typeinfo>
#include <iostream>

#include "type.h"
#include "ast.h"

namespace artic {

bool Type::is_tuple() const {
    return isa<TupleType>();
}

const Type* Type::inner() const {
    return isa<PolyType>() ? as<PolyType>()->body() : this;
}

bool Type::is_nominal() const {
    return isa<TypeApp>() && as<TypeApp>()->is_nominal();
}

bool TypeApp::is_nominal() const {
    return name != "";
}

const Type* FnType::first_arg() const {
    if (auto tuple_type = from()->isa<TupleType>()) {
        if (!tuple_type->args.empty()) return tuple_type->args[0];
    }
    return from();
}

size_t FnType::num_args() const {
    if (auto tuple_type = from()->isa<TupleType>()) {
        return tuple_type->args.size();
    }
    return 1;
}

bool TraitType::subtrait(const TraitType* super) const {
    if (super == this || supers.count(super))
        return true;
    return std::any_of(supers.begin(), supers.end(), [=] (auto trait) {
        return trait->subtrait(super);
    });
}

// Members -------------------------------------------------------------------------

const StructType::Members& StructType::members(TypeTable& table) const {
    if (members_.empty()) {
        assert(decl->type);
        assert(!decl->type_params || decl->type_params->params.size() == args.size());
        assert(decl->type_params || args.empty());
        for (auto& field : decl->fields) {
            assert(field->Node::type);
            members_.emplace(field->id.name, field->Node::type->reduce(table, args.size(), args));
        }
    }
    return members_;
}

const TraitType::Members& TraitType::members() const {
    if (members_.empty()) {
        assert(decl->type);
        for (auto& decl : decl->decls) {
            assert(decl->type);
            members_.emplace(decl->id.name, decl->type);
        }
    }
    return members_;
}

// Hash ----------------------------------------------------------------------------

uint32_t PrimType::hash() const {
    return uint32_t(tag);
}

uint32_t TypeApp::hash() const {
    return hash_combine(
        typeid(*this).hash_code(),
        hash_string(name),
        hash_list(args, [] (auto& arg) { return arg->hash(); })
    );
}

uint32_t PtrType::hash() const {
    return hash_combine(
        typeid(*this).hash_code(),
        pointee()->hash(),
        addr_space.hash(),
        uint32_t(mut)
    );
}

uint32_t PolyType::hash() const {
    return hash_combine(body()->hash(), uint32_t(num_vars));
}

uint32_t TypeVar::hash() const {
    return hash_combine(hash_init(), index,
        hash_list(traits, [] (auto& t) {
            return hash_string(t->name);
        })
    );
}

uint32_t UnknownType::hash() const {
    return hash_combine(hash_init(), number);
}

uint32_t ErrorType::hash() const {
    return loc.hash();
}

uint32_t InferError::hash() const {
    return hash_combine(loc.hash(), left->hash(), right->hash());
}

// Equals ----------------------------------------------------------------------------

bool PrimType::equals(const Type* t) const {
    return t->isa<PrimType>() && t->as<PrimType>()->tag == tag;
}

bool TypeApp::equals(const Type* t) const {
    if (auto app = t->isa<TypeApp>()) {
        // Check that the types have the same name if they are nominally typed
        if (typeid(*this) != typeid(*t) || name != app->name)
            return false;

        // Check that the arguments are the same
        size_t n = app->args.size();
        if (n != args.size()) return false;
        for (size_t i = 0; i < n; i++) {
            if (args[i] != app->args[i]) return false;
        }
        return true;
    }
    return false;
}

bool PtrType::equals(const Type* t) const {
    return typeid(*this) == typeid(*t) &&
           t->as<PtrType>()->pointee() == pointee() &&
           t->as<PtrType>()->addr_space == addr_space &&
           t->as<PtrType>()->mut == mut;
}

bool PolyType::equals(const Type* t) const {
    if (auto poly = t->isa<PolyType>()) {
        return poly->body() == body() &&
               poly->num_vars == num_vars;
    }
    return false;
}

bool TypeVar::equals(const Type* t) const {
    return t->isa<TypeVar>() &&
           t->as<TypeVar>()->index == index &&
           t->as<TypeVar>()->traits == traits;
}

bool UnknownType::equals(const Type* t) const {
    return t == this;
}

bool ErrorType::equals(const Type*) const {
    return false;
}

bool InferError::equals(const Type* t) const {
    return t->isa<InferError>() &&
           t->as<InferError>()->loc == loc &&
           t->as<InferError>()->left == left &&
           t->as<InferError>()->right == right;
}

// Rebuild -------------------------------------------------------------------------

const CompoundType* StructType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.struct_type(std::string(name), std::move(new_args), decl);
}

const CompoundType* TraitType::rebuild(TypeTable& table, Args&&) const {
    return table.trait_type(std::string(name), decl);
}

const CompoundType* TupleType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.tuple_type(std::move(new_args));
}

const CompoundType* FnType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.fn_type(new_args[0], new_args[1]);
}

const CompoundType* PtrType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.ptr_type(new_args[0], addr_space, mut);
}

const CompoundType* PolyType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.poly_type(num_vars, new_args[0])->as<PolyType>();
}

// All -----------------------------------------------------------------------------

void CompoundType::all(std::unordered_set<const Type*>& set, const std::function<bool (const Type*)>& pred) const {
    if (pred(this)) set.emplace(this);
    for (auto arg : args)
        arg->all(set, pred);
}

void InferError::all(std::unordered_set<const Type*>& set, const std::function<bool (const Type*)>& pred) const {
    if (pred(this)) set.emplace(this);
    left->all(set, pred);
    right->all(set, pred);
}

// Has -----------------------------------------------------------------------------

bool CompoundType::has(const std::function<bool (const Type*)>& pred) const {
    if (pred(this)) return true;
    for (auto arg : args) {
        if (arg->has(pred)) return true;
    }
    return false;
}

bool InferError::has(const std::function<bool (const Type*)>& pred) const {
    return pred(this) || left->has(pred) || right->has(pred);
}

// Reduce --------------------------------------------------------------------------

const Type* CompoundType::reduce(TypeTable& table, size_t depth, const std::vector<const Type*>& types) const {
    Args new_args(args.size());
    std::transform(args.begin(), args.end(), new_args.begin(), [&] (auto arg) {
        return arg->reduce(table, depth, types);
    });
    return rebuild(table, std::move(new_args));
}

const Type* TypeVar::reduce(TypeTable& table, size_t depth, const std::vector<const Type*>& types) const {
    if (depth > index + types.size() || depth <= index)
        return this;
    return types[depth - index - 1]->shift(table, 0, depth);
}

const Type* PolyType::reduce(TypeTable& table, size_t depth, const std::vector<const Type*>& types) const {
    if (depth == 0) {
        assert(types.size() <= num_vars);
        auto poly = table.poly_type(num_vars - types.size(), body()->reduce(table, depth + num_vars, types));
        return poly->shift(table, depth, -types.size());
    } else {
        return table.poly_type(num_vars, body()->reduce(table, depth + num_vars, types));
    }
}

// Shift ---------------------------------------------------------------------------

const Type* CompoundType::shift(TypeTable& table, size_t depth, int32_t n) const {
    Args new_args(args.size());
    std::transform(args.begin(), args.end(), new_args.begin(), [&] (auto arg) {
        return arg->shift(table, depth, n);
    });
    return rebuild(table, std::move(new_args));
}

const Type* TypeVar::shift(TypeTable& table, size_t depth, int32_t n) const {
    assert(index < depth || int32_t(index) + n >= 0);
    return index >= depth ? table.type_var(index + n, Traits(traits)) : this;
}

const Type* PolyType::shift(TypeTable& table, size_t depth, int32_t n) const {
    return table.poly_type(num_vars, body()->shift(table, depth + num_vars, n));
}

// Type table ----------------------------------------------------------------------

const PrimType* TypeTable::prim_type(PrimType::Tag tag) {
    return new_type<PrimType>(tag);
}

const StructType* TypeTable::struct_type(std::string&& name, StructType::Args&& args, const ast::StructDecl* decl) {
    return new_type<StructType>(std::move(name), std::move(args), decl);
}

const TraitType* TypeTable::trait_type(std::string&& name, const ast::TraitDecl* decl) {
    return new_type<TraitType>(std::move(name), decl);
}

const TupleType* TypeTable::tuple_type(TupleType::Args&& args) {
    return new_type<TupleType>(std::move(args));
}

const TupleType* TypeTable::unit_type() {
    return new_type<TupleType>(std::vector<const Type*>{});
}

const FnType* TypeTable::fn_type(const Type* from, const Type* to) {
    return new_type<FnType>(from, to);
}

const PtrType* TypeTable::ptr_type(const Type* pointee, AddrSpace addr_space, bool mut) {
    return new_type<PtrType>(pointee, addr_space, mut);
}

const Type* TypeTable::poly_type(size_t num_vars, const Type* body, PolyType::VarTraits&& var_traits) {
    if (num_vars == 0)
        return body;
    var_traits.resize(num_vars);
    if (auto poly_type = body->isa<PolyType>()) {
        var_traits.insert(var_traits.end(), poly_type->var_traits.begin(), poly_type->var_traits.end());
        return new_type<PolyType>(num_vars + poly_type->num_vars, poly_type->body(), std::move(var_traits));
    }
    return new_type<PolyType>(num_vars, body, std::move(var_traits));
}

const TypeVar* TypeTable::type_var(uint32_t index, TypeVar::Traits&& traits) {
    return new_type<TypeVar>(index, std::move(traits));
}

const ErrorType* TypeTable::error_type(const Loc& loc) {
    return new_type<ErrorType>(loc);
}

const InferError* TypeTable::infer_error(const Loc& loc, const Type* left, const Type* right) {
    return new_type<InferError>(loc, left, right);
}

const UnknownType* TypeTable::unknown_type(UnknownType::Traits&& traits) {
    unknowns_.emplace_back(new UnknownType(unknowns_.size(), std::move(traits)));
    return unknowns_.back();
}

} // namespace artic
