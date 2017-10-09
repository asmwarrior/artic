#include <algorithm>
#include <typeinfo>
#include <iostream>
#include "type.h"

namespace artic {

bool Type::is_tuple() const {
    return isa<TupleType>();
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

// Hash ----------------------------------------------------------------------------

uint32_t PrimType::hash() const {
    return uint32_t(tag);
}

uint32_t TupleType::hash() const {
    return hash_list(args, [] (auto& arg) { return arg->hash(); });
}

uint32_t FnType::hash() const {
    return hash_combine(from()->hash(), to()->hash());
}

uint32_t TypeVar::hash() const {
    return hash_combine(hash_init(), uint32_t(id));
}

uint32_t ErrorType::hash() const {
    return loc.hash();
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

bool TypeVar::equals(const Type* t) const {
    return t == this;
}

bool ErrorType::equals(const Type*) const {
    return false;
}

// Rebuild -------------------------------------------------------------------------

const TypeApp* TupleType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.tuple_type(std::move(new_args));
}

const TypeApp* FnType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.fn_type(new_args[0], new_args[1]);
}

// Variables -----------------------------------------------------------------------

void TypeApp::variables(std::unordered_set<const TypeVar*>& v) const {
    for (auto arg : args) arg->variables(v);
}

void TypeVar::variables(std::unordered_set<const TypeVar*>& v) const {
    v.emplace(this);
}

// Has variables -------------------------------------------------------------------

bool TypeApp::has_variables() const {
    for (auto arg : args) {
        if (arg->has_variables()) return true;
    }
    return false;
}

bool TypeVar::has_variables() const {
    return true;
}

// Has errors ----------------------------------------------------------------------

bool TypeApp::has_errors() const {
    for (auto arg : args) {
        if (arg->has_errors()) return true;
    }
    return false;
}

bool ErrorType::has_errors() const {
    return true;
}

// Substitute ----------------------------------------------------------------------

inline const Type* apply_map(const std::unordered_map<const Type*, const Type*>& map, const Type* type) {
    auto it = map.find(type);
    if (it != map.end()) return it->second;
    return type;
}

const Type* TypeApp::substitute(TypeTable& table, const std::unordered_map<const Type*, const Type*>& map) const {
    Args new_args(args.size());
    std::transform(args.begin(), args.end(), new_args.begin(), [&] (auto arg) {
        return apply_map(map, arg->substitute(table, map));
    });
    return rebuild(table, std::move(new_args));
}

// Type table ----------------------------------------------------------------------

const PrimType* TypeTable::prim_type(PrimType::Tag tag) {
    return new_type<PrimType>(tag);
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

const TypeVar* TypeTable::type_var() {
    return new_type<TypeVar>(++tid_);
}

const ErrorType* TypeTable::error_type(const Loc& loc) {
    return new_type<ErrorType>(loc);
}

} // namespace artic
