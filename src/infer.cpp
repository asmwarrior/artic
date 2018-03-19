#include <typeinfo>
#include <algorithm>

#include "infer.h"
#include "ast.h"

namespace artic {

void TypeInference::run(const ast::Program& program) {
    // Run fix-point iterations until convergence
    do {
        todo_ = false;
        program.infer(*this);
    } while (todo_);
}

const Type* TypeInference::unify(const Loc& loc, const Type* a, const Type* b) {
    // Unifies types: returns the principal type for two given types
    a = find(a);
    b = find(b);

    // Constrain unknowns
    auto unknown_a = a->isa<UnknownType>();
    auto unknown_b = b->isa<UnknownType>();
    if (unknown_a) return join(loc, unknown_a, b);
    if (unknown_b) return join(loc, unknown_b, a);

    // Polymorphic types
    auto poly_a = a->isa<PolyType>();
    auto poly_b = b->isa<PolyType>();
    if (poly_a && poly_b && poly_a->num_vars == poly_b->num_vars) {
        return type_table_.poly_type(poly_a->num_vars, unify(loc, poly_a->body, poly_b->body));
    }
    if (poly_a) return type_table_.poly_type(poly_a->num_vars, unify(loc, poly_a->body, b));
    if (poly_b) return type_table_.poly_type(poly_b->num_vars, unify(loc, poly_b->body, a));

    // Unification for type constructors
    auto app_a = a->isa<TypeApp>();
    auto app_b = b->isa<TypeApp>();
    if (app_a && app_b && typeid(*app_a) == typeid(*app_b) && app_a->args.size() == app_b->args.size()) {
        if (app_a->name != app_b->name)
            return type_table_.infer_error(loc, a, b);

        auto n = app_a->args.size();
        std::vector<const Type*> args(n);
        for (size_t i = 0; i < n; i++) args[i] = unify(loc, app_a->args[i], app_b->args[i]);
        return app_a->rebuild(type_table_, std::move(args));
    }

    if (a != b)
        return type_table_.infer_error(loc, a, b);
    return a;
}

const Type* TypeInference::join(const Loc& loc, const UnknownType* unknown_a, const Type* b) {
    // Adds a type equation that maps type 'from' to type 'to'
    if (!unknown_a || unknown_a == b) return b;
    assert(find(b) != find(unknown_a));

    b->update_rank(unknown_a->rank);
    eqs_.emplace(unknown_a, Equation(loc, b));

    if (auto unknown_b = b->isa<UnknownType>()) {
        unknown_b->traits.insert(unknown_a->traits.begin(), unknown_a->traits.end());
    } else {
        // TODO: Make sure the traits associated with unknown_a are also valid for b
    }

    todo_ = true;
    return b;
}

const Type* TypeInference::find(const Type* type) {
    // Propagates the type equations to find the most
    // precise type corresponding to the given type
    auto it = eqs_.find(type);
    if (it != eqs_.end()) {
        auto next = find(it->second.type);
        todo_ |= next != it->second.type;
        it->second = Equation(it->second.loc, next);
        return next;
    }
    return type;
}

const Type* TypeInference::generalize(const Loc& loc, const Type* type, uint32_t rank) {
    // Get the best estimate of the type at this point
    type = unify(loc, type, type);

    // Generalizes the given type by transforming unknowns
    // into the type variables of a polymorphic type
    int vars = 0;
    for (auto u : type->all<UnknownType>()) {
        u = find(u)->isa<UnknownType>();
        // If the type is not tied to a concrete type nor tied in a higher scope, generalize it
        if (u && u->rank >= rank)
            unify(loc, u, type_table_.type_var(vars++, TypeVar::Traits(u->traits)));
    }
    if (vars == 0) return type;
    assert(!type->isa<PolyType>());
    return type_table_.poly_type(vars, type);
}

const Type* TypeInference::subsume(const Loc& loc, const Type* type, std::vector<const Type*>& type_args) {
    // Get the best estimate of the type at this point
    type = unify(loc, type, type);

    if (auto poly = type->isa<artic::PolyType>()) {
        std::unordered_map<const artic::Type*, const artic::Type*> map;

        // Replaces the type variables in the given type by unknowns
        for (auto var : type->all<TypeVar>()) {
            const Type* arg = var->index < type_args.size()
                ? type_args[var->index]
                : type_table().unknown_type(UnknownType::max_rank(), UnknownType::Traits(var->traits));
            map.emplace(var, arg);
        }

        return poly->substitute(type_table(), map)->as<PolyType>()->body;
    }
    return type;
}

const Type* TypeInference::type(const ast::Node& node, uint32_t rank) {
    if (!node.type)
        node.type = type_table_.unknown_type(std::min(rank, node.rank));
    return find(node.type);
}

const Type* TypeInference::infer(const ast::Node& node, const Type* expected) {
    auto type = node.infer(*this);
    if (type) {
        type = expected ? unify(node.loc, type, expected) : type;
        node.type = node.type ? unify(node.loc, node.type, type) : type;
        return node.type;
    }
    return nullptr;
}

void TypeInference::infer_head(const ast::Decl& decl) {
    auto type = decl.infer_head(*this);
    if (type)
        decl.type = decl.type ? unify(decl.loc, type, decl.type) : type;
}

namespace ast {

template <typename FieldVector>
const artic::Type* infer_struct(TypeInference& ctx, const Loc& loc, const artic::StructType* struct_type, const FieldVector& fields, bool has_etc) {
    auto& members = struct_type->members(ctx.type_table());

    // Infer all fields
    for (size_t i = 0, n = has_etc ? fields.size() - 1 : fields.size(); i < n; i++) {
        auto& field = fields[i];
        auto it = std::find_if(members.begin(), members.end(), [&] (auto& member) {
            return member.first == field->id.name;
        });
        ctx.infer(*field, it != members.end() ? it->second : ctx.type_table().error_type(loc));
    }

    // Make sure all fields are set
    if (!has_etc) {
        auto not_set = std::find_if(members.begin(), members.end(), [&] (auto& member) {
            return std::find_if(fields.begin(), fields.end(), [&] (auto& field) {
                return member.first == field->id.name;
            }) == fields.end();
        });
        if (not_set != members.end())
            return ctx.type_table().error_type(loc);
    }

    return struct_type;
}

const artic::Type* Path::infer(TypeInference& ctx) const {
    // TODO: Follow the whole path
    auto& symbol = elems.back().symbol;
    if (!symbol || symbol->decls.empty()) return ctx.type_table().error_type(loc);

    auto decl = symbol->decls.front();
    assert(decl->type);

    type_args.resize(std::max(type_args.size(), args.size()));
    for (size_t i = 0; i < args.size(); i++) {
        type_args[i] = ctx.infer(*args[i]);
    }

    // Do not subsume twice, as subsumption introduces unknowns
    return type ? type : ctx.subsume(loc, decl->type, type_args);
}

const artic::Type* PrimType::infer(TypeInference& ctx) const {
    return ctx.type_table().prim_type(artic::PrimType::Tag(tag));
}

const artic::Type* TupleType::infer(TypeInference& ctx) const {
    std::vector<const artic::Type*> types(args.size());
    std::transform(args.begin(), args.end(), types.begin(), [&] (auto& arg) {
        return ctx.infer(*arg);
    });
    return ctx.type_table().tuple_type(std::move(types));
}

const artic::Type* FnType::infer(TypeInference& ctx) const {
    return ctx.type_table().fn_type(ctx.infer(*from), ctx.infer(*to));
}

const artic::Type* TypeApp::infer(TypeInference& ctx) const {
    return ctx.infer(path);
}

const artic::Type* ErrorType::infer(TypeInference& ctx) const {
    return ctx.type_table().error_type(loc);
}

const artic::Type* TypedExpr::infer(TypeInference& ctx) const {
    return ctx.infer(*expr, ctx.infer(*type));
}

const artic::Type* PathExpr::infer(TypeInference& ctx) const {
    return ctx.infer(path);
}

const artic::Type* LiteralExpr::infer(TypeInference& ctx) const {
    // TODO: Create a Num type for integers, Fract for floats
    return ctx.type(*this);
}

const artic::Type* FieldExpr::infer(TypeInference& ctx) const {
    return ctx.infer(*expr);
}

const artic::Type* StructExpr::infer(TypeInference& ctx) const {
    auto expr_type = ctx.infer(*expr, ctx.type(*this));
    if (auto struct_type = expr_type->isa<StructType>())
        return infer_struct(ctx, loc, struct_type, fields, false);
    return expr_type;
}

const artic::Type* TupleExpr::infer(TypeInference& ctx) const {
    std::vector<const artic::Type*> type_args;
    for (auto& arg : args) type_args.emplace_back(ctx.infer(*arg));
    return ctx.type_table().tuple_type(std::move(type_args));
}

const artic::Type* FnExpr::infer(TypeInference& ctx) const {
    auto param_type = ctx.infer(*param);
    auto body_type = ctx.infer(*body);
    return ctx.type_table().fn_type(param_type, body_type);
}

const artic::Type* BlockExpr::infer(TypeInference& ctx) const {
    for (auto& expr : exprs) {
        if (auto decl_expr = expr->isa<DeclExpr>())
            ctx.infer_head(*decl_expr->decl);
    }
    for (size_t i = 0, n = exprs.size(); i < n; i++)
        ctx.infer(*exprs[i], i != n - 1 ? ctx.type_table().unit_type() : nullptr);
    return exprs.empty() ? ctx.type_table().unit_type() : exprs.back()->type;
}

const artic::Type* DeclExpr::infer(TypeInference& ctx) const {
    ctx.infer(*decl);
    return ctx.type_table().unit_type();
}

const artic::Type* CallExpr::infer(TypeInference& ctx) const {
    auto arg_type = ctx.infer(*arg);
    auto ret_type = ctx.type(*this);
    ctx.infer(*callee, ctx.type_table().fn_type(arg_type, ret_type));
    return ret_type;
}

const artic::Type* IfExpr::infer(TypeInference& ctx) const {
    ctx.infer(*cond, ctx.type_table().prim_type(artic::PrimType::I1));

    if (if_false)
        return ctx.infer(*if_false, ctx.infer(*if_true));

    return ctx.infer(*if_true, ctx.type_table().unit_type());
}

const artic::Type* ErrorExpr::infer(TypeInference& ctx) const {
    return ctx.type_table().error_type(loc);
}

const artic::Type* TypedPtrn::infer(TypeInference& ctx) const {
    return ctx.infer(*ptrn, ctx.infer(*type));
}

const artic::Type* IdPtrn::infer(TypeInference& ctx) const {
    return ctx.infer(*decl);
}

const artic::Type* LiteralPtrn::infer(TypeInference& ctx) const {
    // TODO: Create a Num type for integers, Fract for floats
    return ctx.type(*this);
}

const artic::Type* FieldPtrn::infer(TypeInference& ctx) const {
    if (ptrn) return ctx.infer(*ptrn);
    return ctx.type(*this);
}

const artic::Type* StructPtrn::infer(TypeInference& ctx) const {
    auto expr_type = ctx.infer(path);
    if (auto struct_type = expr_type->isa<StructType>())
        return infer_struct(ctx, loc, struct_type, fields, !fields.empty() && fields.back()->is_etc());
    return expr_type;
}

const artic::Type* TuplePtrn::infer(TypeInference& ctx) const {
    std::vector<const artic::Type*> type_args;
    for (auto& arg : args) type_args.emplace_back(ctx.infer(*arg));
    return ctx.type_table().tuple_type(std::move(type_args));
}

const artic::Type* ErrorPtrn::infer(TypeInference& ctx) const {
    return ctx.type_table().error_type(loc);
}

const artic::Type* TypeParam::infer(TypeInference& ctx) const {
    TypeVar::Traits traits;
    for (auto& bound : bounds) {
        auto trait = ctx.infer(*bound)->isa<TraitType>();
        if (!trait)
            return ctx.type_table().error_type(loc);
        traits.emplace(trait);
    }
    return ctx.type_table().type_var(index, std::move(traits));
}

const artic::Type* TypeParamList::infer(TypeInference& ctx) const {
    for (auto& param : params) ctx.infer(*param);
    return params.empty() || type
        ? ctx.type(*this)
        : ctx.type_table().poly_type(params.size(), ctx.type_table().unknown_type(rank));
}

const artic::Type* PtrnDecl::infer(TypeInference& ctx) const {
    return ctx.type(*this);
}

const artic::Type* LetDecl::infer(TypeInference& ctx) const {
    return init ? ctx.infer(*ptrn, ctx.infer(*init)) : ctx.infer(*ptrn);
}

const artic::Type* FnDecl::infer_head(TypeInference& ctx) const {
    if (type_params)
        return ctx.infer(*type_params);
    return ctx.type(*this);
}

const artic::Type* FnDecl::infer(TypeInference& ctx) const {
    const artic::Type* init_type = nullptr;
    if (fn->body) {
        init_type = ctx.infer(*fn);
        // If a return type is present, unify it with the return type of the function
        if (auto fn_type = init_type->isa<artic::FnType>()) {
            if (ret_type) ctx.infer(*ret_type, fn_type->to());
        }
    } else {
        // The return type is mandatory here
        init_type = ctx.type_table().fn_type(
            ctx.infer(*fn->param),
            ret_type ? ctx.infer(*ret_type) : ctx.type_table().error_type(loc));
    }
    // Generate a polymorphic type
    return ctx.generalize(loc, init_type, rank);
}

const artic::Type* FieldDecl::infer(TypeInference& ctx) const {
    return ctx.infer(*type);
}

const artic::Type* StructDecl::infer_head(TypeInference& ctx) const {
    if (type_params) {
        auto poly_type = ctx.infer(*type_params);
        StructType::Args args;
        for (auto& param : type_params->params)
            args.emplace_back(param->type);
        return ctx.unify(loc, poly_type, ctx.type_table().struct_type(std::string(id.name), std::move(args), this));
    }
    return ctx.type_table().struct_type(std::string(id.name), StructType::Args(), this);
}

const artic::Type* StructDecl::infer(TypeInference& ctx) const {
    for (auto& field : fields)
        ctx.infer(*field);
    return ctx.type(*this);
}

const artic::Type* TraitDecl::infer_head(TypeInference& ctx) const {
    return ctx.type_table().trait_type(std::string(id.name));
}

const artic::Type* TraitDecl::infer(TypeInference& ctx) const {
    auto trait_type = type->as<TraitType>();
    for (auto& decl : decls)
        trait_type->members.emplace(decl->id.name, ctx.infer(*decl));
    return trait_type;
}

const artic::Type* ErrorDecl::infer(TypeInference& ctx) const {
    return ctx.type_table().error_type(loc);
}

const artic::Type* Program::infer(TypeInference& ctx) const {
    for (auto& decl : decls) ctx.infer_head(*decl);
    for (auto& decl : decls) {
        if (decl->isa<StructDecl>() || decl->isa<TraitDecl>())
            ctx.infer(*decl);
    }
    for (auto& decl : decls) ctx.infer(*decl);
    return nullptr;
}

} // namespace ast

} // namespace artic
