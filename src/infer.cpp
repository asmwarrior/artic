#include <typeinfo>
#include <algorithm>
#include "infer.h"
#include "ast.h"
#include "log.h"

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
    auto var_a = a->isa<TypeVar>();
    auto var_b = b->isa<TypeVar>();
    if (var_a) return join(loc, var_a, b);
    if (var_b) return join(loc, var_b, a);

    // Handle expansions
    auto exp_a = a->isa<ExpVar>();
    auto exp_b = b->isa<ExpVar>();
    if (exp_a && exp_b) return join(loc, exp_a, exp_b);
    if (exp_a || exp_b) {
        auto exp   = exp_a ? exp_a : exp_b;
        auto other = exp_a ? b : a;
        if (auto intr = other->isa<IntrType>()) {
            // Instanciate the expansion for every member of the intersection
            IntrType::Args insts;
            for (auto arg : intr->args)
                insts.insert(unify(loc, instanciate(exp), arg));
            return join(loc, exp, type_table_.intr_type(std::move(insts)));
        } else {
            // Instanciate only once
            return join(loc, exp, unify(loc, instanciate(exp), other));
        }
    }

    // Unification for type constructors
    auto app_a = a->isa<TypeApp>();
    auto app_b = b->isa<TypeApp>();
    if (app_a && app_b && typeid(*app_a) == typeid(*app_b) && app_a->args.size() == app_b->args.size()) {
        if (app_a->name != app_b->name) {
            log::error(loc, "incompatible nominal types '{}' and '{}'", a, b);
            return type_table_.error_type(loc);
        }

        auto n = app_a->args.size();
        std::vector<const Type*> args(n);
        for (size_t i = 0; i < n; i++) args[i] = unify(loc, app_a->args[i], app_b->args[i]);
        return app_a->rebuild(type_table_, std::move(args));
    }

    if (a != b) {
        log::error(loc, "cannot unify '{}' and '{}'", a, b);
        return type_table_.error_type(loc);
    }
    return a;
}

const Type* TypeInference::instanciate(const ExpVar* exp) {
    auto vars = exp->variables();
    TypeSubst subst;
    for (auto var : vars) subst.emplace(var, type_table_.type_var());
    return exp->arg->substitute(type_table_, subst);
}

const Type* TypeInference::join(const Loc& loc, const Type* a, const Type* b) {
    // Adds a type equation that maps type 'from' to type 'to'
    if (a == b) return b;
    assert(find(b) != find(a) && "Cycle detected in unification constraints");
    eqs_.emplace(a, Equation(loc, b));
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

const Type* TypeInference::type(const ast::Node& node) {
    if (!node.type)
        node.type = type_table_.type_var();
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

namespace ast {

const artic::Type* Path::infer(TypeInference& ctx) const {
    // TODO: Follow the whole path
    auto& symbol = elems.back().symbol;
    if (!symbol || symbol->decls.empty()) return ctx.type_table().error_type(loc);

    auto decl = symbol->decls.front();
    if (!decl->type) return ctx.type(*this);

    // TODO: Make sure the type symbol type is polymorphic and expects at most args.size() variables

    return decl->type;
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

const artic::Type* UnaryExpr::infer(TypeInference& ctx) const {
    // TODO: Use bounds on types here
    return ctx.infer(*expr);
}

const artic::Type* BinaryExpr::infer(TypeInference& ctx) const {
    // TODO: Use bounds on types here
    auto op_type = ctx.infer(*left, ctx.infer(*right));
    if (has_cmp()) return ctx.type_table().prim_type(artic::PrimType::I1);
    return op_type;
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

const artic::Type* TuplePtrn::infer(TypeInference& ctx) const {
    std::vector<const artic::Type*> type_args;
    for (auto& arg : args) type_args.emplace_back(ctx.infer(*arg));
    return ctx.type_table().tuple_type(std::move(type_args));
}

const artic::Type* ErrorPtrn::infer(TypeInference& ctx) const {
    return ctx.type_table().error_type(loc);
}

const artic::Type* PtrnDecl::infer(TypeInference& ctx) const {
    return ctx.type(*this);
}

const artic::Type* LocalDecl::infer(TypeInference& ctx) const {
    return init ? ctx.infer(*ptrn, ctx.infer(*init)) : ctx.infer(*ptrn);
}

const artic::Type* ErrorDecl::infer(TypeInference& ctx) const {
    return ctx.type_table().error_type(loc);
}

const artic::Type* Program::infer(TypeInference& ctx) const {
    for (auto& decl : decls) ctx.infer(*decl);
    return nullptr;
}

} // namespace ast

} // namespace artic
