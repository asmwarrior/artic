#include "check.h"
#include "log.h"

namespace artic {

bool TypeChecker::run(const ast::Program& program) {
    program.check(*this);
    return errors() == 0;
}

void TypeChecker::expect(const std::string& where, const Ptr<ast::Expr>& expr, const artic::Type* type) {
    if (expr->type != type)
        log::error(expr->loc, "type mismatch in {}, got '{}'", where, *expr->type);
}

namespace ast {

void Path::check(TypeChecker&) const {}

void PrimType::check(TypeChecker&) const {}

void TupleType::check(TypeChecker& ctx) const {
    for (auto& arg : args) arg->check(ctx);
}

void FnType::check(TypeChecker& ctx) const {
    from->check(ctx);
    to->check(ctx);
}

void ErrorType::check(TypeChecker&) const {}

void TypedExpr::check(TypeChecker& ctx) const {
    expr->check(ctx);
    type->check(ctx);
}

void PathExpr::check(TypeChecker& ctx) const {
    path.check(ctx);
}

void LiteralExpr::check(TypeChecker&) const {}

void TupleExpr::check(TypeChecker& ctx) const {
    for (auto& arg : args) arg->check(ctx);
}

void FnExpr::check(TypeChecker& ctx) const {
    param->check(ctx);
    body->check(ctx);
}

void BlockExpr::check(TypeChecker& ctx) const {
    for (auto& expr : exprs) expr->check(ctx);
}

void DeclExpr::check(TypeChecker& ctx) const {
    decl->check(ctx);
}

void CallExpr::check(TypeChecker& ctx) const {
    auto fn_type = callee->type->isa<artic::FnType>();
    if (!fn_type) {
        log::error(loc, "callee '{}' is not a function", *callee);
        return;
    }

    if (arg->type != fn_type->from()) {
        ctx.expect("function call", arg, callee->type->as<artic::FnType>()->from());
        return;
    }

    callee->check(ctx);
    arg->check(ctx);
}

void IfExpr::check(TypeChecker& ctx) const {
    cond->check(ctx);
    if_true->check(ctx);
    if (if_false) if_false->check(ctx);
}

void UnaryExpr::check(TypeChecker& ctx) const {
    expr->check(ctx);
}

void BinaryExpr::check(TypeChecker& ctx) const {
    left->check(ctx);
    right->check(ctx);
}

void ErrorExpr::check(TypeChecker&) const {}

void TypedPtrn::check(TypeChecker& ctx) const {
    ptrn->check(ctx);
    type->check(ctx);
}

void IdPtrn::check(TypeChecker& ctx) const {
    decl->check(ctx);
}

void LiteralPtrn::check(TypeChecker&) const {}

void TuplePtrn::check(TypeChecker& ctx) const {
    for (auto& arg : args) arg->check(ctx);
}

void ErrorPtrn::check(TypeChecker&) const {}

void PtrnDecl::check(TypeChecker&) const {
    if (type->has_variables())
        log::error(loc, "cannot infer type for '{}'", id.name);
}

void LocalDecl::check(TypeChecker& ctx) const {
    if (init) {
        ctx.expect("variable declaration", init, ptrn->type);
        init->check(ctx);
    }
    ptrn->check(ctx);
}

void ErrorDecl::check(TypeChecker&) const {}

void Program::check(TypeChecker& ctx) const {
    for (auto& decl : decls) decl->check(ctx);
}

} // namespace ast

} // namespace artic
