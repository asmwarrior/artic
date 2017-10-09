#include <algorithm>
#include "parser.h"
#include "print.h"

using namespace artic::ast;

namespace artic {

Parser::Parser(Lexer& lexer, TypeTable& type_table)
    : ahead_(Loc()), lexer_(lexer), type_table_(type_table)
{
    next();
}

Ptr<ast::Program> Parser::parse_program() {
    Tracker tracker(this);
    PtrVector<ast::Decl> decls;
    parse_list(Token::END, Token::SEMICOLON, [&] {
        if (ahead().tag() != Token::SEMICOLON)
            decls.emplace_back(parse_decl());
    });
    return make_ptr<ast::Program>(tracker(), std::move(decls));
}

// Declarations --------------------------------------------------------------------

Ptr<ast::Decl> Parser::parse_decl() {
    switch (ahead().tag()) {
        case Token::DEF:    return parse_def_decl();
        case Token::VAR:    return parse_var_decl();
        default:            return parse_error_decl();
    }
}

Ptr<ast::DefDecl> Parser::parse_def_decl() {
    Tracker tracker(this);
    eat(Token::DEF);

    auto ptrn = parse_ptrn();
    expect_binder("definition", ptrn);

    if (ptrn->isa<IdPtrn>() && ahead().tag() == Token::L_PAREN) {
        auto param = parse_tuple_ptrn();
        expect_binder("function parameter", param);
        auto body = parse_block_expr();
        auto fn = make_ptr<FnExpr>(tracker(), std::move(param), std::move(body));
        return make_ptr<DefDecl>(tracker(), std::move(ptrn), std::move(fn), true);
    }

    expect(Token::EQ);
    eat_nl();

    auto init = parse_expr();
    return make_ptr<DefDecl>(tracker(), std::move(ptrn), std::move(init));  
}

Ptr<ast::VarDecl> Parser::parse_var_decl() {
    Tracker tracker(this);
    eat(Token::VAR);
    auto ptrn = parse_ptrn();
    expect_binder("variable declaration", ptrn);
    Ptr<Expr> init;
    if (ahead().tag() == Token::EQ) {
        eat(Token::EQ);
        eat_nl();
        init = std::move(parse_expr());
    }
    return make_ptr<ast::VarDecl>(tracker(), std::move(ptrn), std::move(init));
}

Ptr<ast::ErrorDecl> Parser::parse_error_decl() {
    Tracker tracker(this);
    log::error(ahead().loc(), "expected declaration, got '{}'", ahead().string());
    next();
    return make_ptr<ast::ErrorDecl>(tracker());
}

// Patterns ------------------------------------------------------------------------

Ptr<ast::Ptrn> Parser::parse_ptrn() {
    Ptr<ast::Ptrn> ptrn;
    switch (ahead().tag()) {
        case Token::ID:      ptrn = std::move(parse_id_ptrn(parse_id())); break;
        case Token::L_PAREN: ptrn = std::move(parse_tuple_ptrn());        break;
        case Token::LIT:     ptrn = std::move(parse_literal_ptrn());      break;
        default:             ptrn = std::move(parse_error_ptrn());        break;
    }
    return parse_typed_ptrn(std::move(ptrn));
}

Ptr<ast::Ptrn> Parser::parse_typed_ptrn(Ptr<ast::Ptrn>&& ptrn) {
    if (ahead().tag() == Token::COLON) {
        Tracker tracker(this, ptrn->loc);
        eat(Token::COLON);
        auto type = parse_type();
        return make_ptr<ast::TypedPtrn>(tracker(), std::move(ptrn), std::move(type));
    }
    return std::move(ptrn);
}

Ptr<ast::IdPtrn> Parser::parse_id_ptrn(Identifier&& id) {
    Tracker tracker(this, id.loc);
    return make_ptr<ast::IdPtrn>(tracker(), make_ptr<ast::PtrnDecl>(tracker(), std::move(id)));
}

Ptr<ast::LiteralPtrn> Parser::parse_literal_ptrn() {
    Tracker tracker(this);
    auto lit = parse_lit();
    return make_ptr<ast::LiteralPtrn>(tracker(), lit);
}

Ptr<ast::Ptrn> Parser::parse_tuple_ptrn() {
    Tracker tracker(this);
    eat(Token::L_PAREN);
    PtrVector<Ptrn> args;
    parse_list(Token::R_PAREN, Token::COMMA, [&] {
        args.emplace_back(parse_ptrn());
    });
    return args.size() == 1
        ? std::move(args[0])
        : make_ptr<ast::TuplePtrn>(tracker(), std::move(args));
}

Ptr<ast::ErrorPtrn> Parser::parse_error_ptrn() {
    Tracker tracker(this);
    log::error(ahead().loc(), "expected pattern, got '{}'", ahead().string());
    next();
    return make_ptr<ast::ErrorPtrn>(tracker());
}

// Expressions ---------------------------------------------------------------------

Ptr<ast::Expr> Parser::parse_expr() {
    auto expr = parse_primary_expr();
    return parse_binary_expr(std::move(expr), BinaryExpr::max_precedence());
}

Ptr<ast::Expr> Parser::parse_typed_expr(Ptr<Expr>&& expr) {
    if (ahead().tag() == Token::COLON) {
        Tracker tracker(this, expr->loc);
        eat(Token::COLON);
        eat_nl();
        return make_ptr<ast::TypedExpr>(tracker(), std::move(expr), std::move(parse_type()));
    }
    return std::move(expr);
}

Ptr<ast::PathExpr> Parser::parse_path_expr() {
    Tracker tracker(this);
    auto path = parse_path(parse_id());
    return make_ptr<ast::PathExpr>(tracker(), std::move(path));
}

Ptr<ast::LiteralExpr> Parser::parse_literal_expr() {
    Tracker tracker(this);
    auto lit = parse_lit();
    return make_ptr<ast::LiteralExpr>(tracker(), lit);
}

Ptr<ast::Expr> Parser::parse_tuple_expr() {
    Tracker tracker(this);
    eat(Token::L_PAREN);
    eat_nl();
    PtrVector<ast::Expr> args;
    parse_list(Token::R_PAREN, Token::COMMA, [&] {
        eat_nl();
        args.emplace_back(parse_expr());
        eat_nl();
    });
    return args.size() == 1
        ? std::move(args[0])
        : make_ptr<ast::TupleExpr>(tracker(), std::move(args));
}

Ptr<ast::BlockExpr> Parser::parse_block_expr() {
    Tracker tracker(this);
    eat(Token::L_BRACE);
    PtrVector<ast::Expr> exprs;
    parse_list(Token::R_BRACE, Token::SEMICOLON, [&] {
        if (ahead().tag() != Token::SEMICOLON)
            exprs.emplace_back(parse_expr());
    });
    return make_ptr<ast::BlockExpr>(tracker(), std::move(exprs));
}

Ptr<ast::DeclExpr> Parser::parse_decl_expr() {
    Tracker tracker(this);
    auto decl = parse_decl();
    return make_ptr<ast::DeclExpr>(tracker(), std::move(decl));
}

Ptr<ast::FnExpr> Parser::parse_fn_expr(Ptr<Expr>&& param) {
    Tracker tracker(this);
    auto ptrn = expr_to_ptrn(std::move(param));
    expect_binder("anonymous function parameter", ptrn);
    eat(Token::ARROW);
    auto body = parse_expr();
    return make_ptr<ast::FnExpr>(tracker(), std::move(ptrn), std::move(body));
}

Ptr<ast::CallExpr> Parser::parse_call_expr(Ptr<Expr>&& callee) {
    Tracker tracker(this, callee->loc);
    auto args = parse_tuple_expr();
    return make_ptr<ast::CallExpr>(tracker(), std::move(callee), std::move(args));
}

Ptr<ast::IfExpr> Parser::parse_if_expr() {
    Tracker tracker(this);
    eat(Token::IF);
    expect(Token::L_PAREN);
    auto cond = parse_expr();
    expect(Token::R_PAREN);
    eat_nl();
    auto if_true = parse_expr();
    eat_nl();
    Ptr<ast::Expr> if_false;
    if (ahead().tag() == Token::ELSE) {
        eat(Token::ELSE);
        eat_nl();
        if_false = std::move(parse_expr());
    }
    return make_ptr<ast::IfExpr>(tracker(), std::move(cond), std::move(if_true), std::move(if_false));
}

Ptr<ast::Expr> Parser::parse_primary_expr() {
    Ptr<ast::Expr> expr;
    switch (ahead().tag()) {
        case Token::INC:
        case Token::DEC:
        case Token::ADD:
        case Token::SUB:
            expr = std::move(parse_prefix_expr());
            break;
        case Token::L_BRACE: expr = std::move(parse_block_expr());   break;
        case Token::L_PAREN: expr = std::move(parse_tuple_expr());   break;
        case Token::ID:      expr = std::move(parse_path_expr());    break;
        case Token::LIT:     expr = std::move(parse_literal_expr()); break;
        case Token::DEF:
        case Token::VAR:
            expr = std::move(parse_decl_expr());
            break;
        case Token::IF: expr = std::move(parse_if_expr()); break;
        default:
            expr = std::move(parse_error_expr()); break;
    }
    if (ahead().tag() == Token::INC || ahead().tag() == Token::DEC)
        expr = std::move(parse_postfix_expr(std::move(expr)));
    if (ahead().tag() == Token::ARROW)
        expr = std::move(parse_fn_expr(std::move(expr)));
    while (ahead().tag() == Token::L_PAREN)
        expr = std::move(parse_call_expr(std::move(expr)));
    return parse_typed_expr(std::move(expr));
}

Ptr<ast::UnaryExpr> Parser::parse_prefix_expr() {
    Tracker tracker(this);
    auto tag = ast::UnaryExpr::tag_from_token(ahead(), true);
    next();
    auto expr = parse_expr();
    return make_ptr<ast::UnaryExpr>(tracker(), tag, std::move(expr));
}

Ptr<ast::UnaryExpr> Parser::parse_postfix_expr(Ptr<ast::Expr>&& expr) {
    Tracker tracker(this, expr->loc);
    auto tag = ast::UnaryExpr::tag_from_token(ahead(), false);
    next();
    return make_ptr<ast::UnaryExpr>(tracker(), tag, std::move(expr));
}

Ptr<ast::Expr> Parser::parse_binary_expr(Ptr<ast::Expr>&& left, int max_prec) {
    while (true) {
        Tracker tracker(this, left->loc);

        auto tag = ast::BinaryExpr::tag_from_token(ahead());
        if (tag == ast::BinaryExpr::ERR) break;
        auto prec = ast::BinaryExpr::precedence(tag);
        if (prec > max_prec) break;
        next();

        eat_nl();
        auto right = parse_primary_expr();

        auto next_tag = ast::BinaryExpr::tag_from_token(ahead());
        if (next_tag != ast::BinaryExpr::ERR) {
            auto next_prec = ast::BinaryExpr::precedence(next_tag);
            if (next_prec < prec) right = std::move(parse_binary_expr(std::move(right), next_prec));
        }

        left = std::move(make_ptr<ast::BinaryExpr>(tracker(), tag, std::move(left), std::move(right)));
    }
    return std::move(left);
}

Ptr<ast::ErrorExpr> Parser::parse_error_expr() {
    Tracker tracker(this);
    log::error(ahead().loc(), "expected expression, got '{}'", ahead().string());
    next();
    return make_ptr<ast::ErrorExpr>(tracker());
}

// Types ---------------------------------------------------------------------------

Ptr<ast::Type> Parser::parse_type() {
    Ptr<ast::Type> type;
    switch (ahead().tag()) {
        case Token::ID:      type = std::move(parse_named_type()); break;
        case Token::L_PAREN: type = std::move(parse_tuple_type()); break;
        default:             type = std::move(parse_error_type()); break;
    }
    if (ahead().tag() == Token::ARROW)
        return parse_fn_type(std::move(type));
    return std::move(type);
}

Ptr<ast::Type> Parser::parse_named_type() {
    auto tag = ast::PrimType::tag_from_token(ahead());
    if (tag != ast::PrimType::ERR)
        return parse_prim_type(tag);
    return parse_error_type();
}

Ptr<ast::PrimType> Parser::parse_prim_type(ast::PrimType::Tag tag) {
    Tracker tracker(this);
    next();
    return make_ptr<ast::PrimType>(tracker(), tag);
}

Ptr<ast::TupleType> Parser::parse_tuple_type() {
    Tracker tracker(this);
    eat(Token::L_PAREN);
    PtrVector<ast::Type> args;
    parse_list(Token::R_PAREN, Token::COMMA, [&] {
        eat_nl();
        args.emplace_back(parse_type());
        eat_nl();
    });
    return make_ptr<ast::TupleType>(tracker(), std::move(args));
}

Ptr<ast::FnType> Parser::parse_fn_type(Ptr<ast::Type>&& from) {
    Tracker tracker(this, from->loc);
    eat(Token::ARROW);
    eat_nl();
    auto to = parse_type();
    return make_ptr<ast::FnType>(tracker(), std::move(from), std::move(to));
}

Ptr<ast::ErrorType> Parser::parse_error_type() {
    Tracker tracker(this);
    log::error(ahead().loc(), "expected type, got '{}'", ahead().string());
    next();
    return make_ptr<ast::ErrorType>(tracker());
}

ast::Path Parser::parse_path(ast::Identifier&& id) {
    Tracker tracker(this, id.loc);
    std::vector<ast::Path::Elem> elems;

    elems.emplace_back(std::move(id));
    while (ahead().tag() == Token::DOT) {
        eat(Token::DOT);
        eat_nl();
        elems.emplace_back(parse_id());
    }
    return ast::Path(tracker(), std::move(elems));
}

ast::Identifier Parser::parse_id() {
    Tracker tracker(this);
    std::string ident;
    if (ahead().is_identifier())
        ident = ahead().identifier();
    else
        log::error(ahead().loc(), "expected identifier, got '{}'", ahead().string());
    next();
    return Identifier(tracker(), std::move(ident));
}

Literal Parser::parse_lit() {
    Literal lit;
    if (!ahead().is_literal())
        log::error(ahead().loc(), "expected literal, got '{}'", ahead().string());
    else
        lit = ahead().literal();
    next();
    return lit;
}

Ptr<ast::Ptrn> Parser::expr_to_ptrn(Ptr<ast::Expr>&& expr) {
    if (auto tuple = expr->isa<TupleExpr>()) {
        PtrVector<ast::Ptrn> args;
        for (auto& arg : tuple->args)
            args.emplace_back(expr_to_ptrn(std::move(arg)));
        return make_ptr<ast::TuplePtrn>(expr->loc, std::move(args));
    } else if (auto path = expr->isa<PathExpr>()) {
        if (path->path.elems.size() == 1) {
            auto decl = make_ptr<ast::PtrnDecl>(expr->loc, std::move(path->identifier()));
            return make_ptr<ast::IdPtrn>(expr->loc, std::move(decl));
        }
    } else if (auto typed = expr->isa<TypedExpr>()) {
        return make_ptr<ast::TypedPtrn>(expr->loc, expr_to_ptrn(std::move(typed->expr)), std::move(typed->type));
    } else if (auto lit = expr->isa<LiteralExpr>()) {
        return make_ptr<LiteralPtrn>(expr->loc, lit->lit);
    }

    log::error(expr->loc, "invalid pattern");
    return make_ptr<ast::ErrorPtrn>(expr->loc);
}

} // namespace artic
