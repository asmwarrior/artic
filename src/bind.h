#ifndef BIND_NAMES_H
#define BIND_NAMES_H

#include <unordered_map>
#include <vector>

#include "symbol.h"
#include "ast.h"
#include "log.h"

namespace artic {

class Expr;

/// Binds identifiers to the nodes of the AST.
class NameBinder : public Logger {
public:
    NameBinder(const Logger& log = Logger())
        : Logger(log), var_depth_(0)
    {
        push_scope();
    }

    ~NameBinder() { pop_scope(); }

    /// Performs name binding on a whole program.
    /// Returns true on success, otherwise false.
    bool run(const ast::Program&);

    void bind_head(const ast::Decl&);
    void bind(const ast::Node&);

    void push_scope(size_t vars = 0) {
        var_depth_ += vars;
        scopes_.emplace_back();
    }

    void pop_scope(size_t vars = 0) {
        scopes_.pop_back();
        var_depth_ -= vars;
    }

    void insert_var(const ast::TypeParam&) { var_depth_++; }
    void insert_symbol(const ast::NamedDecl&);

    std::shared_ptr<Symbol> find_symbol(const std::string& name) {
        for (auto it = scopes_.rbegin(); it != scopes_.rend(); it++) {
            if (auto decl = it->find(name)) return decl;
        }
        return nullptr;
    }

private:
    std::vector<SymbolTable> scopes_;
    size_t var_depth_;
};

} // namespace artic

#endif // BIND_NAMES_H
