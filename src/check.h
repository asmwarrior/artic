#ifndef CHECK_H
#define CHECK_H

#include "ast.h"
#include "log.h"

namespace artic {

/// Utility class to perform type checking.
class TypeChecker : public Logger {
public:
    TypeChecker(const Logger& log = Logger()) : Logger(log) {}

    /// Performs type checking on a whole program.
    /// Returns true on success, otherwise false.
    bool run(const ast::Program&);
    void check(const ast::Node&);
};

} // namespace artic

#endif // CHECK_H
