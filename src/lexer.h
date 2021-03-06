#ifndef LEXER_H
#define LEXER_H

#include <unordered_map>
#include <istream>
#include <string>

#include "log.h"
#include "token.h"

namespace artic {

/// Generates a stream of tokens for the Parser.
class Lexer : public Logger {
public:
    Lexer(const std::string& filename, std::istream& is, const Logger& log = Logger());

    Token next();

private:
    struct Utf8Buffer {
        char buf[4];
        size_t count;

        Utf8Buffer()
            : buf{0, 0, 0, 0}, count(0)
        {}

        bool empty() const { return count == 0; }
        bool fill(std::istream&);
        uint32_t decode();
    };

    void eat();
    void eat_spaces();
    void eat_comments();
    Literal parse_literal();

    void accept();
    bool accept(uint32_t);
    bool accept(const std::string&);

    uint32_t peek() const { return code_; }
    bool eof() const { return eof_; }

    std::istream& stream_;

    Loc loc_;
    bool eof_;
    uint32_t code_;
    Utf8Buffer buf_;
    std::string str_;

    static std::unordered_map<std::string, Token::Tag> keywords;
};

} // namespace artic

#endif // LEXER_H
