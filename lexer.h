
#ifndef LEXER_H
#define LEXER_H

#include <fstream>
#include <deque>

#undef EOF

class Token
{
public:
    enum class Type {
        Unknown,
        EOF,
        Extern,
        Func,
        Struct,
        Let,
        Iface,
        Impl,
        Identifier,
        Numeric,
        Return,
        LeftParens,
        RightParens,
        LeftBrace,
        RightBrace,
        LeftBracket,
        RightBracket,
        Colon,
        Comma,
        Semicolon,
        Dot,
        Star,
        Div,
        Plus,
        Minus,
        Equal,
        CompareEqual,
        StringLiteral,
    };
    Token() {}
    Token(Type t, int l, int c, const std::string &filename, const std::string &txt, const std::string &line): m_type(t), m_lineno(l), m_columnno(c), m_filename(filename), m_line(line), m_text(txt) {}

    Type type() const { return m_type; }
    int lineNo() const { return m_lineno; }
    int columnNo() const { return m_columnno; }
    const std::string &filename() const { return m_filename; }
    const std::string &line() const { return m_line; }
    const std::string &text() const { return m_text; }

private:
    Type m_type;
    int m_lineno;
    int m_columnno;
    std::string m_filename;
    std::string m_line;
    std::string m_text;
};

inline std::ostream &operator<<(std::ostream &s, Token::Type t)
{
#define TOK(tok) case Token::Type::tok: s << #tok; break;
#define TOKSTR(tok, str) case Token::Type::tok: s << str; break;
    switch (t) {
        TOK(Unknown)
        TOK(EOF)
        TOKSTR(Extern, "'extern'")
        TOKSTR(Func, "'func'")
        TOKSTR(Struct, "'struct'")
        TOKSTR(Let, "'let'")
        TOKSTR(Iface, "'iface'")
        TOKSTR(Impl, "'impl'")
        TOK(Identifier)
        TOK(Numeric)
        TOKSTR(Return, "'return'")
        TOKSTR(LeftParens, "'('")
        TOKSTR(RightParens, "')'")
        TOKSTR(LeftBrace, "'{'")
        TOKSTR(RightBrace, "'}'")
        TOKSTR(LeftBracket, "'['")
        TOKSTR(RightBracket, "']'")
        TOKSTR(Colon, "':'")
        TOKSTR(Comma, "','")
        TOKSTR(Semicolon, "';'")
        TOKSTR(Dot, "'.'")
        TOKSTR(Star, "'*'")
        TOKSTR(Div, "'/'")
        TOKSTR(Plus, "'+'")
        TOKSTR(Minus, "'-'")
        TOKSTR(Equal, "'='")
        TOKSTR(CompareEqual, "'=='")
        TOK(StringLiteral)
    }
#undef TOK
    return s;
}


class Lexer
{
public:
    Lexer(const std::string &filename);

    Token peekToken(int advance = 1);
    Token nextToken();

private:
    Token readNextToken();
    int nextChar();
    int peekChar();
    void nextLine();
    inline Token token(Token::Type type) const { return Token(type, m_lineno, m_colno > 0 ? m_colno - 1 : m_line.size(), m_filename, m_text, m_line); }
    Token token(const std::string &str) const;

    std::string m_filename;
    std::ifstream m_stream;
    int m_lastChar;
    std::string m_line;
    std::string m_text;
    int m_lineno;
    int m_colno;
    std::deque<Token> m_readTokens;
};

#endif
