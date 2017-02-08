
#ifndef COMMON_H
#define COMMON_H

#include "fmt/format.h"
#include "fmt/ostream.h"

#include "lexer.h"

template<class... Args>
static void error(Args &&... args)
{
    auto s = fmt::format(std::forward<Args>(args)...);
    fmt::print(stderr, "\033[1;31merror:\033[1;37m {}.\033[0m\n", s);
    exit(1);
}

struct Message
{
    enum class Type {
        Debug,
        Warning,
        Error,
    };

    Type type;
    std::string filename;
    int lineno;
    int colno;
    bool firstLine;

    Message(Type t, const std::string &fname, int l, int c) : type(t), filename(fname), lineno(l), colno(c), firstLine(true) {}
    ~Message() { if (type == Type::Error) exit(2); }

    template<class... Args>
    Message &line(Args &&... args)
    {
        if (firstLine) {
            auto s = fmt::format(std::forward<Args>(args)...);
            switch (type) {
                case Type::Error:
                    fmt::print(stderr, "\033[1;37m{}:{}:{}: \033[1;31merror:\033[1;37m {}\033[0m\n", filename, lineno, colno, s);
                    break;
                case Type::Warning:
                    fmt::print(stderr, "\033[1;37m{}:{}:{}: \033[1;31mwarning:\033[1;37m {}\033[0m\n", filename, lineno, colno, s);
                    break;
                case Type::Debug:
                    fmt::print(stderr, "\033[1;37m{}:{}:{}: \033[1;31mdebug:\033[1;37m {}\033[0m\n", filename, lineno, colno, s);
                    break;
            }
            firstLine = false;
        } else {
            fmt::print(stderr, std::forward<Args>(args)...);
            fmt::print(stderr, "\n");
        }
        return *this;
    }

};

template<class... Args>
void err(const Token &tok, Args &&... args)
{
    Message(Message::Type::Error, tok.filename(), tok.lineNo(), tok.columnNo()).line(std::forward<Args>(args)...)
                                                                               .line("{}", tok.line()).line(std::string(std::max(tok.columnNo() - 1, 0), ' ') + '^');
}

template<class... Args>
void warning(const Token &tok, Args &&... args)
{
    Message(Message::Type::Warning, tok.filename(), tok.lineNo(), tok.columnNo()).line(std::forward<Args>(args)...)
                                                                                 .line("{}", tok.line()).line(std::string(std::max(tok.columnNo() - 1, 0), ' ') + '^');
}

template<class... Args>
void debug(const Token &tok, Args &&... args)
{
    Message(Message::Type::Debug, tok.filename(), tok.lineNo(), tok.columnNo()).line(std::forward<Args>(args)...)
                                                                               .line("{}", tok.line()).line(std::string(std::max(tok.columnNo() - 1, 0), ' ') + '^');
}

template<class... Args>
inline void error2(const std::string &filename, int lineno, int colno, Args &&... args)
{
    Message(Message::Type::Error, filename, lineno, colno).line(std::forward<Args>(args)...);
}

template<class T>
class Optional
{
public:
    Optional() : m_valid(false) {}
    Optional(T t) : m_valid(true), m_value(std::move(t)) {}

    operator bool() const { return m_valid; }
    operator const T &() const { return m_value; }
    const T *operator->() const { return &m_value; }
    T *operator->() { return &m_value; }

private:
    bool m_valid;
    T m_value;
};


#endif
