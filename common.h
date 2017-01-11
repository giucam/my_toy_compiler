
#ifndef COMMON_H
#define COMMON_H

#include "fmt/format.h"
#include "fmt/ostream.h"

template<class... Args>
static void error(Args &&... args)
{
    auto s = fmt::format(std::forward<Args>(args)...);
    fmt::print(stderr, "\033[1;31merror:\033[1;37m {}.\033[0m\n", s);
    exit(1);
}

// template<class... Args>
// static void error2(const std::string &filename, int lineno, int colno, Args &&... args)
// {
//     auto s = fmt::format(std::forward<Args>(args)...);
//     fmt::print(stderr, "\033[1;37m{}:{}:{}: \033[1;31merror:\033[1;37m {}.\033[0m\n", filename, lineno, colno, s);
//     exit(1);
// }

struct Err
{
    std::string filename;
    int lineno;
    int colno;
    bool firstLine;

    Err(const std::string &fname, int l, int c) : filename(fname), lineno(l), colno(c), firstLine(true) {}
    ~Err() { exit(2); }

    template<class... Args>
    Err &line(Args &&... args)
    {
        if (firstLine) {
            auto s = fmt::format(std::forward<Args>(args)...);
            fmt::print(stderr, "\033[1;37m{}:{}:{}: \033[1;31merror:\033[1;37m {}\033[0m\n", filename, lineno, colno, s);
            firstLine = false;
        } else {
            fmt::print(stderr, std::forward<Args>(args)...);
            fmt::print(stderr, "\n");
        }
        return *this;
    }

};

template<class... Args>
inline void error2(const std::string &filename, int lineno, int colno, Args &&... args)
{
    Err(filename, lineno, colno).line(std::forward<Args>(args)...);
//     auto s = fmt::format(std::forward<Args>(args)...);
//     fmt::print(stderr, "\033[1;37m{}:{}:{}: \033[1;31merror:\033[1;37m {}.\033[0m\n", filename, lineno, colno, s);
//     exit(1);
}

#endif
