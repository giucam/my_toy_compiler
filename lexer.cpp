
#include <iostream>

#include "lexer.h"
#include "common.h"

static bool isWhitespace(int c)
{
    return c == ' ' || c == '\n' || c == '\t';
}

static bool isAlpha(int c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static bool isDigit(int c)
{
    return c >= '0' && c <= '9';
}

Lexer::Lexer(const std::string &filename)
     : m_filename(filename)
     , m_stream(filename)
     , m_lastChar(' ')
     , m_lineno(0)
     , m_colno(0)
{

}

void Lexer::reset()
{
    m_colno = 0;
    m_lineno = 0;
    m_lastChar = ' ';
    m_readTokens.clear();
    m_stream.clear();
    m_stream.seekg(0, m_stream.beg);
    m_line.clear();
}

int Lexer::nextChar()
{
    if (m_colno >= (int)m_line.size()) {
        m_line.clear();
        while (!m_stream.eof()) {
            int c = m_stream.get();
            if (c == '\n') {
                m_line += ' ';
                break;
            }
            m_line += c;
        }
        m_colno = 0;
        m_lineno += 1;

        std::cout<<"read line "<<m_lineno<<" "<<m_line<<"\n";

        m_lastChar = '\n';
    } else {
        m_lastChar = m_line[m_colno++];
    }
    return m_lastChar;
}

int Lexer::peekChar()
{
    if (m_colno < (int)m_line.size()) {
        return m_line[m_colno];
    }
    return ' ';
}

void Lexer::nextLine()
{
    while (m_lastChar != '\n' && !m_stream.eof()) {
        nextChar();
    }
}

Token Lexer::token(const std::string &str) const
{
    if (str == "extern") {
        return token(Token::Type::Extern);
    } else if (str == "func") {
        return token(Token::Type::Func);
    } else if (str == "struct") {
        return token(Token::Type::Struct);
    } else if (str == "iface") {
        return token(Token::Type::Iface);
    } else if (str == "impl") {
        return token(Token::Type::Impl);
    } else if (str == "return") {
        return token(Token::Type::Return);
    } else if (str == "let") {
        return token(Token::Type::Let);
    } else if (str == "if") {
        return token(Token::Type::If);
    } else if (str == "else") {
        return token(Token::Type::Else);
    } else if (str == "mut") {
        return token(Token::Type::Mut);
    } else if (str == "while") {
        return token(Token::Type::While);
    } else if (str == "true") {
        return token(Token::Type::True);
    } else if (str == "false") {
        return token(Token::Type::False);
    } else if (str == "import") {
        return token(Token::Type::Import);
    }

    return token(Token::Type::Identifier);
}

Token Lexer::nextToken()
{
    if (m_readTokens.empty()) {
        return readNextToken();
    }

    Token tok = m_readTokens.front();
    m_readTokens.pop_front();
    return tok;
}

Token Lexer::peekToken(int advance)
{
    advance -= 1;
    while ((int)m_readTokens.size() <= advance) {
        m_readTokens.push_back(readNextToken());
    }
    return m_readTokens[advance];
}

Token Lexer::readNextToken()
{
    while (!m_stream.eof()) {
        if (isWhitespace(m_lastChar)) {
            nextChar();
            continue;
        }

        if (isAlpha(m_lastChar)) {
            std::string str; str += m_lastChar;
            while (!m_stream.eof()) {
                nextChar();
                if (isAlpha(m_lastChar) || isDigit(m_lastChar)) {
                    str += m_lastChar;
                } else {
                    break;
                }
            }

            m_text = str;
            return token(str);
        } else if (isDigit(m_lastChar)) {
            std::string str; str += m_lastChar;
            while (!m_stream.eof()) {
                nextChar();
                if (isDigit(m_lastChar)) {
                    str += m_lastChar;
                } else {
                    break;
                }
            }
            m_text = str;
            return token(Token::Type::Numeric);
        } else if (m_lastChar == '"') {
            std::string str;
            while (!m_stream.eof()) {
                bool escape = (m_lastChar == '\\');
                nextChar();
                if (!escape && m_lastChar == '"') {
                    nextChar();
                    break;
                } else if (m_lastChar == '\\' && nextChar() == 'n') {
                    str += '\n';
                } else {
                    str += m_lastChar;
                }
            }
            m_text = str;
            fmt::print("literal {}\n",str);
            return token(Token::Type::StringLiteral);
        } else if (m_lastChar == '\'') {
            std::string str;
            while (!m_stream.eof()) {
                nextChar();
                if (m_lastChar == '\'') {
                    nextChar();
                    break;
                } else {
                    str += m_lastChar;
                }
            }
            m_text = str;
            return token(Token::Type::CharLiteral);
        } else if (m_lastChar == '.') {
            if (nextChar() != '.') {
                return token(Token::Type::Dot);
            } else {
                if (nextChar() != '.') {
                    return token(Token::Type::Unknown);
                } else {
                    nextChar();
                    return token(Token::Type::Ellipsis);
                }
            }
        } else if (m_lastChar == '(') {
            nextChar();
            return token(Token::Type::LeftParens);
        } else if (m_lastChar == ')') {
            nextChar();
            return token(Token::Type::RightParens);
        } else if (m_lastChar == '{') {
            nextChar();
            return token(Token::Type::LeftBrace);
        } else if (m_lastChar == '}') {
            nextChar();
            return token(Token::Type::RightBrace);
        } else if (m_lastChar == '[') {
            nextChar();
            return token(Token::Type::LeftBracket);
        } else if (m_lastChar == ']') {
            nextChar();
            return token(Token::Type::RightBracket);
        } else if (m_lastChar == ':') {
            nextChar();
            return token(Token::Type::Colon);
        } else if (m_lastChar == ',') {
            nextChar();
            return token(Token::Type::Comma);
        } else if (m_lastChar == ';') {
            nextChar();
            return token(Token::Type::Semicolon);
        } else if (m_lastChar == '*') {
            nextChar();
            return token(Token::Type::Star);
        } else if (m_lastChar == '/') {
            if (nextChar() == '/') {
                nextLine();
                continue;
            } else {
                return token(Token::Type::Div);
            }
        } else if (m_lastChar == '+') {
            nextChar();
            return token(Token::Type::Plus);
        } else if (m_lastChar == '-') {
            nextChar();
            return token(Token::Type::Minus);
        } else if (m_lastChar == '=') {
            if (nextChar() == '=') {
                nextChar();
                return token(Token::Type::CompareEqual);
            } else {
                return token(Token::Type::Equal);
            }
        } else if (m_lastChar == '!') {
            if (nextChar() == '=') {
                nextChar();
                return token(Token::Type::CompareNotEqual);
            }
        } else if (m_lastChar == '&') {
            nextChar();
            return token(Token::Type::Ampersand);
        } else if (m_lastChar == '<') {
            nextChar();
            return token(Token::Type::LeftAngleBracket);
        } else if (m_lastChar == '>') {
            nextChar();
            return token(Token::Type::RightAngleBracket);
        } else if (m_lastChar == '|') {
            nextChar();
            return token(Token::Type::Pipe);
        } else if (m_lastChar == '$') {
            nextChar();
            return token(Token::Type::Dollar);
        } else if (m_lastChar == '%') {
            nextChar();
            return token(Token::Type::Percent);
        } else {
            break;
        }
    }

    if (m_stream.eof()) {
        return token(Token::Type::EOF);
    }

    nextChar();
    return token(Token::Type::Unknown);
}
