
#include <memory>

#include "parser.h"
#include "lexer.h"
#include "common.h"
#include "node.h"

Parser::Parser(const std::string &filename)
      : m_filename(filename)
      , m_lexer(filename)
{
}

void Parser::checkTokenType(const Token &tok, Token::Type expected) const
{
    if (tok.type() != expected) {
        err(tok, "wrong token found: expected {}, found {}", expected, tok.type());
    }
}

Token Parser::nextToken(Token::Type expected)
{
    auto tok = m_lexer.nextToken();
    checkTokenType(tok, expected);
    return tok;
}

static int opPrecedence(Token::Type t)
{
    switch (t) {
        case Token::Type::Star: return 40;
        case Token::Type::Div: return 40;
        case Token::Type::Plus: return 20;
        case Token::Type::Minus: return 20;
        case Token::Type::LeftAngleBracket: return 10;
        case Token::Type::RightAngleBracket: return 10;
        case Token::Type::CompareEqual: return 5;
        default:
            break;
    }
    return -1;
}

std::unique_ptr<NExpression> Parser::parseBinOp(std::unique_ptr<NExpression> lhs, int precedence)
{
    while (true) {
        auto opTok = m_lexer.peekToken();
        int tokPrec = opPrecedence(opTok.type());

        if (tokPrec < precedence) {
            return std::move(lhs);
        }
        nextToken();

        NBinaryOperator::OP op = [&]() {
            switch (opTok.type()) {
                case Token::Type::Star: return NBinaryOperator::OP::Mul;
                case Token::Type::Plus: return NBinaryOperator::OP::Add;
                case Token::Type::Minus: return NBinaryOperator::OP::Sub;
                case Token::Type::LeftAngleBracket: return NBinaryOperator::OP::Lesser;
                case Token::Type::RightAngleBracket: return NBinaryOperator::OP::Greater;
                case Token::Type::CompareEqual: return NBinaryOperator::OP::Equal;
                default:
                    abort();
            }
        }();
        auto rhs = parsePrimary(nullptr);
        if (!rhs) {
            return nullptr;
        }

        int nextPrec = opPrecedence(m_lexer.peekToken().type());

        if (tokPrec < nextPrec) {
            rhs = parseBinOp(std::move(rhs), tokPrec + 1);
            if (!rhs) {
                return nullptr;
            }
        }

        lhs = std::make_unique<NBinaryOperator>(std::move(lhs), op, std::move(rhs));
    }
}

std::unique_ptr<NExpression> Parser::parsePrimary(NExpression *context)
{
        auto parseExpressionList = [&]() {
        checkTokenType(nextToken(), Token::Type::LeftParens);

        ExpressionList list;
        while (m_lexer.peekToken().type() != Token::Type::RightParens) {
            auto ex = parseExpression();
            list.push_back(std::move(ex));

            if (m_lexer.peekToken().type() == Token::Type::Comma) {
                nextToken();
            }
        }
        nextToken();
        return list;
    };

    auto tok = m_lexer.peekToken();

    if (tok.type() == Token::Type::Identifier || (tok.type() == Token::Type::Numeric && context)) {
        nextToken();
        if (m_lexer.peekToken().type() == Token::Type::LeftParens) {
            ExpressionList list = parseExpressionList();
            auto ex = std::make_unique<NMethodCall>(tok, tok.text(), list);
            ex->pushContext(context);

            return ex;
        }
        if (tok.type() == Token::Type::Numeric) {
            return std::make_unique<NIdentifier>(tok, std::stol(tok.text()));
        }
        return std::make_unique<NIdentifier>(tok, tok.text());
    } else if (tok.type() == Token::Type::Numeric) {
        nextToken();

        //only if the token after the dot is a numeric too, otherwise it's a method call on the given numeric
        if (m_lexer.peekToken(1).type() == Token::Type::Dot && m_lexer.peekToken(2).type() == Token::Type::Numeric) {
            nextToken();
            auto decTok = nextToken();
            return std::make_unique<NDouble>(tok, std::stof(tok.text() + "." + decTok.text()));
        }
        return std::make_unique<NInteger>(tok, std::stol(tok.text()));
    } else if (tok.type() == Token::Type::StringLiteral) {
        nextToken();
        return std::make_unique<NString>(tok.text());
    } else if (tok.type() == Token::Type::LeftParens) {
        ExpressionList list = parseExpressionList();

        return std::make_unique<NExpressionPack>(tok, list);
    } else if (tok.type() == Token::Type::Ampersand) {
        nextToken();
        checkTokenType(m_lexer.peekToken(), Token::Type::Identifier);
        return std::make_unique<NAddressOfExpression>(tok, parseExpression());
    } else if (tok.type() == Token::Type::CharLiteral) {
        nextToken();
        if (tok.text().size() < 1) {
            err(tok, "empty char literal");
        } else if (tok.text().size() > 1) {
            err(tok, "multibyte char literal");
        }
        return std::make_unique<NInteger>(tok, tok.text().data()[0]);
    }
    return nullptr;
}

std::unique_ptr<NExpression> Parser::parseExpression(NExpression *context)
{
    auto expr = parsePrimary(context);
    if (expr) {
        expr->pushContext(context);

        auto peekType = m_lexer.peekToken().type();
        switch (peekType) {
            case Token::Type::Dot: {
                nextToken();

                auto ex = parseExpression(expr.get());
                ex->attach(std::move(expr));
                expr = std::move(ex);
                break;
            }
            case Token::Type::Equal: {
                auto tok = nextToken();

                auto rhs = parseExpression();
                expr = std::make_unique<NAssignment>(tok, std::move(expr), std::move(rhs));
                break;
            }
            case Token::Type::LeftAngleBracket:
            case Token::Type::RightAngleBracket:
            case Token::Type::CompareEqual:
            case Token::Type::Star:
            case Token::Type::Minus:
            case Token::Type::Plus: {
                expr = parseBinOp(std::move(expr), 0);
                break;
            }
            default:
                break;
        }
    }
    if (expr) {
        return expr;
    }

    err(nextToken(), "unable to parse expression");
    return nullptr;
}

void Parser::parseReturn()
{
    checkTokenType(nextToken(), Token::Type::Return);
    NReturnStatement *stmt = nullptr;
    if (m_lexer.peekToken().type() == Token::Type::Semicolon) {
        nextToken();
        stmt = new NReturnStatement(nullptr);
    } else {
        stmt = new NReturnStatement(std::move(parseExpression()));
        checkTokenType(nextToken(), Token::Type::Semicolon);
    }
    m_block->statements.push_back(stmt);
}

void Parser::parseLet()
{
    auto letTok = nextToken(Token::Type::Let);

    if (m_lexer.peekToken().type() == Token::Type::Extern) {
        nextToken();
        auto name = nextToken(Token::Type::Identifier);
        nextToken(Token::Type::Colon);
        auto type = parseType();
        nextToken(Token::Type::Semicolon);

        auto var = new NExternVariableDeclaration(letTok, name.text(), type);
        m_block->statements.push_back(var);
        return;
    }

    auto checkMut = [&]() {
        if (m_lexer.peekToken().type() == Token::Type::Mut) {
            nextToken();
            return true;
        }
        return false;
    };

    std::vector<NVariableName> nameToks;
    bool isMulti = false;
    if (m_lexer.peekToken().type() == Token::Type::LeftParens) {
        isMulti = true;
        nextToken();
        while (m_lexer.peekToken().type() != Token::Type::RightParens) {
            bool mut = checkMut();
            auto tok = nextToken(Token::Type::Identifier);
            nameToks.emplace_back(tok, tok.text(), mut);

            if (m_lexer.peekToken().type() == Token::Type::Comma) {
                nextToken();
            }
        }
        nextToken();
    } else {
        bool mut = checkMut();
        auto tok = nextToken(Token::Type::Identifier);
        nameToks.emplace_back(tok, tok.text(), mut);
    }

    auto &&varName = nameToks.front();

    NStatement *var = nullptr;

    auto tok = nextToken();
    if (tok.type() == Token::Type::Equal) {
        auto expr = parseExpression();

        if (!isMulti) {
            var = new NVariableDeclaration(letTok, varName, std::make_unique<NVarExpressionInitializer>(tok, TypeName(), std::move(expr)));
        } else {
            var = new NMultiVariableDeclaration(letTok, nameToks, std::move(expr));
        }
    } else if (tok.type() == Token::Type::Colon) {
        auto type = parseType();
        checkTokenType(nextToken(), Token::Type::Equal);

        if (m_lexer.peekToken().type() == Token::Type::LeftBrace) {
            std::vector<NVarStructInitializer::Field> list;

            nextToken(); // brace
            auto tok = nextToken();
            while (tok.type() != Token::Type::RightBrace) {
                checkTokenType(tok, Token::Type::Identifier);
                checkTokenType(nextToken(), Token::Type::Equal);
                auto expr = parseExpression();

                list.push_back({tok.text(), std::move(expr)});

                tok = nextToken();
                if (tok.type() == Token::Type::Comma) {
                    tok = nextToken();
                } else if (tok.type() != Token::Type::RightBrace) {
                    err(tok, "',' or '{}' expected", '}');
                }
            }

            var = new NVariableDeclaration(letTok, varName, std::make_unique<NVarStructInitializer>(tok, type, list));
        } else {
            auto expr = parseExpression();
            var = new NVariableDeclaration(letTok, varName, std::make_unique<NVarExpressionInitializer>(tok, type, std::move(expr)));
        }
    } else {
        err(tok, "':' or '=' expected");
    }

    assert(var);
    m_block->statements.push_back(var);

    checkTokenType(nextToken(), Token::Type::Semicolon);
}

void Parser::parseStruct()
{
    checkTokenType(nextToken(), Token::Type::Struct);

    auto nameTok = nextToken(Token::Type::Identifier);
    checkTokenType(nextToken(), Token::Type::LeftBrace);

    std::vector<NStructDeclaration::Field> list;

    auto tok = nextToken();
    while (tok.type() != Token::Type::RightBrace) {
        bool mut = false;
        if (tok.type() == Token::Type::Mut) {
            mut = true;
            tok = nextToken();
        }

        checkTokenType(tok, Token::Type::Identifier);
        checkTokenType(nextToken(), Token::Type::Colon);
        auto type = parseType();
        checkTokenType(nextToken(), Token::Type::Semicolon);

        list.push_back({ tok.text(),  type, mut });

        tok = nextToken();
    }

    auto decl = new NStructDeclaration(nameTok.text(), list);
    m_block->statements.push_back(decl);
}

std::vector<TypeName> Parser::parseParameterList()
{
    checkTokenType(nextToken(), Token::Type::LeftParens);

    std::vector<TypeName> list;
    while (m_lexer.peekToken().type() != Token::Type::RightParens) {
//     auto tok = nextToken();
//     while (tok.type() != Token::Type::RightParens) {
        list.push_back(parseType());
//         checkTokenType(tok, Token::Type::Identifier);

//         list.emplace_back(tok, tok.text());

//         tok = nextToken();
        if (m_lexer.peekToken().type() == Token::Type::Comma) {
            nextToken();
        }
    }
    nextToken();

    return list;
}

void Parser::parseIface()
{
    checkTokenType(nextToken(), Token::Type::Iface);

    auto nameTok = nextToken(Token::Type::Identifier);
    auto paraList = parseParameterList();

    checkTokenType(nextToken(), Token::Type::LeftBrace);

    NIfacePrototypeList protoList;
    while (true) {
        checkTokenType(nextToken(), Token::Type::Func);
        auto funcTok = nextToken(Token::Type::Identifier);
        auto funcList = parseParameterList();

        protoList.push_back(new NIfacePrototype(funcTok.text(), funcList));

        checkTokenType(nextToken(), Token::Type::Semicolon);

        if (m_lexer.peekToken().type() == Token::Type::RightBrace) {
            nextToken();
            break;
        }
    }

    auto iface = new NIfaceDeclaration(nameTok.text(), paraList, protoList);
    m_block->statements.push_back(iface);
}

void Parser::parseImpl()
{
    checkTokenType(nextToken(), Token::Type::Impl);

    auto nameTok = nextToken(Token::Type::Identifier);
    auto parList = parseParameterList();

    checkTokenType(nextToken(), Token::Type::LeftBrace);

    FuncDeclarationList list;
    while (m_lexer.peekToken().type() == Token::Type::Func) {
        list.push_back(parseFunc());
    }
    checkTokenType(nextToken(), Token::Type::RightBrace);

    auto impl = new NImplDeclaration(nameTok.text(), parList, list);
    m_block->statements.push_back(impl);
}

void Parser::parseExpressionStatement()
{
    auto expr = parseExpression();
    auto stmt = new NExpressionStatement(std::move(expr));

    m_block->statements.push_back(stmt);

    checkTokenType(nextToken(), Token::Type::Semicolon);
}

void Parser::parseIf()
{
    nextToken(Token::Type::If);

    nextToken(Token::Type::LeftParens);
    auto expr = parseExpression();
    nextToken(Token::Type::RightParens);

    auto block = parseBlock();
    NBlock *elseBlock = nullptr;

    if (m_lexer.peekToken().type() == Token::Type::Else) {
        nextToken();
        elseBlock = parseBlock();
    }

    auto stmt = new NIfStatement(std::move(expr), block, elseBlock);
    m_block->statements.push_back(stmt);
}

void Parser::parseWhile()
{
    nextToken(Token::Type::While);

    nextToken(Token::Type::LeftParens);
    auto expr = parseExpression();
    nextToken(Token::Type::RightParens);

    auto block = parseBlock();

    auto stmt = new NWhileStatement(std::move(expr), block);
    m_block->statements.push_back(stmt);
}

void Parser::parseStatements()
{
    fmt::print("block\n");
    bool eof = false;
    while (!eof) {
        auto tok = m_lexer.peekToken();
        switch (tok.type()) {
        case Token::Type::Unknown:
            err(tok, "unknown token");
        case Token::Type::EOF:
            eof = true;
            break;
        case Token::Type::Func:
            if (m_lexer.peekToken(2).type() == Token::Type::Extern) {
                parseExtern();
            } else {
                m_block->statements.push_back(parseFunc());
            }
            break;
        case Token::Type::Return:
            parseReturn();
            break;
        case Token::Type::Let:
            parseLet();
            break;
        case Token::Type::Struct:
            parseStruct();
            break;
        case Token::Type::Iface:
            parseIface();
            break;
        case Token::Type::Impl:
            parseImpl();
            break;
        case Token::Type::If:
            parseIf();
            break;
        case Token::Type::While:
            parseWhile();
            break;
        case Token::Type::RightBrace:
            fmt::print("br\n");
            return;
        case Token::Type::Identifier:
        case Token::Type::Numeric:
        case Token::Type::LeftParens:
            parseExpressionStatement();
            break;
//             Err(m_filename, tok.lineNo(), tok.columnNo()).line("unexpected identifier").line("{}", tok.line()).line(cursorLine(tok.columnNo()));
//             fmt::print("ident {}\n", tok.text());
//             break;
//         case Token::Type::StringLiteral:
//             fmt::print("literal '{}'\n", tok.text());
//             break;
        default:
            err(tok, "unhandled token of type {}", tok.type());
//             tok = m_lexer.nextToken();
        }
    }
}

NBlock *Parser::parseBlock()
{
    checkTokenType(nextToken(), Token::Type::LeftBrace);

    NBlock *old = m_block;
    NBlock *block = new NBlock;
    m_block = block;
    parseStatements();

    m_block = old;

    checkTokenType(nextToken(), Token::Type::RightBrace);

    return block;
}

void Parser::parseExtern()
{
    fmt::print("extern\n");
    nextToken(Token::Type::Func);
    checkTokenType(nextToken(), Token::Type::Extern);

    auto nameTok = nextToken(Token::Type::Identifier);
    auto leftParensTok = nextToken(Token::Type::LeftParens);

    std::vector<NFunctionArgumentDeclaration> list;
    auto tok = nextToken();
    bool varargs = false;
    while (true) {
        if (tok.type() == Token::Type::RightParens) {
            break;
        } else if (tok.type() == Token::Type::Ellipsis) {
            nextToken(Token::Type::RightParens);
            varargs = true;
            break;
        }

        checkTokenType(tok, Token::Type::Identifier);
        checkTokenType(nextToken(), Token::Type::Colon);

        list.emplace_back(tok, tok.text(), parseType(), true);

        tok = nextToken();
        if (tok.type() == Token::Type::Comma) {
            tok = nextToken();
        }
    }

    checkTokenType(nextToken(), Token::Type::Colon);

    auto decl = new NExternDeclaration(nameTok.text(), parseType(), list, varargs);
    nextToken(Token::Type::Semicolon);
    m_block->statements.push_back(decl);
}

TypeName Parser::parseType()
{
    int pointer = 0;
    auto typeTok = nextToken();
    if (typeTok.type() == Token::Type::LeftParens) {
        std::vector<Token> types;
        std::string type = "(";
        while (m_lexer.peekToken().type() != Token::Type::RightParens) {
            auto tok = nextToken();
            types.push_back(tok);
            type += tok.type() == Token::Type::Ellipsis ? "..." : tok.text();

            if (m_lexer.peekToken().type() == Token::Type::Comma) {
                type += ", ";
                nextToken();
            }
        }
        type += ")";
        nextToken();

        return TypeName(typeTok, type, 0);
    }
    while (typeTok.type() == Token::Type::Star) {
        typeTok = nextToken();
        pointer++;
    }
    checkTokenType(typeTok, Token::Type::Identifier);
    return TypeName(typeTok, typeTok.text(), pointer);
}

NFunctionDeclaration *Parser::parseFunc()
{
    fmt::print("func\n");

    checkTokenType(nextToken(), Token::Type::Func);

    auto nameTok = nextToken(Token::Type::Identifier);
    auto leftParensTok = nextToken(Token::Type::LeftParens);

    std::vector<NFunctionArgumentDeclaration> list;
    auto tok = nextToken();
    while (true) {
        if (tok.type() == Token::Type::RightParens) {
            break;
        }

        bool mut = false;
        if (tok.type() == Token::Type::Mut) {
            mut = true;
            tok = nextToken();
        }

        checkTokenType(tok, Token::Type::Identifier);
        checkTokenType(nextToken(), Token::Type::Colon);

        list.emplace_back(tok, tok.text(), parseType(), mut);

        tok = nextToken();
        if (tok.type() == Token::Type::Comma) {
            tok = nextToken();
        }
    }

    checkTokenType(nextToken(), Token::Type::Colon);
    auto retType = parseType();

    auto block = parseBlock();


    auto decl = new NFunctionDeclaration(nameTok.text(), retType, list, block);
    return decl;
}

void Parser::parse(NBlock *root)
{
    m_block = root;
    parseStatements();
}
