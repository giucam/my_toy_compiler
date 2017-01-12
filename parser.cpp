
#include <memory>

#include "parser.h"
#include "lexer.h"
#include "common.h"
#include "node.h"

inline std::string cursorLine(int col)
{
    return std::string(std::max(col - 1, 0), ' ') + '^';
}

template<class... Args>
void err(const Token &tok, Args &&... args)
{
    Err(tok.filename(), tok.lineNo(), tok.columnNo()).line(std::forward<Args>(args)...).line("{}", tok.line()).line(cursorLine(tok.columnNo()));
}

Parser::Parser(const std::string &filename)
      : m_filename(filename)
      , m_lexer(filename)
{
}

void Parser::checkTokenType(const Token &tok, Token::Type expected) const
{
    if (tok.type() != expected) {
        if (expected == Token::Type::Identifier) {
            err(tok, "expected identifier");
        } else {
            err(tok, "wrong token found: expected {}, found {}", expected, tok.type());
        }
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
            return std::make_unique<NDouble>(std::stof(tok.text() + "." + decTok.text()));
        }
        return std::make_unique<NInteger>(std::stol(tok.text()));
    } else if (tok.type() == Token::Type::StringLiteral) {
        nextToken();
        return std::make_unique<NString>(tok.text());
    } else if (tok.type() == Token::Type::LeftParens) {
        ExpressionList list = parseExpressionList();

        return std::make_unique<NExpressionPack>(tok, list);
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
                nextToken();

                auto rhs = parseExpression();
                expr = std::make_unique<NAssignment>(std::move(expr), std::move(rhs));
                break;
            }
            case Token::Type::Star:
            case Token::Type::Plus: {
//                 nextToken();

                expr = parseBinOp(std::move(expr), 0);
//                 auto rhs = parseExpression();
//                 NBinaryOperator::OP op = [&]() {
//                     switch (peekType) {
//                         case Token::Type::Star: return NBinaryOperator::OP::Mul;
//                         case Token::Type::Plus: return NBinaryOperator::OP::Add;
//                         default:
//                             abort();
//                     }
//                 }();
//                 expr = std::make_unique<NBinaryOperator>(std::move(expr), op, std::move(rhs));
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
    auto statement = new NReturnStatement(std::move(parseExpression()));

    checkTokenType(nextToken(), Token::Type::Semicolon);
    m_block->statements.push_back(statement);
}

void Parser::parseLet()
{
    checkTokenType(nextToken(), Token::Type::Let);

    auto nameTok = nextToken(Token::Type::Identifier);

    NVariableDeclaration *var = nullptr;

    auto tok = nextToken();
    if (tok.type() == Token::Type::Equal) {
        auto expr = parseExpression();

        auto id = std::make_unique<NIdentifier>(nameTok, nameTok.text());
        var = new NVariableDeclaration(nameTok, nameTok.text(), new NAssignment(std::move(id), std::move(expr)));
    } else if (tok.type() == Token::Type::Colon) {
        auto typeTok = nextToken(Token::Type::Identifier);
        checkTokenType(nextToken(), Token::Type::Equal);

        if (m_lexer.peekToken().type() == Token::Type::LeftBrace) {
            AssignmentList list;

            nextToken(); // brace
            auto tok = nextToken();
            while (tok.type() != Token::Type::RightBrace) {
                checkTokenType(tok, Token::Type::Identifier);
                checkTokenType(nextToken(), Token::Type::Equal);
                auto expr = parseExpression();

                list.push_back(new NAssignment(std::make_unique<NIdentifier>(tok, tok.text()), std::move(expr)));

                tok = nextToken();
                if (tok.type() == Token::Type::Comma) {
                    tok = nextToken();
                } else if (tok.type() != Token::Type::RightBrace) {
                    err(tok, "',' or '{}' expected", '}');
                }
            }

            var = new NVariableDeclaration(nameTok, nameTok.text(), TypeName(typeTok, typeTok.text()), list);
        } else {
            auto expr = parseExpression();

            auto id = std::make_unique<NIdentifier>(nameTok, nameTok.text());
            var = new NVariableDeclaration(nameTok, nameTok.text(), TypeName(typeTok, typeTok.text()), new NAssignment(std::move(id), std::move(expr)));
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

    std::vector<NVariableDeclaration> list;

    auto tok = nextToken();
    while (tok.type() != Token::Type::RightBrace) {

        checkTokenType(tok, Token::Type::Identifier);
        checkTokenType(nextToken(), Token::Type::Colon);
        auto typeTok = nextToken(Token::Type::Identifier);
        checkTokenType(nextToken(), Token::Type::Semicolon);

        list.push_back(NVariableDeclaration(tok, tok.text(), TypeName(typeTok, typeTok.text())));

        tok = nextToken();
    }

    auto decl = new NStructDeclaration(nameTok.text(), list);
    m_block->statements.push_back(decl);
}

NIfaceParameterList Parser::parseParameterList()
{
    checkTokenType(nextToken(), Token::Type::LeftParens);

    NIfaceParameterList list;
    auto tok = nextToken();
    while (tok.type() != Token::Type::RightParens) {
        checkTokenType(tok, Token::Type::Identifier);

        list.emplace_back(tok, tok.text());

        tok = nextToken();
        if (tok.type() == Token::Type::Comma) {
            tok = nextToken();
        }
    }

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
        case Token::Type::Extern:
            parseExtern();
            break;
        case Token::Type::Func:
            m_block->statements.push_back(parseFunc());
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
    checkTokenType(nextToken(), Token::Type::Extern);

    auto nameTok = nextToken(Token::Type::Identifier);
    auto leftParensTok = nextToken(Token::Type::LeftParens);

    std::vector<NFunctionArgumentDeclaration> list;
    auto tok = nextToken();
    while (true) {
        if (tok.type() == Token::Type::RightParens) {
            break;
        }

        checkTokenType(tok, Token::Type::Identifier);
        checkTokenType(nextToken(), Token::Type::Colon);
        auto typeTok = nextToken(Token::Type::Identifier);

        list.emplace_back(tok, tok.text(), TypeName(typeTok, typeTok.text()));

        tok = nextToken();
        if (tok.type() == Token::Type::Comma) {
            tok = nextToken();
        }
    }

    checkTokenType(nextToken(), Token::Type::Colon);
    auto retTypeTok = nextToken(Token::Type::Identifier);

    auto decl = new NExternDeclaration(nameTok.text(), TypeName(retTypeTok, retTypeTok.text()), list);
    m_block->statements.push_back(decl);
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

        checkTokenType(tok, Token::Type::Identifier);
        checkTokenType(nextToken(), Token::Type::Colon);
        auto typeTok = nextToken(Token::Type::Identifier);

        list.emplace_back(tok, tok.text(), TypeName(typeTok, typeTok.text()));

        tok = nextToken();
        if (tok.type() == Token::Type::Comma) {
            tok = nextToken();
        }
    }

    checkTokenType(nextToken(), Token::Type::Colon);
    auto retTypeTok = nextToken(Token::Type::Identifier);

    auto block = parseBlock();


    auto decl = new NFunctionDeclaration(nameTok.text(), TypeName(retTypeTok, retTypeTok.text()), list, block);
    return decl;
}

void Parser::parse(NBlock *root)
{
    m_block = root;
    parseStatements();
}
