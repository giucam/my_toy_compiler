
#ifndef PARSER_H
#define PARSER_H

#include <fstream>

#include "lexer.h"
#include "node.h"

class NBlock;

class Parser
{
public:
    Parser(const std::string &filename);

    void parse(NBlock *rootBlock, bool declarationsOnly);

private:
    Token nextToken(Token::Type expected);
    inline Token nextToken() { return m_lexer.nextToken(); }
    void checkTokenType(const Token &tok, Token::Type expected) const;
    void parseExtern();
    NFunctionDeclaration *parseFunc();
    NBlock *parseBlock();
    void parseStatements();
    void parseReturn();
    std::unique_ptr<NExpression> parseBinOp(std::unique_ptr<NExpression> lhs, int precedence);
    std::unique_ptr<NExpression> parsePrimary(NExpression *context);
    std::unique_ptr<NExpression> parseExpression(NExpression *context = nullptr);
    void parseLet();
    void parseStruct();
    void parseIface();
    std::vector<Type> parseParameterList();
    void parseImpl();
    void parseExpressionStatement();
    Type parseType();
    void parseIf();
    void parseWhile();
    void parseImport();

    std::string m_filename;
    Lexer m_lexer;
    NBlock *m_block;
    bool m_declarationsOnly;
};

#endif
