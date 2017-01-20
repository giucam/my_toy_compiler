
#include <unistd.h>
#include <dirent.h>
#include <ext/stdio_filebuf.h>

#include <memory>

#include "cparser.h"
#include "lexer.h"
#include "common.h"
#include "node.h"
#include "cparser.h"

static std::string findFile(const std::string &file)
{
    if (file[0] == '/' || file[0] == '.') {
        return file;
    }

    std::string includeDir = "/usr/include";
    DIR *d = opendir(includeDir.c_str());
    dirent *dir;
    if (d) {
        while ((dir = readdir(d))) {
            if (file == dir->d_name) {
                return includeDir + '/' + dir->d_name;
            }
        }

        closedir(d);
    }

    if (access(file.c_str(), F_OK) != 0) {
        error("cannot open file '{}' for reading", file);
    }
    return file;
}

CParser::CParser(const std::string &filename)
       : m_filename(findFile(filename))
{
}

static std::string getCString(CXString s)
{
    std::string str = clang_getCString(s);
    clang_disposeString(s);
    return str;
}

Type CParser::parseType(CXType type)
{
    int pointer = 0;
    while (type.kind == CXType_Pointer) {
        ++pointer;
        type = clang_getPointeeType(type);
    }

    auto getPointer = [&](Type t) {
        for (int i = 0; i < pointer; ++i) {
            t = t.getPointerTo();
        }
        return t;
    };

    auto it = m_types.find(type);
    if (it != m_types.end()) {
        return getPointer(it->second);
    }

    auto spelling = clang_getTypeSpelling(type);
    std::string str = getCString(spelling);

    switch (type.kind) {
        case CXType_Void:
            if (pointer == 0) {
                return VoidType();
            } else {
                return getPointer(IntegerType(true, 8));
            }
        case CXType_Char_U:
        case CXType_UChar:
        case CXType_UShort:
        case CXType_UInt:
        case CXType_ULong:
            return getPointer(IntegerType(false, clang_Type_getSizeOf(type) * 8));
        case CXType_Char_S:
        case CXType_SChar:
        case CXType_Short:
        case CXType_Int:
        case CXType_Long:
            return getPointer(IntegerType(true, clang_Type_getSizeOf(type) * 8));
        case CXType_FunctionProto: {
            auto ret = parseType(clang_getResultType(type));
            std::vector<Type> args;
            for (int i = 0; i < clang_getNumArgTypes(type); ++i) {
                auto t = parseType(clang_getArgType(type, i));
                args.push_back(t);
            }

            FunctionPointerType t(ret, args);
            return getPointer(t);
        }
        case CXType_ConstantArray: {
            auto eltype = parseType(clang_getElementType(type));
            auto n = clang_getNumElements(type);
            ArrayType t(eltype, n);
            return getPointer(t);
        }
        case CXType_IncompleteArray: {
            auto t = parseType(clang_getElementType(type)).getPointerTo();
            return getPointer(t);
        }
        default:
            break;
    }

    error("C: unknown type '{}', kind '{}'", str, type.kind);
    return VoidType();
}

void CParser::parseStruct(CXCursor c)
{
    CXType type = clang_getCursorType(c);
    CXString str = clang_getTypeSpelling(type);
    CXString str1 = clang_getCursorSpelling(c);
    std::string signature = getCString(str);
    std::string name = getCString(str1);

    if (m_types.find(type) != m_types.end()) {
        return;
    }

    auto loc = clang_getCursorLocation(c);
    CXFile file;
    unsigned line, column, offset;
    clang_getSpellingLocation(loc, &file, &line, &column, &offset);

    auto decl = new NStructDeclaration(name);
    m_types.insert(std::make_pair(type, decl->type()));

    struct ParseContext {
        CParser *parser;
        std::vector<NStructDeclaration::Field> list;
    } context = { this, {} };

    clang_visitChildren(c, [](CXCursor c, CXCursor parent, CXClientData client_data) {
        auto *ctx = static_cast<ParseContext *>(client_data);
        auto kind = clang_getCursorKind(c);
        if (kind == CXCursor_UnionDecl) {
            ctx->parser->parseUnion(c);
        } else if (kind == CXCursor_StructDecl) {
            ctx->parser->parseStruct(c);
        } else if (kind == CXCursor_FieldDecl) {
            auto name = getCString(clang_getCursorSpelling(c));
            auto decl = clang_getCursorDefinition(c);
            auto ctype = clang_getCursorType(decl);
            auto type = ctx->parser->parseType(ctype);

            ctx->list.push_back({ name, type, true });
        } else {
            return CXChildVisit_Recurse;
        }
        return CXChildVisit_Continue;
    }, &context);

    decl->setFields(context.list);
    m_block->statements.push_back(decl);
}

void CParser::parseUnion(CXCursor c)
{
    CXType type = clang_getCursorType(c);
    CXString str = clang_getTypeSpelling(type);
    CXString str1 = clang_getCursorSpelling(c);
    std::string signature = getCString(str);
    std::string name = getCString(str1);

    struct ParseContext {
        CParser *parser;
        std::vector<NUnionDeclaration::Field> list;
    } context = { this, {} };

    clang_visitChildren(c, [](CXCursor c, CXCursor parent, CXClientData client_data) {
        auto *ctx = static_cast<ParseContext *>(client_data);
        auto kind = clang_getCursorKind(c);
        if (kind == CXCursor_UnionDecl) {
            ctx->parser->parseUnion(c);
        } else if (kind == CXCursor_StructDecl) {
            ctx->parser->parseStruct(c);
        } else if (kind == CXCursor_FieldDecl) {
            auto ctype = clang_getCursorType(c);
            auto name = getCString(clang_getCursorSpelling(c));
            auto type = ctx->parser->parseType(ctype);

            ctx->list.push_back({ name, type, true });
        } else {
            return CXChildVisit_Recurse;
        }
        return CXChildVisit_Continue;
    }, &context);

    auto decl = new NUnionDeclaration(name, context.list);
    m_types.insert(std::make_pair(type, decl->type()));
    m_block->statements.push_back(decl);
}

void CParser::parseFunction(CXCursor c)
{
    CXType type = clang_getCursorType(c);
    CXString str = clang_getTypeSpelling(type);
    CXString str1 = clang_getCursorSpelling(c);
    std::string signature = getCString(str);
    std::string name = getCString(str1);

    auto retType = parseType(clang_getCursorResultType(c));

    std::vector<NFunctionArgumentDeclaration> list;

    auto functype = clang_getCursorType(c);
    for (int i = 0; i < clang_getNumArgTypes(functype); ++i) {
        auto t = clang_getArgType(functype, i);
        auto type = parseType(t);
        std::string name = "arg";

        list.emplace_back(Token(), name + std::to_string(i), type, true);
    }

    bool variadic = clang_isFunctionTypeVariadic(functype);

    auto decl = new NExternDeclaration(name, retType, list, variadic);
    m_block->statements.push_back(decl);
}

void CParser::parseTypedef(CXCursor c)
{
    CXType type = clang_getCursorType(c);
    auto underlying = clang_getTypedefDeclUnderlyingType(c);
    auto utype = parseType(underlying);

    m_types.insert(std::make_pair(type, utype));
}

void CParser::parseVariable(CXCursor c)
{
    CXType type = clang_getCursorType(c);
    CXString str1 = clang_getCursorSpelling(c);
    std::string name = getCString(str1);

    auto var = new NExternVariableDeclaration(Token(), name, parseType(type));
    m_block->statements.push_back(var);
}

void CParser::parse(NBlock *root)
{
    m_block = root;

    CXIndex index = clang_createIndex(0, 0);
    CXTranslationUnit unit = clang_parseTranslationUnit(index, m_filename.c_str(), nullptr, 0, nullptr, 0, CXTranslationUnit_None);
    if (!unit) {
        error("unable to parse translation unit");
    }

    CXCursor cursor = clang_getTranslationUnitCursor(unit);
    clang_visitChildren(cursor, [](CXCursor c, CXCursor parent, CXClientData client_data) {
        CParser *parser = static_cast<CParser *>(client_data);

        switch (clang_getCursorKind(c)) {
            case CXCursor_FunctionDecl: {
                parser->parseFunction(c);
                return CXChildVisit_Continue;
            }
            case CXCursor_TypedefDecl:
                parser->parseTypedef(c);
                return CXChildVisit_Continue;
            case CXCursor_StructDecl:
                parser->parseStruct(c);
                return CXChildVisit_Continue;
            case CXCursor_UnionDecl:
                parser->parseUnion(c);
                return CXChildVisit_Continue;
            case CXCursor_VarDecl:
                parser->parseVariable(c);
                return CXChildVisit_Continue;
            default:
                break;
        }

        return CXChildVisit_Recurse;
    }, this);

    clang_disposeTranslationUnit(unit);
    clang_disposeIndex(index);
}
