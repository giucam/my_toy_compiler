
#include <unistd.h>
#include <dirent.h>
#include <ext/stdio_filebuf.h>

#include <memory>

#include "cparser.h"
#include "lexer.h"
#include "common.h"
#include "node.h"
#include "cparser.h"

static bool isInDir(const std::string &dirName, const std::string &entry)
{
    DIR *d = opendir(dirName.c_str());
    dirent *dir;
    if (d) {
        while ((dir = readdir(d))) {
            if (entry == dir->d_name) {
                return true;
            }
        }

        closedir(d);
    }
    return false;
}

static std::string findFile(const std::string &file)
{
    if (file[0] == '/' || file[0] == '.') {
        return file;
    }

    std::string includeDir = "/usr/include";
    size_t start = 0;
    std::string found = includeDir;
    while (start < file.size()) {
        size_t end = file.find('/', start);
        if (end == std::string::npos) {
            end = file.size();
        }
        auto substr = file.substr(start, end - start);
        if (!isInDir(found, substr)) {
            fmt::print("not found\n");
            break;
        }

        found += "/" + substr;
        start = end + 1;
    }

    if (access(found.c_str(), F_OK) != 0) {
        error("cannot open file '{}' for reading", file);
    }
    return found;
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

static Token getToken(CXCursor c)
{
    auto loc = clang_getCursorLocation(c);
    CXFile file;
    unsigned linenum, column, offset;
    auto text = getCString(clang_getCursorSpelling(c));
    clang_getSpellingLocation(loc, &file, &linenum, &column, &offset);
    auto filename = getCString(clang_getFileName(file));

    std::ifstream stream(filename);
    std::string line;
    unsigned l = 1;
    while (stream.good() && l <= linenum) {
        while (stream.good()) {
            int c = stream.get();
            if (c == '\n') {
                l++;
                break;
            }
            if (l == linenum) {
                line += c;
            }
        }
    }

    return Token(Token::Type::Unknown, linenum, column, filename, text, line);
}

Type CParser::parseType(CXType type, CXCursor cursor)
{
    int pointer = 0;
    while (type.kind == CXType_Pointer) {
        ++pointer;
        type = clang_getPointeeType(type);
    }

    auto getPointer = [&](Type t) {
        if (t.getSpecialization<VoidType>()) {
            t = IntegerType(true, 8);
        }
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

    if (type.kind == CXType_Unexposed) {
        type = clang_getCanonicalType(type);
    }

    switch (type.kind) {
        case CXType_Void:
            return getPointer(VoidType());
        case CXType_Char_U:
        case CXType_UChar:
        case CXType_UShort:
        case CXType_UInt:
        case CXType_ULong:
        case CXType_ULongLong:
            return getPointer(IntegerType(false, clang_Type_getSizeOf(type) * 8));
        case CXType_Char_S:
        case CXType_SChar:
        case CXType_Short:
        case CXType_Int:
        case CXType_Long:
        case CXType_LongLong:
            return getPointer(IntegerType(true, clang_Type_getSizeOf(type) * 8));
        case CXType_Float:
        case CXType_Double:
        case CXType_LongDouble:
            return getPointer(FloatingType(clang_Type_getSizeOf(type) * 8));
        case CXType_FunctionProto: {
            auto ret = parseType(clang_getResultType(type), cursor);
            std::vector<Type> args;
            for (int i = 0; i < clang_getNumArgTypes(type); ++i) {
                auto t = parseType(clang_getArgType(type, i), cursor);
                args.push_back(t);
            }

            if (pointer == 0) pointer++;
            FunctionPointerType t(ret, args);
            return getPointer(t);
        }
        case CXType_ConstantArray: {
            auto eltype = parseType(clang_getElementType(type), cursor);
            auto n = clang_getNumElements(type);
            ArrayType t(eltype, n);
            return getPointer(t);
        }
        case CXType_IncompleteArray: {
            auto t = parseType(clang_getElementType(type), cursor).getPointerTo();
            return getPointer(t);
        }
        default:
            break;
    }

    err(getToken(cursor), "C: unknown type '{}', kind '{}'", str, type.kind);
    return VoidType();
}

static bool isForwardDeclaration(CXCursor cursor)
{
    return clang_equalCursors(clang_getCursorDefinition(cursor), clang_getNullCursor());
}

void CParser::parseStruct(CXCursor c)
{
    CXType type = clang_getCursorType(c);
    CXString str = clang_getTypeSpelling(type);
    CXString str1 = clang_getCursorSpelling(c);
    std::string signature = getCString(str);
    std::string name = getCString(str1);

    auto decl = [&]() {
        auto it = m_structs.find(type);
        if (it != m_structs.end()) {
            return it->second;
        }
        auto decl = new NStructDeclaration(name.empty() ? signature : name);
        m_block->statements.push_back(decl);
        m_types.insert(std::make_pair(type, decl->type()));
        m_structs.insert(std::make_pair(type, decl));
        return decl;
    }();

    if (!isForwardDeclaration(c)) {
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
                auto type = ctx->parser->parseType(ctype, c);

                ctx->list.push_back({ name, type, true });
            } else {
                return CXChildVisit_Recurse;
            }
            return CXChildVisit_Continue;
        }, &context);

        decl->setFields(context.list);
    }
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
            auto type = ctx->parser->parseType(ctype, c);

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

    auto retType = parseType(clang_getCursorResultType(c), c);

    std::vector<NFunctionArgumentDeclaration> list;

    auto functype = clang_getCursorType(c);
    for (int i = 0; i < clang_getNumArgTypes(functype); ++i) {
        auto t = clang_getArgType(functype, i);
        auto type = parseType(t, c);
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
    auto utype = parseType(underlying, c);

    m_types.insert(std::make_pair(type, utype));
}

void CParser::parseVariable(CXCursor c)
{
    CXType type = clang_getCursorType(c);
    CXString str1 = clang_getCursorSpelling(c);
    std::string name = getCString(str1);

    auto var = new NExternVariableDeclaration(Token(), name, parseType(type, c));
    m_block->statements.push_back(var);
}

void CParser::parseEnum(CXCursor c)
{
    CXType type = clang_getCursorType(c);
    auto underlying = clang_getEnumDeclIntegerType(c);
    auto utype = parseType(underlying, c);

    m_types.insert(std::make_pair(type, utype));
}

std::vector<std::string> CParser::parse(NBlock *root)
{
    m_block = root;

    CXIndex index = clang_createIndex(0, 0);
    CXTranslationUnit unit;
    auto err = clang_parseTranslationUnit2(index, m_filename.c_str(), nullptr, 0, nullptr, 0, CXTranslationUnit_SkipFunctionBodies, &unit);
    if (err != CXError_Success) {
        error("unable to parse translation unit '{}', {}", m_filename, err);
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
            case CXCursor_EnumDecl:
                parser->parseEnum(c);
                return CXChildVisit_Continue;
            case CXCursor_InclusionDirective:
            case CXCursor_MacroDefinition: {
                fmt::print("MACRO {}\n",getCString(clang_getCursorSpelling(c)));
                abort();
                }
            default:
                break;
        }

        return CXChildVisit_Recurse;
    }, this);

    std::vector<std::string> includedFiles;

    clang_getInclusions(unit, [](CXFile file, CXSourceLocation *stack, unsigned len, CXClientData data) {
        auto filename = getCString(clang_getFileName(file));
        static_cast<std::vector<std::string> *>(data)->push_back(filename);
    }, &includedFiles);

    clang_disposeTranslationUnit(unit);
    clang_disposeIndex(index);

    return includedFiles;
}
