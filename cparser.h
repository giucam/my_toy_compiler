
#ifndef CPARSER_H
#define CPARSER_H

#include <unordered_map>
#include <memory>

#include <clang-c/Index.h>

#include "node.h"

class NBlock;

namespace std {
template<>
struct hash<CXType> {
    std::size_t operator()(CXType t) const
    {
        return clang_hashCursor(clang_getTypeDeclaration(t));
    }
};
}

inline bool operator==(CXType t1, CXType t2)
{
    return clang_equalCursors(clang_getTypeDeclaration(t1), clang_getTypeDeclaration(t2));
}

class CParser
{
public:
    CParser(const std::string &filename);

    void parse(NBlock *rootBlock);

private:
    Type parseType(CXType type);
    void parseStruct(CXCursor c);
    void parseUnion(CXCursor c);
    void parseFunction(CXCursor c);
    void parseTypedef(CXCursor c);
    void parseVariable(CXCursor c);

    std::string m_filename;
    NBlock *m_block;
    std::unordered_map<CXType, Type> m_types;
};


#endif
