
#ifndef STAGE_H
#define STAGE_H

#include <string>

class Type;
class Node;

class Stage
{
public:
    Stage() {}
    virtual ~Stage() {}

    virtual int typeSize(const Type &t) = 0;
    virtual bool isFunctionDefined(const std::string &name) const = 0;

    enum class InjectScope {
        Local,
        Global
    };
    virtual void inject(Node *node, InjectScope scope) = 0;
};

#endif
