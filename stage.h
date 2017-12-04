
#ifndef STAGE_H
#define STAGE_H

#include <string>
#include <vector>

class Type;
class Node;

class Stage
{
public:
    Stage() {}
    virtual ~Stage() {}

    virtual bool isFunctionDefined(const std::string &name, const std::vector<Type> &args) const = 0;

    enum class InjectScope {
        Local,
        Global
    };
    virtual void inject(Node *node, InjectScope scope) = 0;
};

#endif
