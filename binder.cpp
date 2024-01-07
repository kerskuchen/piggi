#include "definitions.hpp"

struct Binder {
    Source source;
    ASTNode* syntaxTree;
};

fun Binder BinderCreate(Source source, ASTNode* syntaxTree) {
    let Binder result;
    result.source = source;
    result.syntaxTree = syntaxTree;
    return result;
}

fun ASTNode* BinderBindTree(Binder* binder) {
    return binder->syntaxTree;
}