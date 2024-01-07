#include "definitions.hpp"

struct Emitter {
    FILE* outFile;
    int indentationLevel;
};

fun Emitter EmitterCreate(String emitFilepath) {
    let FILE* outFile = fopen(emitFilepath.cstr, "w");
    if (outFile == nullptr) {
        fprintf(stderr, "Cannot open file for writing: '%s'", emitFilepath.cstr);
        exit(1);
    }

    let Emitter result;
    result.outFile = outFile;
    result.indentationLevel = 0;
    return result;
}

fun void EmitNewLine(Emitter* emitter)
{ 
    fprintf(emitter->outFile, "\n");
    for (let int index = 0; index < emitter->indentationLevel; index += 1) {
        fprintf(emitter->outFile, "    ");
    }
} 

fun void EmitType(Emitter* emitter, Type type);
fun void EmitExpression(Emitter* emitter, ASTNode* node);
fun void EmitStatement(Emitter* emitter, ASTNode* node);

fun void EmitCastExpression(Emitter* emitter, ASTNode* node) {
    fprintf(emitter->outFile, "(");
    EmitType(emitter, node->type);
    fprintf(emitter->outFile, ")");
    EmitExpression(emitter, node->left);
}

fun void EmitSizeOfExpression(Emitter* emitter, ASTNode* node) {
    fprintf(emitter->outFile, "sizeof(");
    EmitType(emitter, node->left->type);
    fprintf(emitter->outFile, ")");
}

fun void EmitParenthesizedExpression(Emitter* emitter, ASTNode* node) {
    fprintf(emitter->outFile, "(");
    EmitExpression(emitter, node->left);
    fprintf(emitter->outFile, ")");
}

fun void EmitUnaryExpression(Emitter* emitter, ASTNode* node) {
    if (node->kind == ASTNodeKind::Identity) {
        EmitExpression(emitter, node->left);
        return;
    }

    switch (node->kind) {
        case ASTNodeKind::Negation:
            fprintf(emitter->outFile, "-");
            break;
        case ASTNodeKind::LogicalNegation:
            fprintf(emitter->outFile, "!");
            break;
        case ASTNodeKind::BitwiseNegation:
            fprintf(emitter->outFile, "~");
            break;
        case ASTNodeKind::Address:
            fprintf(emitter->outFile, "&");
            break;
        case ASTNodeKind::Dereference:
            fprintf(emitter->outFile, "*");
            break;
        
        default:
            fprintf(stdout, "Unexpected unary operation kind in emitter %d", node->kind);
            exit(1);
    }

    EmitExpression(emitter, node->left);
}

fun void EmitBinaryExpression(Emitter* emitter, ASTNode* node) {
    EmitExpression(emitter, node->left);
    fprintf(emitter->outFile, " %s ", TokenKindToString(node->token.kind).cstr);
    EmitExpression(emitter, node->right);
}

fun void EmitTernaryConditionalExpression(Emitter* emitter, ASTNode* node) {
    EmitExpression(emitter, node->left);
    fprintf(emitter->outFile, " ? ");
    EmitExpression(emitter, node->right);
    fprintf(emitter->outFile, " : ");
    EmitExpression(emitter, node->extra1);
}

fun void EmitEnumLiteralSymbol(Emitter* emitter, Symbol* enumLiteral) {
    fprintf(emitter->outFile, "%s_%s", enumLiteral->type.name.cstr, enumLiteral->name.cstr);
}

fun void EmitBoolLiteral(Emitter* emitter, ASTNode* node) {
    if (node->intvalue == 0)
        fprintf(emitter->outFile, "false");
    else 
        fprintf(emitter->outFile, "true");
}

fun void EmitEnumLiteral(Emitter* emitter, ASTNode* node) {
    EmitEnumLiteralSymbol(emitter, node->symbol);
}

fun void EmitCharacterLiteral(Emitter* emitter, ASTNode* node) {
    fprintf(emitter->outFile, "'%s'", node->stringvalue.cstr);
}

fun void EmitIntegerLiteral(Emitter* emitter, ASTNode* node) {
    assert(node->token.kind == SyntaxKind::IntegerLiteralToken);
    if (node->token.intvalueIsHex)
        fprintf(emitter->outFile, "%s", TokenGetText(node->token).cstr);
    else
        fprintf(emitter->outFile, "%lld", node->intvalue);
}

fun void EmitStringLiteral(Emitter* emitter, ASTNode* node) {
    fprintf(emitter->outFile, "\"%s\"", node->stringvalue.cstr);
}

fun void EmitArrayLiteral(Emitter* emitter, ASTNode* node) {
    fprintf(emitter->outFile, "{ ");
    for (let int index = 0; index < node->children.count; index += 1) {
        EmitExpression(emitter, node->children.nodes[index]);
        if (index != node->children.count - 1) 
            fprintf(emitter->outFile, ", ");
    }
    fprintf(emitter->outFile, " }");
}

fun void EmitNullLiteral(Emitter* emitter, ASTNode* node) {
    fprintf(emitter->outFile, "NULL");
}

fun void EmitIdentifier(Emitter* emitter, ASTNode* node) {
    let String name = node->symbol->name;
    fprintf(emitter->outFile, "%s", name.cstr);
}

fun void EmitFunctionCallExpression(Emitter* emitter, ASTNode* node) {
    let String funcName = node->symbol->name;
    fprintf(emitter->outFile, "%s(", funcName.cstr);
    for (let int index = 0; index < node->children.count; index += 1) {
        EmitExpression(emitter, node->children.nodes[index]);
        if (index != node->children.count - 1)
            fprintf(emitter->outFile, ", ");
    }
    fprintf(emitter->outFile, ")");
}

fun void EmitArrayIndexingExpression(Emitter* emitter, ASTNode* node) {
    EmitExpression(emitter, node->left);
    fprintf(emitter->outFile, "[");
    EmitExpression(emitter, node->right);
    fprintf(emitter->outFile, "]");
}

fun void EmitMemberAccessExpression(Emitter* emitter, ASTNode* node) {
    EmitExpression(emitter, node->left);
    if (IsPointerType(node->left->type))
        fprintf(emitter->outFile, "->");
    else
        fprintf(emitter->outFile, ".");
    let String memberName = node->symbol->name;
    fprintf(emitter->outFile, "%s", memberName.cstr);
}


fun void EmitExpression(Emitter* emitter, ASTNode* node) {
    switch (node->kind) {
        case ASTNodeKind::CastExpression:
            EmitCastExpression(emitter, node);
            break;
        case ASTNodeKind::SizeOfExpression:
            EmitSizeOfExpression(emitter, node);
            break;
        case ASTNodeKind::ParenthesizedExpression:
            EmitParenthesizedExpression(emitter, node);
            break;

        case ASTNodeKind::Identity:
        case ASTNodeKind::Negation:
        case ASTNodeKind::LogicalNegation:
        case ASTNodeKind::BitwiseNegation:
        case ASTNodeKind::Address:
        case ASTNodeKind::Dereference:
            EmitUnaryExpression(emitter, node);
            break;

        case ASTNodeKind::Assignment:
        case ASTNodeKind::Add:
        case ASTNodeKind::AddAssignment:
        case ASTNodeKind::Subtract:
        case ASTNodeKind::SubtractAssignment:
        case ASTNodeKind::Multiply:
        case ASTNodeKind::MultiplyAssignment:
        case ASTNodeKind::Divide:
        case ASTNodeKind::DivideAssignment:
        case ASTNodeKind::Remainder:
        case ASTNodeKind::RemainderAssignment:
        case ASTNodeKind::Equals:
        case ASTNodeKind::NotEquals:
        case ASTNodeKind::Less:
        case ASTNodeKind::LessEquals:
        case ASTNodeKind::Greater:
        case ASTNodeKind::GreaterEquals:
        case ASTNodeKind::AddToPointer:
        case ASTNodeKind::AddToPointerAssignment:
        case ASTNodeKind::SubtractFromPointer:
        case ASTNodeKind::SubtractFromPointerAssignment:
        case ASTNodeKind::DistanceBetweenPointers:
        case ASTNodeKind::BitshiftLeft:
        case ASTNodeKind::BitshiftLeftAssignment:
        case ASTNodeKind::BitshiftRight:
        case ASTNodeKind::BitshiftRightAssignment:
        case ASTNodeKind::BitwiseAnd:
        case ASTNodeKind::BitwiseAndAssignment:
        case ASTNodeKind::BitwiseOr:
        case ASTNodeKind::BitwiseOrAssignment:
        case ASTNodeKind::BitwiseXor:
        case ASTNodeKind::BitwiseXorAssignment:
        case ASTNodeKind::LogicalAnd:
        case ASTNodeKind::LogicalOr:
            EmitBinaryExpression(emitter, node);
            break;

        case ASTNodeKind::TernaryConditionalExpression:
            EmitTernaryConditionalExpression(emitter, node);
            break;

        case ASTNodeKind::Identifier:
            EmitIdentifier(emitter, node);
            break;
        case ASTNodeKind::BoolLiteral:
            EmitBoolLiteral(emitter, node);
            break;
        case ASTNodeKind::CharacterLiteral:
            EmitCharacterLiteral(emitter, node);
            break;
        case ASTNodeKind::IntegerLiteral:
            EmitIntegerLiteral(emitter, node);
            break;
        case ASTNodeKind::StringLiteral:
            EmitStringLiteral(emitter, node);
            break;
        case ASTNodeKind::EnumValueLiteral:
            EmitEnumLiteral(emitter, node);
            break;
        case ASTNodeKind::ArrayLiteral:
            EmitArrayLiteral(emitter, node);
            break;
        case ASTNodeKind::NullLiteral:
            EmitNullLiteral(emitter, node);
            break;

        case ASTNodeKind::Arrayindexing:
            EmitArrayIndexingExpression(emitter, node);
            break;
        case ASTNodeKind::Memberaccess:
            EmitMemberAccessExpression(emitter, node);
            break;
        case ASTNodeKind::FunccallExpression:
            EmitFunctionCallExpression(emitter, node);
            break;

        default:
            fprintf(stderr, "Unknown expression in emitter: %d\n", node->kind);
            exit(1);
    }
}

fun void EmitExpressionStatement(Emitter* emitter, ASTNode* node) {
    EmitExpression(emitter, node->left);
    fprintf(emitter->outFile, ";");
}

fun void EmitIfStatement(Emitter* emitter, ASTNode* node) {
    fprintf(emitter->outFile, "if (");
    EmitExpression(emitter, node->left);
    fprintf(emitter->outFile, ") ");

    EmitStatement(emitter, node->right);

    if (node->extra1 != nullptr) {
        fprintf(emitter->outFile, " else ");
        EmitStatement(emitter, node->extra1);
    }
}

fun void EmitDoWhileStatement(Emitter* emitter, ASTNode* node) {
    fprintf(emitter->outFile, "do ");
    EmitStatement(emitter, node->right);
    fprintf(emitter->outFile, " while (");
    EmitExpression(emitter, node->left);
    fprintf(emitter->outFile, ");");
}

fun void EmitWhileStatement(Emitter* emitter, ASTNode* node) {
    fprintf(emitter->outFile, "while (");
    EmitExpression(emitter, node->left);
    fprintf(emitter->outFile, ") ");

    EmitStatement(emitter, node->right);
}

fun void EmitForStatement(Emitter* emitter, ASTNode* node) {
    fprintf(emitter->outFile, "for (");
    EmitStatement(emitter, node->left);
    fprintf(emitter->outFile, " ");
    EmitStatement(emitter, node->right);
    fprintf(emitter->outFile, " ");
    EmitExpression(emitter, node->extra1);
    fprintf(emitter->outFile, ") ");

    EmitStatement(emitter, node->extra2);
}

fun void EmitReturnStatement(Emitter* emitter, ASTNode* node) {
    if (node->left == nullptr) {
        fprintf(emitter->outFile, "return;");
    } else {
        fprintf(emitter->outFile, "return ");
        EmitExpression(emitter, node->left);
        fprintf(emitter->outFile, ";");
    }
}

fun void EmitBreakStatement(Emitter* emitter, ASTNode* node) {
    fprintf(emitter->outFile, "break;");
}

fun void EmitContinueStatement(Emitter* emitter, ASTNode* node) {
    fprintf(emitter->outFile, "continue;");
}

fun void EmitCaseStatement(Emitter* emitter, ASTNode* node) {
    if (node->kind == ASTNodeKind::DefaultStatement) {
        fprintf(emitter->outFile, "default: ");
        if (node->left != nullptr)
            EmitStatement(emitter, node->left);
        else
            fprintf(emitter->outFile, "// Fallthrough");
    } else {
        fprintf(emitter->outFile, "case ");
        EmitExpression(emitter, node->right);
        fprintf(emitter->outFile, ": ");
        if (node->left != nullptr)
            EmitStatement(emitter, node->left);
        else
            fprintf(emitter->outFile, "// Fallthrough");
    }
}

fun void EmitSwitchStatement(Emitter* emitter, ASTNode* node) {
    fprintf(emitter->outFile, "switch (");
    EmitExpression(emitter, node->left);
    fprintf(emitter->outFile, ") {");
    emitter->indentationLevel += 1;
    EmitNewLine(emitter);

    for (let int index = 0; index < node->children.count; index += 1) {
        EmitCaseStatement(emitter, node->children.nodes[index]);
        if (index == node->children.count - 1)
            emitter->indentationLevel -= 1;
        EmitNewLine(emitter);
    }

    fprintf(emitter->outFile, "}");
}


fun void EmitType(Emitter* emitter, Type type) {
    switch (type.kind){
        case TypeKind::PrimitiveVoid:
            fprintf(emitter->outFile, "void");
            break;
        case TypeKind::PrimitiveBool:
            fprintf(emitter->outFile, "bool");
            break;
        case TypeKind::PrimitiveChar:
            fprintf(emitter->outFile, "char");
            break;
        case TypeKind::PrimitiveByte:
            fprintf(emitter->outFile, "int8");
            break;
        case TypeKind::PrimitiveShort:
            fprintf(emitter->outFile, "int16");
            break;
        case TypeKind::PrimitiveInt:
            fprintf(emitter->outFile, "int32");
            break;
        case TypeKind::PrimitiveLong:
            fprintf(emitter->outFile, "int64");
            break;
        case TypeKind::PrimitiveCString:
            fprintf(emitter->outFile, "const char*");
            break;
        case TypeKind::Struct:
        case TypeKind::Union:
        case TypeKind::Enum:
            fprintf(emitter->outFile, "%s", type.name.cstr);
            break;
        default:
            assert(false && "Unexpected type in emitter");
    }
    for (let int index = 0; index < type.baseIndirectionLevel; index += 1){
        fprintf(emitter->outFile, "*");
    }
}

fun void EmitVariableDeclarationWithoutTerminator(Emitter* emitter, Symbol* varSymbol) {
    let Type varType = varSymbol->type;
    EmitType(emitter, varType);

    let String varName = varSymbol->name;
    fprintf(emitter->outFile, " %s", varName.cstr);
    if (varSymbol->type.isArray) {
        fprintf(emitter->outFile, "[%lld]", varSymbol->type.arrayElementCount);
    }
}

fun void EmitVariableDeclarationStatement(Emitter* emitter, ASTNode* node) {
    if (node->symbol->scopeKind == SymbolScopeKind::Extern)
        return; // We don't need to declare externals because we emit them anyway as a first thing
    else if (node->symbol->scopeKind == SymbolScopeKind::LocalPersist)
        fprintf(emitter->outFile, "static ");
    else if (node->symbol->scopeKind == SymbolScopeKind::Global)
        fprintf(emitter->outFile, "static ");

    EmitVariableDeclarationWithoutTerminator(emitter, node->symbol);
    if (node->left != nullptr) {
        // Optionals initializer
        fprintf(emitter->outFile, " = ");
        EmitExpression(emitter, node->left);
    }
    fprintf(emitter->outFile, ";");
}

fun void EmitCompoundStatement(Emitter* emitter, ASTNode* node) {
    if (node->children.count == 0) {
        fprintf(emitter->outFile, "{}");
    } else {
        fprintf(emitter->outFile, "{");
        emitter->indentationLevel += 1;
        EmitNewLine(emitter);

        for (let int index = 0; index < node->children.count; index += 1) {
            EmitStatement(emitter, node->children.nodes[index]);
            if (index == node->children.count - 1)
                emitter->indentationLevel -= 1;
            EmitNewLine(emitter);
        }

        fprintf(emitter->outFile, "}");
    }
}

fun void EmitEnumDeclarationStatement(Emitter* emitter, ASTNode* node) {
    if (node->symbol->scopeKind == SymbolScopeKind::Extern)
        return; // We don't need to declare externals because we emit them anyway as a first thing

    let String enumName = node->symbol->name;
    fprintf(emitter->outFile, "enum %s; ", enumName.cstr);
    fprintf(emitter->outFile, "typedef enum %s %s;", enumName.cstr, enumName.cstr);
}

fun void EmitEnumDefinitionStatement(Emitter* emitter, ASTNode* node) {
    if (node->symbol->scopeKind == SymbolScopeKind::Extern)
        return; // We don't need to declare externals because we emit them anyway as a first thing

    EmitEnumDeclarationStatement(emitter, node);

    let String enumName = node->symbol->name;
    fprintf(emitter->outFile, " enum %s ", enumName.cstr);

    fprintf(emitter->outFile, "{");
    emitter->indentationLevel += 1;
    EmitNewLine(emitter);

    let int memberCount = node->symbol->membersSymbolTable->count;
    for (let int index = 0; index < memberCount; index += 1) {
        let Symbol* memberSymbol = node->symbol->membersSymbolTable->symbols[index];
        EmitEnumLiteralSymbol(emitter, memberSymbol);
        fprintf(emitter->outFile, " = %lld,", memberSymbol->enumValue);
        if (index == memberCount - 1)
            emitter->indentationLevel -= 1;
        EmitNewLine(emitter);
    }

    fprintf(emitter->outFile, "};");
}


fun void EmitStructOrUnionDeclarationStatement(Emitter* emitter, ASTNode* node, bool isUnion) {
    if (node->symbol->scopeKind == SymbolScopeKind::Extern)
        return; // We don't need to declare externals because we emit them anyway as a first thing

    let String structName = node->symbol->name;
    fprintf(emitter->outFile, isUnion ? "union %s; " : "struct %s; ", structName.cstr);
    fprintf(emitter->outFile, isUnion ? "typedef union %s %s;" : "typedef struct %s %s;", structName.cstr, structName.cstr);
}

fun void EmitStructOrUnionDefinitionStatement(Emitter* emitter, ASTNode* node, bool isUnion) {
    if (node->symbol->scopeKind == SymbolScopeKind::Extern)
        return; // We don't need to declare externals because we emit them anyway as a first thing

    EmitStructOrUnionDeclarationStatement(emitter, node, isUnion);

    let String structName = node->symbol->name;
    fprintf(emitter->outFile, isUnion ? " union %s" : " struct %s ", structName.cstr);

    fprintf(emitter->outFile, "{");
    emitter->indentationLevel += 1;
    EmitNewLine(emitter);

    let int memberCount = node->symbol->membersSymbolTable->count;
    for (let int index = 0; index < memberCount; index += 1) {
        EmitVariableDeclarationWithoutTerminator(emitter, node->symbol->membersSymbolTable->symbols[index]);
        fprintf(emitter->outFile, ";");

        if (index == memberCount - 1)
            emitter->indentationLevel -= 1;
        EmitNewLine(emitter);
    }

    fprintf(emitter->outFile, "};");
}

fun void EmitFunctionDeclarationWithoutTerminator(Emitter* emitter, ASTNode* node) {
    let Type returnType = node->symbol->type;
    EmitType(emitter, returnType);

    let String funcName = node->symbol->name;
    fprintf(emitter->outFile, " %s(", funcName.cstr);
    for (let int index = 0; index < node->symbol->membersSymbolTable->count; index += 1) {
        EmitVariableDeclarationWithoutTerminator(emitter, node->symbol->membersSymbolTable->symbols[index]);
        if (index != node->symbol->membersSymbolTable->count - 1)
                fprintf(emitter->outFile, ", ");
        if (node->symbol->isVariadric && index == node->symbol->membersSymbolTable->count - 1) 
            fprintf(emitter->outFile, ", ...");
    }
    fprintf(emitter->outFile, ")");
}

fun void EmitFunctionDeclarationStatement(Emitter* emitter, ASTNode* node) {
    if (node->symbol->scopeKind == SymbolScopeKind::Extern)
        return; // We don't need to declare externals because we emit them anyway as a first thing

    EmitFunctionDeclarationWithoutTerminator(emitter, node);
    fprintf(emitter->outFile, ";");
}

fun void EmitFunctionDefinitionStatement(Emitter* emitter, ASTNode* node) {
    fprintf(emitter->outFile, "static ");
    EmitFunctionDeclarationWithoutTerminator(emitter, node);
    fprintf(emitter->outFile, " ");
    EmitCompoundStatement(emitter, node->left);
}

fun void EmitPreamble(Emitter* emitter) {
    fprintf(emitter->outFile, "//////////////////////////////////////////////////////////////////////\n");
    fprintf(emitter->outFile, "// This file was generated by piggi and should not be modified by hand\n");
    fprintf(emitter->outFile, "//////////////////////////////////////////////////////////////////////\n");
    EmitNewLine(emitter);
    EmitNewLine(emitter);
    fprintf(emitter->outFile, "#include <stdio.h>");
    EmitNewLine(emitter);
    fprintf(emitter->outFile, "#include <stdarg.h>");
    EmitNewLine(emitter);
    fprintf(emitter->outFile, "#include <stdint.h>");
    EmitNewLine(emitter);
    fprintf(emitter->outFile, "#include <stdbool.h>");
    EmitNewLine(emitter);
    fprintf(emitter->outFile, "#include <stdlib.h>");
    EmitNewLine(emitter);
    fprintf(emitter->outFile, "#include <assert.h>");

    EmitNewLine(emitter);
    EmitNewLine(emitter);

    fprintf(emitter->outFile, "typedef int8_t  int8;");
    EmitNewLine(emitter);
    fprintf(emitter->outFile, "typedef int16_t int16;");
    EmitNewLine(emitter);
    fprintf(emitter->outFile, "typedef int32_t int32;");
    EmitNewLine(emitter);
    fprintf(emitter->outFile, "typedef int64_t int64;");
    EmitNewLine(emitter);
    EmitNewLine(emitter);
}

fun void EmitPostamble(Emitter* emitter) {
    fclose(emitter->outFile);
}

fun void EmitStatement(Emitter* emitter, ASTNode* node) {
    switch (node->kind) {
        case ASTNodeKind::IfStatement:
            EmitIfStatement(emitter, node);
            break;
        case ASTNodeKind::DoWhileStatement:
            EmitDoWhileStatement(emitter, node);
            break;
        case ASTNodeKind::WhileStatement:
            EmitWhileStatement(emitter, node);
            break;
        case ASTNodeKind::ForStatement:
            EmitForStatement(emitter, node);
            break;
        case ASTNodeKind::ReturnStatement:
            EmitReturnStatement(emitter, node);
            break;
        case ASTNodeKind::BreakStatement:
            EmitBreakStatement(emitter, node);
            break;
        case ASTNodeKind::ContinueStatement:
            EmitContinueStatement(emitter, node);
            break;
        case ASTNodeKind::SwitchStatement:
            EmitSwitchStatement(emitter, node);
            break;
        case ASTNodeKind::EnumDeclarationStatement:
            EmitEnumDeclarationStatement(emitter, node);
            break;
        case ASTNodeKind::EnumDefinitionStatement:
            EmitEnumDefinitionStatement(emitter, node);
            break;
        case ASTNodeKind::UnionDeclarationStatement:
            EmitStructOrUnionDeclarationStatement(emitter, node, true);
            break;
        case ASTNodeKind::UnionDefinitionStatement:
            EmitStructOrUnionDefinitionStatement(emitter, node, true);
            break;
        case ASTNodeKind::StructDeclarationStatement:
            EmitStructOrUnionDeclarationStatement(emitter, node, false);
            break;
        case ASTNodeKind::StructDefinitionStatement:
            EmitStructOrUnionDefinitionStatement(emitter, node, false);
            break;
        case ASTNodeKind::FunctionDeclarationStatement:
            EmitFunctionDeclarationStatement(emitter, node);
            break;
        case ASTNodeKind::FunctionDefinitionStatement:
            EmitFunctionDefinitionStatement(emitter, node);
            break;
        case ASTNodeKind::VariableDeclarationStatement:
        case ASTNodeKind::ArrayDeclarationStatement:
            EmitVariableDeclarationStatement(emitter, node);
            break;
        case ASTNodeKind::ExpressionStatement:
            EmitExpressionStatement(emitter, node);
            break;
        case ASTNodeKind::CompoundStatement:
            EmitCompoundStatement(emitter, node);
            break;

        default:
            fprintf(stderr, "Unknown statement in emitter: %d\n", node->kind);
            exit(1);
    }
}

fun void EmitRoot(Emitter* emitter, ASTNode* node) {
    assert(node->kind == ASTNodeKind::Root);

    EmitPreamble(emitter);
    for (let int index = 0; index < node->children.count; index += 1) {
        EmitStatement(emitter, node->children.nodes[index]);
        EmitNewLine(emitter);
        EmitNewLine(emitter);
    }
    EmitPostamble(emitter);
}