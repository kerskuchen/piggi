#include "definitions.hpp"
#if 0
import "definitions.hpp"
#endif

struct Binder {
    SymbolTable* symbolTable;
    ModuleStatementSyntax* tree;

    int loopLevel;
    int switchCaseLevel;
    Symbol* currentFunctionSymbol;
};

fun Binder BinderCreate(SymbolTable* symbolTable) {
    let Binder result;
    result.symbolTable = symbolTable;
    result.loopLevel = 0;
    result.switchCaseLevel = 0;
    result.currentFunctionSymbol = nullptr;
    return result;
}

fun ASTNode* BindStatement(Binder* binder, SyntaxNode* syntax);
fun ASTNode* BindExpression(Binder* binder, SyntaxNode* syntax);
fun ASTNode* BindVariableDefinitionStatement(Binder* binder, SyntaxNode* syntax, SymbolScopeKind symbolScopeKind);

fun ASTNode* _WrapInBlockStatementIfNecessary(Binder* binder, ASTNode* node) {
    if (node->kind != ASTNodeKind::BlockStatement) {
        let ASTNode* block = ASTNodeCreate2(ASTNodeKind::BlockStatement, binder->symbolTable, nullptr);
        ASTNodeArrayPush(&block->children, node);
        node = block;
    }
    return node;
}

// Turns {{{ a; {b c} d; e; }}} -> { a; {b c} d; e; }
fun ASTNode* _FlattenBlockStatementIfNecessary(Binder* binder, ASTNode* node) {
    if (node->kind == ASTNodeKind::BlockStatement) {
        if (node->children.count == 1 && node->children.nodes[0]->kind == ASTNodeKind::BlockStatement) {
            let ASTNode* result = node->children.nodes[0];
            result->symbolTable->parent = node->symbolTable->parent;
            return _FlattenBlockStatementIfNecessary(binder, result);
        }
    }
    return node;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Types and specials

fun Type BindType(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::TypeExpression);

    let Type type = TypeCreateVoid();
    let SyntaxToken primary = syntax->typeExpr.typeTokens.nodes[0]->token;
    switch (primary.kind) {
        case SyntaxKind::VoidKeyword:
            type.kind = TypeKind::PrimitiveVoid;
            break;
        case SyntaxKind::CharKeyword:
            type.kind = TypeKind::PrimitiveChar;
            break;
        case SyntaxKind::BoolKeyword:
            type.kind = TypeKind::PrimitiveBool;
            break;
        case SyntaxKind::ByteKeyword:
            type.kind = TypeKind::PrimitiveByte;
            break;
        case SyntaxKind::ShortKeyword:
            type.kind = TypeKind::PrimitiveShort;
            break;
        case SyntaxKind::IntKeyword:
            type.kind = TypeKind::PrimitiveInt;
            break;
        case SyntaxKind::LongKeyword:
            type.kind = TypeKind::PrimitiveLong;
            break;
        case SyntaxKind::CStringKeyword:
            type.kind = TypeKind::PrimitiveCString;
            break;
        case SyntaxKind::IdentifierToken: {
                let String name = TokenGetText(primary);
                let Symbol* symbol = GetSymbol(binder->symbolTable, name);
                if (symbol != nullptr) {
                    if (symbol->kind == SymbolKind::Struct) {
                        type.kind = TypeKind::Struct;
                        type.name = name;
                        break;
                    } else if (symbol->kind == SymbolKind::Union) {
                        type.kind = TypeKind::Union;
                        type.name = name;
                        break;
                    } else if (symbol->kind == SymbolKind::Enum) {
                        type.kind = TypeKind::Enum;
                        type.name = name;
                        break;
                    }
                }
            } // Fallthrough
        default: 
            ReportError(
                primary.location,
                "SyntaxToken '%s' is not a type", 
                TokenGetText(primary).cstr
            );
    }

    for (let int index = 1; index < syntax->typeExpr.typeTokens.count; index += 1) {
        let SyntaxToken token = *((as SyntaxToken*)syntax->typeExpr.typeTokens.nodes[index]);
        assert(token.kind == SyntaxKind::StarToken);
        type = GetPointerTypeForBaseType(type);
    }

    if (type.kind == TypeKind::Struct) {
        let Symbol* symbol = GetSymbol(binder->symbolTable, type.name);
        if (!symbol->alreadyDefined && type.baseIndirectionLevel == 0) {
            ReportError(
                primary.location,
                "Usage of undefined but forward declared type '%s' is only allowed as pointer", 
                type.name.cstr
            );
        }
    }

    return type;
}

fun ASTNode* BindTypeExpression(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::TypeExpression);

    let Type type = BindType(binder, syntax);
    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::TypeExpression, binder->symbolTable, syntax);
    result->type = type;
    return result;
}

fun ASTNode* BindTypeCastExpression(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::TypeCastExpression);

    let ASTNode* expression = BindExpression(binder, syntax->typeCastExpr.expression);
    let Type targetType = BindType(binder, syntax->typeCastExpr.targetTypeExpression);
    
    let TypeConversionResult conversion = CanConvertTypeFromTo(expression->type, targetType);
    if (conversion == TypeConversionResult::NonConvertible) {
        ReportError(
            syntax->typeCastExpr.asKeyword.location,
            "Cast from type '%s' to type '%s' is impossible",
            TypeGetText(expression->type).cstr, TypeGetText(targetType).cstr
        );
    }

    let ASTNode* result =  ASTNodeCreate2(ASTNodeKind::CastExpression, binder->symbolTable, syntax);
    result->type = targetType;
    result->left = expression;
    return result;
}

fun ASTNode* BindSizeOfExpression(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::SizeOfExpression);

    let ASTNode* typeExpr = BindTypeExpression(binder, syntax->sizeofExpr.typeExpression);
    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::SizeOfExpression, binder->symbolTable, syntax);
    result->type = TypeCreatePrimitive(TypeKind::PrimitiveInt);
    result->left = typeExpr;
    result->isRValue = true;
    return result;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Literals

fun ASTNode* BindNullLiteralExpression(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::NullLiteralExpression);

    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::NullLiteral, binder->symbolTable, syntax);
    result->isRValue = true;
    result->type = TypeCreate(TypeKind::PrimitiveNull, 1, StringCreateEmpty());
    return result;
}

fun ASTNode* BindCharacterLiteralExpression(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::CharacterLiteralExpression);

    let SyntaxToken literal = syntax->characterLiteralExpr.characterLiteral;
    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::CharacterLiteral, binder->symbolTable, syntax);
    result->isRValue = true;
    result->intvalue = literal.intvalue;
    result->stringvalue = literal.stringValueWithoutQuotes;
    result->type = TypeCreatePrimitive(TypeKind::PrimitiveChar);
    return result;
}

fun ASTNode* BindStringLiteralExpression(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::StringLiteralExpression);

    let String stringValueWithoutQuotes = StringCreateEmpty();
    for (let int index = 0; index < syntax->stringLiteralExpr.stringLiteralTokens.count; index += 1) {
        let SyntaxToken next = syntax->stringLiteralExpr.stringLiteralTokens.nodes[index]->token;
        stringValueWithoutQuotes = StringAppend(stringValueWithoutQuotes, next.stringValueWithoutQuotes);
    }
    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::StringLiteral, binder->symbolTable, syntax);
    result->isRValue = true;
    result->stringvalue = stringValueWithoutQuotes;
    result->type = TypeCreate(TypeKind::PrimitiveChar, 1, StringCreateEmpty());
    return result;
}

fun ASTNode* BindBoolLiteralExpression(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::BoolLiteralExpression);

    let SyntaxToken literal = syntax->boolLiteralExpr.boolLiteral;
    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::BoolLiteral, binder->symbolTable, syntax);
    result->isRValue = true;
    result->intvalue = literal.kind == SyntaxKind::TrueKeyword ? 1 : 0;
    result->type = TypeCreatePrimitive(TypeKind::PrimitiveBool);
    return result;
}

fun ASTNode* BindIntegerLiteralExpression(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::IntegerLiteralExpression);

    let SyntaxToken literal = syntax->integerLiteralExpr.integerLiteral;
    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::IntegerLiteral, binder->symbolTable, syntax);
    result->isRValue = true;
    result->intvalue = literal.intvalue;
    if (CHAR_MIN <= literal.intvalue && literal.intvalue <= CHAR_MAX)
        result->type = TypeCreatePrimitive(TypeKind::PrimitiveByte);
    else if (SHRT_MIN <= literal.intvalue && literal.intvalue <= SHRT_MAX)
        result->type = TypeCreatePrimitive(TypeKind::PrimitiveShort);
    else if (INT_MIN <= literal.intvalue && literal.intvalue <= INT_MAX)
        result->type = TypeCreatePrimitive(TypeKind::PrimitiveInt);
    else
        result->type = TypeCreatePrimitive(TypeKind::PrimitiveLong);
    return result;
}

fun ASTNode* BindEnumValueLiteralExpression(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::EnumValueLiteralExpression);

    let SyntaxToken enumIdentifier = syntax->enumLiteralExpr.enumIdentifier;
    let SyntaxToken valueIdentifier = syntax->enumLiteralExpr.valueIdentifier;

    let String enumText = TokenGetText(enumIdentifier);
    let Symbol* enumSymbol = GetSymbol(binder->symbolTable, enumText);
    if (enumSymbol == nullptr) {
        ReportError(
            enumIdentifier.location,
            "Undeclared identifier '%s'", 
            enumText.cstr
        );
    }
    if (enumSymbol->kind != SymbolKind::Enum) {
        ReportError(
            enumIdentifier.location,
            "Identifier '%s' is not an enum", 
            enumText.cstr
        );
    }

    let String valueText = TokenGetText(valueIdentifier);
    let Symbol* valueSymbol = GetSymbol(enumSymbol->membersSymbolTable, valueText);
    if (valueSymbol == nullptr) {
        ReportError(
            enumIdentifier.location,
            "Identifier '%s' is not a member of enum '%s'", 
            valueText.cstr, enumText.cstr
        );
    }
    assert (valueSymbol->kind == SymbolKind::Enumvalue);

    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::EnumValueLiteral, binder->symbolTable, syntax);
    result->isRValue = true;
    result->symbol = valueSymbol;
    result->type = valueSymbol->type;
    return result;
}

fun ASTNode* BindArrayLiteralExpression(Binder* binder, Symbol* arraySymbol, SyntaxNode* syntax) { 
    assert(syntax->kind == SyntaxKind::ArrayLiteralExpression);
    assert(arraySymbol->type.isArray);

    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::ArrayLiteral, binder->symbolTable, syntax);
    let Type arrayElemType = GetElementTypeForArrayType(arraySymbol->type);

    for (let int index = 0; index < syntax->arrayLiteralExpr.elemsWithSeparators.count; index += 2) {
        let SyntaxNode* expression = syntax->arrayLiteralExpr.elemsWithSeparators.nodes[index];
        let ASTNode* boundExpression = BindExpression(binder, expression);
        let TypeConversionResult conversion = CanConvertTypeFromTo(boundExpression->type, arrayElemType);
        if (conversion == TypeConversionResult::NonConvertible 
         || conversion == TypeConversionResult::ExplicitlyConvertible) {
            ReportError(
                expression->token.location,
                "Cannot convert type '%s' of element %d in array initializer to array type '%s'",
                TypeGetText(boundExpression->type).cstr, index / 2 + 1, TypeGetText(arrayElemType).cstr
            );
        } 
        ASTNodeArrayPush(&result->children, boundExpression);
    }

    let int elementCount = result->children.count;
    let SyntaxToken leftBrace = syntax->arrayLiteralExpr.leftBrace;
    if (arraySymbol->type.arrayElementCount == -1)
        arraySymbol->type.arrayElementCount = elementCount;
    if (elementCount == 0) {
        ReportError(
            leftBrace.location,
            "Element count cannot be zero in array initializer of array '%s'",
            arraySymbol->name.cstr
        );
    }
    if (arraySymbol->type.arrayElementCount != elementCount) {
        ReportError(
            leftBrace.location,
            "Element count %d of array initializer does not match element count %d of array '%s'",
            elementCount, arraySymbol->type.arrayElementCount, arraySymbol->name.cstr
        );
    }
    
    result->isRValue = true;
    return result;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Expressions

fun ASTNode* BindNameExpression(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::NameExpression);

    let SyntaxToken identifier = syntax->nameExpr.identifier;
    let String identifierText = TokenGetText(identifier);
    let Symbol* symbol = GetSymbol(binder->symbolTable, identifierText);
    if (symbol == nullptr) {
        ReportError(
            identifier.location,
            "Undeclared identifier '%s'", 
            identifierText.cstr
        );
    }
    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::NameExpression, binder->symbolTable, syntax);
    result->symbol = symbol;
    result->type = symbol->type;
    return result;
}

fun ASTNode* BindParenthesizedExpression(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::ParenthesizedExpression);

    let ASTNode* inner = BindExpression(binder, syntax->parenthesizedExpr.expression);
    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::ParenthesizedExpression, binder->symbolTable, syntax);
    result->type = inner->type;
    result->left = inner;
    return result;
}

fun ASTNode* BindFunctionCallExpression(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::FuncCallExpression);

    let ASTNode* left = BindExpression(binder, syntax->funcCallExpr.func);
    let SyntaxToken leftParen = syntax->funcCallExpr.leftParen;

    let Symbol* funcSymbol = left->symbol;
    if (funcSymbol == nullptr) {
        // TODO: change this to a span that prints the whole left expression
        ReportError(
            leftParen.location,
            "Expression left of '%s' is not a known symbol", 
            TokenGetText(leftParen)
        );
    }
    if (funcSymbol->kind != SymbolKind::Function) {
        // TODO: change this to a span that prints the whole left expression
        ReportError(
            leftParen.location,
            "Identifier '%s' is not a callable function", 
            funcSymbol->name.cstr
        );
    }

    let ASTNodeArray argumentList = ASTNodeArrayCreate();
    for (let int index = 0; index < syntax->funcCallExpr.argumentsWithSeparators.count; index += 2) {
        let SyntaxNode* arg = syntax->funcCallExpr.argumentsWithSeparators.nodes[index];
        let ASTNode* boundArg = BindExpression(binder, arg);
        ASTNodeArrayPush(&argumentList, boundArg);
    }
    
    if (funcSymbol->isVariadric) {
        if (argumentList.count < funcSymbol->membersSymbolTable->count) {
            ReportError(
                leftParen.location,
                "Function '%s' expects at least %d arguments but %d arguments were provided", 
                funcSymbol->name.cstr, funcSymbol->membersSymbolTable->count, argumentList.count
            );
        }
    } else {
        if (argumentList.count != funcSymbol->membersSymbolTable->count) {
            ReportError(
                leftParen.location,
                "Function '%s' expects %d arguments but %d arguments were provided", 
                funcSymbol->name.cstr, funcSymbol->membersSymbolTable->count, argumentList.count
            );
        }
    }

    for (let int argumentIndex = 0; argumentIndex < funcSymbol->membersSymbolTable->count; argumentIndex += 1) {
        let Type argumentType = argumentList.nodes[argumentIndex]->type;
        let Type expectedType = funcSymbol->membersSymbolTable->symbols[argumentIndex]->type;

        let TypeConversionResult conversion = CanConvertTypeFromTo(argumentType, expectedType);
        if (conversion == TypeConversionResult::NonConvertible) {
            ReportError(
                leftParen.location,
                "Passed incompatible type '%s' for argument %d to function '%s' - expected type '%s'", 
                TypeGetText(argumentType).cstr, argumentIndex + 1, funcSymbol->name.cstr, TypeGetText(expectedType).cstr 
            );
        }
        if (conversion == TypeConversionResult::ExplicitlyConvertible) {
            ReportError(
                leftParen.location,
                "Cannot implicitly convert type '%s' of argument %d  to expected type '%s' in function call of '%s'", 
                TypeGetText(argumentType).cstr, argumentIndex + 1, TypeGetText(expectedType).cstr, funcSymbol->name.cstr
            );
        }
    }

    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::FunccallExpression, binder->symbolTable, syntax);
    result->symbol = funcSymbol;
    result->type = funcSymbol->type;
    result->children = argumentList;
    return result;
}

fun ASTNode* BindArrayIndexingExpression(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::ArrayIndexExpression);

    let SyntaxToken leftBracket = syntax->arrayIndexExpr.leftBracket;
    let ASTNode* left = BindExpression(binder, syntax->arrayIndexExpr.arr);
    let ASTNode* index = BindExpression(binder, syntax->arrayIndexExpr.indexExpression);
    if (!IsNumberType(index->type)) {
        ReportError(
            leftBracket.location,
            "Array index after '%s' must be number type", 
            TokenKindToString(leftBracket.kind).cstr
        );
    }
    if (left->symbol == nullptr || TypeGetIndirectionLevel(left->symbol->type) == 0) {
        ReportError(
            leftBracket.location,
            "Left hand side of array index operator '%s' is not a known array or pointer", 
            TokenKindToString(leftBracket.kind).cstr
        );
    }

    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::Arrayindexing, binder->symbolTable, syntax);
    result->type = left->symbol->type;
    if (result->type.isArray)
        result->type.isArray = false; // Because we index it
    else
        result->type = GetBaseTypeForPointerType(result->type); // Because we index it
    result->left = left;
    result->right = index;
    return result;
}

fun ASTNode* BindMemberAccessExpression(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::MemberAccessExpression);

    let ASTNode* container = BindExpression(binder, syntax->memberAccessExpr.container);
    let SyntaxToken accessorToken = syntax->memberAccessExpr.accessToken;
    if (container->type.kind != TypeKind::Struct && container->type.kind != TypeKind::Union) {
        ReportError(
            accessorToken.location,
            "Attempt to access member of non union or struct identifier '%s'", 
            container->symbol->name.cstr
        );
    }

    let Symbol* containerSymbol = GetSymbol(binder->symbolTable, container->type.name);
    assert(containerSymbol != nullptr);
    if (containerSymbol->kind != SymbolKind::Struct && containerSymbol->kind != SymbolKind::Union) {
        ReportError(
            accessorToken.location,
            "Attempt to access member of non union or struct identifier '%s'", 
            containerSymbol->name.cstr
        );
    }
    if (!containerSymbol->alreadyDefined) {
        ReportError(
            accessorToken.location,
            "Attempt to access member of forward declared but undefined union or struct '%s'", 
            containerSymbol->name.cstr
        );
    }

    let bool isArrow = accessorToken.kind == SyntaxKind::ArrowToken;
    if (isArrow && TypeGetIndirectionLevel(container->type) == 0) {
        ReportError(
            accessorToken.location,
            "Member access of '%s' with '->' is only allowed for pointer types", 
            containerSymbol->name.cstr
        );
    }
    if (!isArrow && TypeGetIndirectionLevel(container->type) > 0) {
        ReportError(
            accessorToken.location,
            "Member access of '%s' with '.' is only allowed for non-pointer types", 
            containerSymbol->name.cstr
        );
    }
    
    let SyntaxToken memberIdentifier = syntax->memberAccessExpr.memberIdentifier;
    let String identifierText = TokenGetText(memberIdentifier);
    let Symbol* memberSymbol = GetSymbol(containerSymbol->membersSymbolTable, identifierText);
    if (memberSymbol == nullptr) {
        ReportError(
            memberIdentifier.location,
            "Undeclared struct or union member '%s'", 
            identifierText.cstr
        );
    }
    assert(memberSymbol->kind == SymbolKind::Member);

    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::Memberaccess, binder->symbolTable, syntax);
    result->symbol = memberSymbol;
    result->type = memberSymbol->type;
    result->left = container;
    return result;
}


fun ASTNode* BindUnaryExpression(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::UnaryExpression);

    let ASTNode* operand = BindExpression(binder, syntax->unaryExpr.operand);
    let SyntaxToken operatorToken = syntax->unaryExpr.operatorToken;
    let UnaryOperator op = GetUnaryOperationForToken(operatorToken, operand->type);
    if (op.operandMustBeLValue && operand->isRValue) {
        ReportError(
            operatorToken.location,
            "Operand of operator '%s' must be an storage location", 
            TokenKindToString(operatorToken.kind).cstr
        );
    }

    let ASTNode* result = ASTNodeCreate2(op.operatorKind, binder->symbolTable, syntax);
    result->isRValue = op.resultIsRValue;
    result->type = op.resultType;
    result->left = operand;
    return result;
}

fun ASTNode* BindBinaryExpression(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::BinaryExpression);

    let ASTNode* left = BindExpression(binder, syntax->binaryExpr.left);
    let ASTNode* right = BindExpression(binder, syntax->binaryExpr.right);
    let SyntaxToken operatorToken = syntax->binaryExpr.operatorToken;

    let BinaryOperator op = GetBinaryOperationForToken(operatorToken, left->type, right->type);
    if (op.leftMustBeLValue && left->isRValue) {
        ReportError(
            operatorToken.location,
            "Left argument of operator '%s' must be an storage location", 
            TokenKindToString(operatorToken.kind).cstr
        );
    }
    if (op.rightMustBeLValue && right->isRValue) {
        ReportError(
            operatorToken.location,
            "Right argument of operator '%s' must be a storage location", 
            TokenKindToString(operatorToken.kind).cstr
        );
    }

    let ASTNode* result = ASTNodeCreate2(op.operatorKind, binder->symbolTable, syntax);
    result->isRValue = op.resultIsRValue;
    result->type = op.resultType;
    result->left = left;
    result->right = right;
    return result;
}

fun ASTNode* BindTernaryConditionalExpression(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::TernaryConditionalExpression);

    let ASTNode* condition = BindExpression(binder, syntax->ternaryConditionalExpr.conditionExpression);
    let ASTNode* thenExpression = BindExpression(binder, syntax->ternaryConditionalExpr.thenExpression);
    let ASTNode* elseExpression = BindExpression(binder, syntax->ternaryConditionalExpr.elseExpression);
    let Type type = GetTypeThatFitsBothTypes(thenExpression->type, elseExpression->type);
    if (IsVoidType(type)) {
        ReportError(
            syntax->ternaryConditionalExpr.questionmark.location,
            "Incompatible expression types in ternary operator - then branch: '%s', else branch: '%s'",
            TypeGetText(thenExpression->type).cstr, TypeGetText(elseExpression->type).cstr
        );
    }

    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::TernaryConditionalExpression, binder->symbolTable, syntax);
    result->left = condition;
    result->right = thenExpression;
    result->extra1 = elseExpression;
    result->type = type;
    result->isRValue = true;
    return result;
}

fun ASTNode* BindExpression(Binder* binder, SyntaxNode* syntax) {
    let ASTNode* result = nullptr;
    switch (syntax->kind) {
        // Expressions
        case SyntaxKind::UnaryExpression:
            result = BindUnaryExpression(binder, syntax);
            break;
        case SyntaxKind::BinaryExpression:
            result = BindBinaryExpression(binder, syntax);
            break;
        case SyntaxKind::FuncCallExpression:
            result = BindFunctionCallExpression(binder, syntax);
            break;
        case SyntaxKind::ArrayIndexExpression:
            result = BindArrayIndexingExpression(binder, syntax);
            break;
        case SyntaxKind::MemberAccessExpression:
            result = BindMemberAccessExpression(binder, syntax);
            break;
        case SyntaxKind::TypeCastExpression:
            result = BindTypeCastExpression(binder, syntax);
            break;
        case SyntaxKind::ParenthesizedExpression:
            result = BindParenthesizedExpression(binder, syntax);
            break;
        case SyntaxKind::TernaryConditionalExpression:
            result = BindTernaryConditionalExpression(binder, syntax);
            break;
        case SyntaxKind::SizeOfExpression:
            result = BindSizeOfExpression(binder, syntax);
            break;
        case SyntaxKind::NameExpression:
            result = BindNameExpression(binder, syntax);
            break;
        case SyntaxKind::TypeExpression:
            result = BindTypeExpression(binder, syntax);
            break;

        // Literals
        case SyntaxKind::NullLiteralExpression:
            result = BindNullLiteralExpression(binder, syntax);
            break;
        case SyntaxKind::IntegerLiteralExpression:
            result = BindIntegerLiteralExpression(binder, syntax);
            break;
        case SyntaxKind::CharacterLiteralExpression:
            result = BindCharacterLiteralExpression(binder, syntax);
            break;
        case SyntaxKind::BoolLiteralExpression:
            result = BindBoolLiteralExpression(binder, syntax);
            break;
        case SyntaxKind::StringLiteralExpression:
            result = BindStringLiteralExpression(binder, syntax);
            break;
        case SyntaxKind::EnumValueLiteralExpression:
            result = BindEnumValueLiteralExpression(binder, syntax);
            break;
        
        default:
            assert(false && "Unexpected expression in binder");
    }
    if (result->type.isArray) {
        // TODO: is this the appropriate place for this check?
        result->isRValue = true; // Arrays cannot be assigned to without indexing
    }
    return result;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Statements

fun ASTNode* BindExpressionStatement(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::ExpressionStatement);

    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::ExpressionStatement, binder->symbolTable, syntax);
    result->left = BindExpression(binder, syntax->expressionStmt.expression);
    return result;
}

fun ASTNode* BindVariableDefinitionStatement(Binder* binder, SyntaxNode* syntax, SymbolScopeKind symbolScopeKind) {
    assert(syntax->kind == SyntaxKind::VariableDeclarationStatement);

    let SyntaxToken identifier = syntax->variableDeclarationStmt.identifier;
    let bool isLocalPersist = syntax->variableDeclarationStmt.letKeyword.kind == SyntaxKind::LetLocalPersistKeyword;
    if (isLocalPersist) {
        if (symbolScopeKind != SymbolScopeKind::Local) {
            ReportError(
                identifier.location,
                "Cannot mark global variable '%s' as local persistent", 
                TokenGetText(identifier).cstr
            );
        }
        symbolScopeKind = SymbolScopeKind::LocalPersist;
    }

    let Type type = BindType(binder, syntax->variableDeclarationStmt.typeExpression);
    let Symbol* varSymbol = AddSymbol(binder->symbolTable, TokenGetText(identifier), SymbolKind::Variable, symbolScopeKind, type);
    if (varSymbol == nullptr) {
        ReportError(
            identifier.location,
            "Symbol was '%s' already declared in current scope", 
            TokenGetText(identifier).cstr
        );
    }

    if (IsVoidType(type)) {
        ReportError(
            identifier.location,
            "'void' not allowed as variables '%s' storage type", 
            TokenGetText(identifier).cstr
        );
    }

    if (syntax->variableDeclarationStmt.leftBracket.kind == SyntaxKind::LeftBracketToken) {
        // Array definition
        let longint arrayElementCount = -1;
        let bool arrayElementCountDefined = false;
        if (syntax->variableDeclarationStmt.arraySizeLiteral.kind == SyntaxKind::IntegerLiteralToken) {
            arrayElementCount = syntax->variableDeclarationStmt.arraySizeLiteral.intvalue;
            arrayElementCountDefined = true;
        }
        if (arrayElementCountDefined && arrayElementCount <= 0) {
            ReportError(
                identifier.location,
                "Array size must be greater than zero for '%s'", 
                TokenGetText(identifier).cstr
            );
        }
        varSymbol->type.isArray = true;
        varSymbol->type.arrayElementCount = arrayElementCount;
    }

    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::VariableDeclarationStatement, binder->symbolTable, syntax);
    result->symbol = varSymbol;
    if (syntax->variableDeclarationStmt.initializerExpression != nullptr) {
        let ASTNode* boundInitializer = nullptr;
        if (result->symbol->type.isArray)
            boundInitializer = BindArrayLiteralExpression(binder, result->symbol, syntax->variableDeclarationStmt.initializerExpression);
        else
            boundInitializer = BindExpression(binder, syntax->variableDeclarationStmt.initializerExpression);
        result->left = boundInitializer;
    } 
    return result;
}

fun ASTNode* BindIfStatement(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::IfStatement);
    assert(binder->currentFunctionSymbol != nullptr);

    let ASTNode* boundCondition = BindExpression(binder, syntax->ifStmt.condition);
    // TODO we should check that conditions is truthy

    let ASTNode* boundThenBranch = BindStatement(binder, syntax->ifStmt.thenBlock);
    boundThenBranch = _WrapInBlockStatementIfNecessary(binder, boundThenBranch);

    let ASTNode* boundElseBranch = nullptr;
    if (syntax->ifStmt.elseBlock != nullptr) {
        boundElseBranch = BindStatement(binder, syntax->ifStmt.elseBlock);
        boundElseBranch = _WrapInBlockStatementIfNecessary(binder, boundElseBranch);
    }

    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::IfStatement, binder->symbolTable, syntax);
    result->left = boundCondition;
    result->right = boundThenBranch;
    result->extra1 = boundElseBranch;
    return result;
}

fun ASTNode* BindWhileStatement(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::WhileStatement);
    assert(binder->currentFunctionSymbol != nullptr);

    let ASTNode* boundCondition = BindExpression(binder, syntax->whileStmt.condition);
    // TODO we should check that conditions is truthy

    binder->loopLevel += 1;
    let ASTNode* boundBody = BindStatement(binder, syntax->whileStmt.body);
    boundBody = _WrapInBlockStatementIfNecessary(binder, boundBody);
    binder->loopLevel -= 1;

    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::WhileStatement, binder->symbolTable, syntax);
    result->left = boundCondition;
    result->right = boundBody;
    return result;
}

fun ASTNode* BindDoWhileStatement(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::DoWhileStatement);
    assert(binder->currentFunctionSymbol != nullptr);

    binder->loopLevel += 1;
    let ASTNode* boundBody = BindStatement(binder, syntax->doWhileStmt.body);
    boundBody = _WrapInBlockStatementIfNecessary(binder, boundBody);
    binder->loopLevel -= 1;

    let ASTNode* boundCondition = BindExpression(binder, syntax->doWhileStmt.condition);
    // TODO we should check that conditions is truthy

    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::DoWhileStatement, binder->symbolTable, syntax);
    result->left = boundCondition;
    result->right = boundBody;
    return result;
}

fun ASTNode* BindForStatement(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::ForStatement);
    assert(binder->currentFunctionSymbol != nullptr);

    // Push symboltable scope early to also make the index local to the for statement
    binder->symbolTable = SymbolTableCreate(binder->symbolTable);
    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::ForStatement, binder->symbolTable, syntax);

    let ASTNode* boundInitializer = nullptr;
    if (syntax->forStmt.initializerStatement->kind == SyntaxKind::VariableDeclarationStatement)
        boundInitializer = BindVariableDefinitionStatement(binder, syntax->forStmt.initializerStatement, SymbolScopeKind::Local);
    else 
        boundInitializer = BindExpressionStatement(binder, syntax->forStmt.initializerStatement);
    let ASTNode* boundCondition = BindExpressionStatement(binder, syntax->forStmt.conditionStatement);
    // TODO we should check that conditions is truthy
    let ASTNode* boundIncrementExpr = BindExpression(binder, syntax->forStmt.incrementExpression);

    binder->loopLevel += 1;
    let ASTNode* boundBody = BindStatement(binder, syntax->forStmt.body);
    boundBody = _WrapInBlockStatementIfNecessary(binder, boundBody);
    binder->loopLevel -= 1;

    // Pop symboltable
    binder->symbolTable = binder->symbolTable->parent;

    result->left = boundInitializer;
    result->right = boundCondition;
    result->extra1 = boundIncrementExpr;
    result->extra2 = boundBody;
    return result;
}

fun ASTNode* BindReturnStatement(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::ReturnStatement);
    assert(binder->currentFunctionSymbol != nullptr);

    if (binder->currentFunctionSymbol == nullptr) {
        ReportError(
            syntax->returnStmt.returnKeyword.location,
            "Invalid 'return' statement found outside of function definition"
        );
    }

    let Type functionReturnType = binder->currentFunctionSymbol->type;
    if (IsVoidType(functionReturnType) && syntax->returnStmt.returnExpression != nullptr) {
        ReportError(
            syntax->returnStmt.returnKeyword.location,
            "Invalid return expression in void function"
        );
    }
    if (!IsVoidType(functionReturnType) && syntax->returnStmt.returnExpression == nullptr) {
        ReportError(
            syntax->returnStmt.returnKeyword.location,
            "Must return expression in non-void function"
        );
    }

    let ASTNode* boundExpression = nullptr;
    if (syntax->returnStmt.returnExpression != nullptr) {
        boundExpression = BindExpression(binder, syntax->returnStmt.returnExpression);

        let TypeConversionResult conversion = CanConvertTypeFromTo(boundExpression->type, functionReturnType);
        if (conversion == TypeConversionResult::NonConvertible) {
            ReportError(
                syntax->returnStmt.returnKeyword.location,
                "Incompatible types for return expression '%s'", 
                TokenKindToString(syntax->returnStmt.returnKeyword.kind).cstr
            );
        }
        if (conversion == TypeConversionResult::ExplicitlyConvertible) {
            ReportError(
                syntax->returnStmt.returnKeyword.location,
                "Types cannot be implicitly converted for return expression '%s'", 
                TokenKindToString(syntax->returnStmt.returnKeyword.kind).cstr
            );
        }
    }
    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::ReturnStatement, binder->symbolTable, syntax);
    result->left = boundExpression;
    return result;
}

fun ASTNode* BindBreakStatement(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::BreakStatement);
    assert(binder->currentFunctionSymbol != nullptr);

    if (binder->loopLevel == 0 && binder->switchCaseLevel == 0) {
        ReportError(
            syntax->breakStmt.breakKeyword.location,
            "Invalid 'break' statement found outside of loop or switch-case definition"
        );
    }
    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::BreakStatement, binder->symbolTable, syntax);
    return result;
}

fun ASTNode* BindContinueStatement(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::ContinueStatement);
    assert(binder->currentFunctionSymbol != nullptr);

    if (binder->loopLevel == 0) {
        ReportError(
            syntax->continueStmt.continueKeyword.location,
            "Invalid 'continue' statement found outside of loop definition"
        );
    }
    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::ContinueStatement, binder->symbolTable, syntax);
    return result;
}

fun ASTNode* BindBlockStatement(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::BlockStatement);
    assert(binder->currentFunctionSymbol != nullptr);

    // Push symboltable scope
    binder->symbolTable = SymbolTableCreate(binder->symbolTable);

    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::BlockStatement, binder->symbolTable, syntax);
    for (let int index = 0; index < syntax->blockStmt.statements.count; index += 1) {
        let SyntaxNode* statement = syntax->blockStmt.statements.nodes[index];
        let ASTNode* boundStatement = BindStatement(binder, statement);
        ASTNodeArrayPush(&result->children, boundStatement);
    }

    // Pop symboltable
    binder->symbolTable = binder->symbolTable->parent;
    return _FlattenBlockStatementIfNecessary(binder, result);
}

fun ASTNode* BindCaseStatement(Binder* binder, ASTNode* switchExpression, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::CaseStatement || syntax->kind == SyntaxKind::DefaultStatement);
    assert(binder->currentFunctionSymbol != nullptr);
    assert(binder->switchCaseLevel != 0);

    let bool isDefault = syntax->kind == SyntaxKind::DefaultStatement;

    let ASTNode* caseExpression = nullptr;
    if (syntax->kind == SyntaxKind::CaseStatement) {
        caseExpression = BindExpression(binder, syntax->caseStmt.literalExpression);
        if (caseExpression->kind != ASTNodeKind::IntegerLiteral 
        && caseExpression->kind != ASTNodeKind::StringLiteral 
        && caseExpression->kind != ASTNodeKind::CharacterLiteral 
        && caseExpression->kind != ASTNodeKind::EnumValueLiteral) {
            ReportError(
                caseExpression->token.location, 
                "Expected literal in case label but got '%s'",
                TokenKindToString(caseExpression->token.kind).cstr
            );
        }
        let TypeConversionResult conversion = CanConvertTypeFromTo(caseExpression->type, switchExpression->type);
        if (conversion != TypeConversionResult::Identical && conversion != TypeConversionResult::ImplictlyConvertible) {
            ReportError(
                caseExpression->token.location, 
                "Cannot convert type '%s' of case label literal '%s' to its switch expression type '%s'",
                TypeGetText(caseExpression->type).cstr, TokenGetText(caseExpression->token).cstr, TypeGetText(switchExpression->type).cstr
            );
        }
    } 

    let ASTNode* body = BindBlockStatement(binder, isDefault 
        ? syntax->defaultStmt.body 
        : syntax->caseStmt.body);
    if (body->children.count == 0)
        body = nullptr;

    let ASTNode* result = ASTNodeCreate2( isDefault 
        ? ASTNodeKind::DefaultStatement 
        : ASTNodeKind::CaseStatement, binder->symbolTable, syntax);
    result->left = body;
    result->right = caseExpression;
    return result;
}

fun ASTNode* BindSwitchStatement(Binder* binder, SyntaxNode* syntax) {
    assert(syntax->kind == SyntaxKind::SwitchStatement);
    assert(binder->currentFunctionSymbol != nullptr);

    let ASTNode* switchExpression = BindExpression(binder, syntax->switchStmt.switchExpression);

    binder->switchCaseLevel += 1;

    let int defaultStatementEncountered = false;
    let ASTNodeArray caseStatements = ASTNodeArrayCreate();
    for (let int index = 0; index < syntax->switchStmt.caseStatements.count; index += 1) {
        let SyntaxNode* caseStatement = syntax->switchStmt.caseStatements.nodes[index];
        let ASTNode* boundCaseStatement = BindCaseStatement(binder, switchExpression, caseStatement);
        if (defaultStatementEncountered) {
            ReportError(
                caseStatement->caseStmt.caseKeyword.location,
                "Unexpected case statement after default statement was already defined"
            );
        }
        if (caseStatement->kind == SyntaxKind::DefaultStatement)
            defaultStatementEncountered = true;
        ASTNodeArrayPush(&caseStatements, boundCaseStatement);
    }

    binder->switchCaseLevel -= 1;

    if (caseStatements.count == 0) {
        ReportError(
            syntax->switchStmt.switchKeyword.location,
            "Empty switch statements are not allowed"
        );
    }

    // Check that we don't have any duplicate case labels
    for (let int index = 0; index < caseStatements.count; index += 1) {
        let ASTNode* a = caseStatements.nodes[index];
        for (let int inner = index + 1; inner < caseStatements.count; inner += 1) {
            let ASTNode* b = caseStatements.nodes[inner];
            if (a->right && b->right && AreLiteralsEqual(a->right, b->right)) {
                ReportError(
                    b->token.location,
                    "Duplicate switch case literal '%s'",
                    TokenGetText(b->right->token).cstr
                );
            }
        }
    }

    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::SwitchStatement, binder->symbolTable, syntax);
    result->left = switchExpression;
    result->children = caseStatements;
    return result;
}

fun ASTNode* BindStatement(Binder* binder, SyntaxNode* syntax) {
    assert(binder->currentFunctionSymbol != nullptr);
    switch (syntax->kind) {
        case SyntaxKind::BlockStatement:
            return BindBlockStatement(binder, syntax);
        case SyntaxKind::IfStatement:
            return BindIfStatement(binder, syntax);
        case SyntaxKind::DoWhileStatement:
            return BindDoWhileStatement(binder, syntax);
        case SyntaxKind::WhileStatement:
            return BindWhileStatement(binder, syntax);
        case SyntaxKind::ForStatement:
            return BindForStatement(binder, syntax);
        case SyntaxKind::ReturnStatement:
            return BindReturnStatement(binder, syntax);
        case SyntaxKind::BreakStatement:
            return BindBreakStatement(binder, syntax);
        case SyntaxKind::ContinueStatement:
            return BindContinueStatement(binder, syntax);
        case SyntaxKind::SwitchStatement:
            return BindSwitchStatement(binder, syntax);
        case SyntaxKind::VariableDeclarationStatement:
            return BindVariableDefinitionStatement(binder, syntax, SymbolScopeKind::Local);
        case SyntaxKind::ExpressionStatement:
            return BindExpressionStatement(binder, syntax);
        default:
            assert(false && "Unexpected statement in binder");
    }
    exit(1);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Module

fun ASTNode* BindGlobalVariableDefinitionStatement(Binder* binder, SyntaxNode* syntax) {
    assert(binder->currentFunctionSymbol == nullptr);
    assert(syntax->kind == SyntaxKind::GlobalVariableDeclarationStatement);

    let bool isExternal = syntax->globalVariableStmt.externKeyword.kind == SyntaxKind::ExternKeyword;
    let SymbolScopeKind symbolScopeKind = isExternal ? SymbolScopeKind::Extern : SymbolScopeKind::Global;
    return BindVariableDefinitionStatement(binder, syntax->globalVariableStmt.variableDeclarationStatement, symbolScopeKind);
}

fun ASTNode* BindStructOrUnionDefinitionStatement(Binder* binder, SyntaxNode* syntax) {
    assert(binder->currentFunctionSymbol == nullptr);
    assert(syntax->kind == SyntaxKind::StructOrUnionDeclarationStatement || syntax->kind == SyntaxKind::StructOrUniontDefinitionStatement);

    let bool isExternal = syntax->structOrUnionDeclarationStmt.externKeyword.kind == SyntaxKind::ExternKeyword;
    let SymbolScopeKind symbolScopeKind = isExternal ? SymbolScopeKind::Extern : SymbolScopeKind::Global;

    let bool isUnion = syntax->structOrUnionDeclarationStmt.structOrUnionKeyword.kind == SyntaxKind::UnionKeyword;

    let SyntaxToken identifier = syntax->structOrUnionDeclarationStmt.identifier;
    let String name = TokenGetText(identifier);

    let Symbol* structSymbol = GetSymbol(binder->symbolTable, name);
    if (structSymbol != nullptr) {
        if (isUnion &&  structSymbol->kind != SymbolKind::Union) {
            ReportError(
                identifier.location,
                "Another symbol with the same name '%s' but different type was already declared in current scope", 
                TokenGetText(identifier).cstr
            );
        }
        if (!isUnion &&  structSymbol->kind != SymbolKind::Struct) {
            ReportError(
                identifier.location,
                "Another symbol with the same name '%s' but different type was already declared in current scope", 
                TokenGetText(identifier).cstr
            );
        }
        if (structSymbol->scopeKind != symbolScopeKind) {
            ReportError(
                identifier.location,
                "Struct or union '%s was previously declared but with different scope attribute", 
                TokenGetText(identifier).cstr
            );
        }
    }
    if (structSymbol == nullptr) {
        let Type type = TypeCreate(isUnion ? TypeKind::Union : TypeKind::Struct, 0, name);
        structSymbol = AddSymbol(binder->symbolTable, name, isUnion ? SymbolKind::Union : SymbolKind::Struct, symbolScopeKind, type);
    }

    if (syntax->kind == SyntaxKind::StructOrUnionDeclarationStatement) {
        let ASTNode* result = ASTNodeCreate2(isUnion 
            ? ASTNodeKind::UnionDeclarationStatement 
            : ASTNodeKind::StructDeclarationStatement, binder->symbolTable, syntax);
        result->symbol = structSymbol;
        return result;
    } else {
        structSymbol->membersSymbolTable = SymbolTableCreate(binder->symbolTable);

        // Push union/struct member symboltable scope to parse variable declarations as members
        binder->symbolTable = structSymbol->membersSymbolTable;

        for (let int index = 0; index < syntax->structOrUnionDefinitionStmt.memberDeclarationStatements.count; index += 1) {
            let SyntaxNode* memberDeclaration = syntax->structOrUnionDefinitionStmt.memberDeclarationStatements.nodes[index];
            let ASTNode* memberNode = BindVariableDefinitionStatement(binder, memberDeclaration, SymbolScopeKind::Local);
            memberNode->symbol->kind = SymbolKind::Member;
        }

        // Pop symboltable
        binder->symbolTable = binder->symbolTable->parent;

        if (structSymbol->membersSymbolTable->count == 0){
            ReportError(
                identifier.location,
                "Struct or union '%s' needs at least one member", 
                name.cstr
            );
        }
        if (structSymbol->alreadyDefined) {
            ReportError(
                identifier.location,
                "Duplicate struct or union definition of '%s'", 
                name.cstr
            );
        }
        structSymbol->alreadyDefined = true;

        let ASTNode* result = ASTNodeCreate2(isUnion 
            ? ASTNodeKind::UnionDefinitionStatement 
            : ASTNodeKind::StructDefinitionStatement, binder->symbolTable, syntax);
        result->symbol = structSymbol;
        return result;
    }
}

fun ASTNode* BindEnumDefinitionStatement(Binder* binder, SyntaxNode* syntax) {
    assert(binder->currentFunctionSymbol == nullptr);
    assert(syntax->kind == SyntaxKind::EnumDeclarationStatement || syntax->kind == SyntaxKind::EnumDefinitionStatement);

    let bool isExternal = syntax->enumDeclarationStmt.externKeyword.kind == SyntaxKind::ExternKeyword;
    let SymbolScopeKind symbolScopeKind = isExternal ? SymbolScopeKind::Extern : SymbolScopeKind::Global;

    let SyntaxToken identifier = syntax->enumDeclarationStmt.identifier;
    let String name = TokenGetText(identifier);

    if (binder->currentFunctionSymbol != nullptr) {
        ReportError(
            identifier.location,
            "Unexpected enum declaration of '%s' while already parsing function", 
            name.cstr
        );
    }

    let Symbol* enumSymbol = GetSymbol(binder->symbolTable, name);
    if (enumSymbol != nullptr) {
        if (enumSymbol->kind != SymbolKind::Enum) {
            ReportError(
                identifier.location,
                "Another symbol with the same name '%s' but different type was already declared in current scope",
                TokenGetText(identifier).cstr
            );
        }
        if (enumSymbol->scopeKind != symbolScopeKind) {
            ReportError(
                identifier.location,
                "Enum '%s was previously declared but with different scope attribute", 
                TokenGetText(identifier).cstr
            );
        }
    }
    if (enumSymbol == nullptr) {
        let Type type = TypeCreate(TypeKind::Enum, 0, name);
        enumSymbol = AddSymbol(binder->symbolTable, name, SymbolKind::Enum, symbolScopeKind, type);
    }

    if (syntax->kind == SyntaxKind::EnumDeclarationStatement) {
        let ASTNode* result = ASTNodeCreate2(ASTNodeKind::EnumDeclarationStatement, binder->symbolTable, syntax);
        result->symbol = enumSymbol;
        return result;
    } else {
        enumSymbol->membersSymbolTable = SymbolTableCreate(binder->symbolTable);

        // Push enum member symboltable scope to parse declarations as members
        binder->symbolTable = enumSymbol->membersSymbolTable;

        let longint valueCounter = 0;
        let Type memberType = TypeCreate(TypeKind::Enum, 0, enumSymbol->name);
        for (let int index = 0; index < syntax->enumDefinitionStmt.memberClauses.count; index += 1) {
            let SyntaxNode* memberClause = syntax->enumDefinitionStmt.memberClauses.nodes[index];

            let String valueName = TokenGetText(memberClause->enumMember.identifier);
            let Symbol* valueSymbol = AddSymbol(binder->symbolTable, valueName, SymbolKind::Enumvalue, SymbolScopeKind::Global, memberType);
            if (valueSymbol == nullptr) {
                ReportError(
                    identifier.location,
                    "Symbol was '%s' already declared in current scope", 
                    TokenGetText(identifier).cstr
                );
            }

            if (memberClause->enumMember.integerLiteral.kind == SyntaxKind::IntegerLiteralToken) {
                let SyntaxToken valueToken = memberClause->enumMember.integerLiteral;
                if (valueToken.intvalue < valueCounter)
                {
                    ReportError(
                        valueToken.location,
                        "Assigned value of enum value literal '%s' must chosen such that all enum values of '%s' are unique - chosen value '%lld' would lead to duplicates", 
                        TokenGetText(memberClause->enumMember.identifier).cstr, enumSymbol->name.cstr, valueToken.intvalue
                    );
                }
                valueCounter = valueToken.intvalue;
            }
            valueSymbol->kind = SymbolKind::Enumvalue;
            valueSymbol->enumValue = valueCounter;
            valueCounter += 1;
        }

        // Pop symboltable
        binder->symbolTable = binder->symbolTable->parent;

        if (enumSymbol->membersSymbolTable->count == 0){
            ReportError(
                identifier.location,
                "Enum '%s' needs at least one member", 
                name.cstr
            );
        }

        if (enumSymbol->alreadyDefined) {
            ReportError(
                identifier.location,
                "Duplicate enum definition of '%s'", 
                name.cstr
            );
        }
        enumSymbol->alreadyDefined = true;

        let ASTNode* result = ASTNodeCreate2(ASTNodeKind::EnumDefinitionStatement, binder->symbolTable, syntax);
        result->symbol = enumSymbol;
        return result;
    }
}

fun ASTNode* BindFunctionDefinitionStatement(Binder* binder, SyntaxNode* syntax) {
    assert(binder->currentFunctionSymbol == nullptr);
    assert(syntax->kind == SyntaxKind::FunctionDeclarationStatement || syntax->kind == SyntaxKind::FunctionDefinitionStatement);

    let Type returnType = BindType(binder, syntax->functionDeclarationStmt.returnType);
    let SyntaxToken identifier = syntax->functionDeclarationStmt.identifier;

    let bool isExternal = syntax->functionDeclarationStmt.externKeyword.kind == SyntaxKind::ExternKeyword;
    let SymbolScopeKind symbolScopeKind = isExternal ? SymbolScopeKind::Extern : SymbolScopeKind::Global;

    let Symbol* functionSymbol = GetSymbol(binder->symbolTable, TokenGetText(identifier));
    if (functionSymbol != nullptr) {
        if (functionSymbol->kind != SymbolKind::Function) {
            ReportError(
                identifier.location,
                "Another symbol with the same name '%s' but different type was already declared in current scope", 
                TokenGetText(identifier).cstr
            );
        }
        if (functionSymbol->scopeKind != symbolScopeKind) {
            ReportError(
                identifier.location,
                "Function '%s was previously declared but with different scope attribute", 
                TokenGetText(identifier).cstr
            );
        }
    }

    if (functionSymbol == nullptr)
        functionSymbol = AddSymbol(binder->symbolTable, TokenGetText(identifier), SymbolKind::Function, symbolScopeKind, returnType);

    let SymbolTable* functionParamsSymbolTable = SymbolTableCreate(binder->symbolTable);

    // Push function symboltable scope to parse variables as function parameters
    binder->symbolTable = functionParamsSymbolTable;

    let bool isVariadric = false;
    for (let int index = 0; index < syntax->functionDeclarationStmt.params.count; index += 1) {
        let SyntaxNode* param = syntax->functionDeclarationStmt.params.nodes[index];
        if (param->kind == SyntaxKind::DotDotDotToken) {
            isVariadric = true;
            assert(index == syntax->functionDeclarationStmt.params.count - 1 && "'...' must be last param");
            break;
        }

        let ASTNode* paramNode = BindVariableDefinitionStatement(binder, param, SymbolScopeKind::Local);
        paramNode->symbol->kind = SymbolKind::Parameter;

        if (param->variableDeclarationStmt.terminatorToken.kind != SyntaxKind::CommaToken) {
            assert(index == syntax->functionDeclarationStmt.params.count - 1 && "param omitting ',' must be last param");
            break;
        }
    }

    // Pop symboltable
    binder->symbolTable = binder->symbolTable->parent;

    if (functionSymbol->membersSymbolTable != nullptr) {
        // The function was already declared before, we need to make sure its types and 
        // parameters match up with the previous defined type and parameters
        if (!TypesIdentical(returnType, functionSymbol->type)) {
            ReportError(
                identifier.location,
                "Return type of function '%s' does not match return type of a previous declaration", 
                TokenGetText(identifier).cstr
            );
        }

        if (functionSymbol->isVariadric != isVariadric) {
            ReportError(
                identifier.location,
                "Vadriaticity of function '%s' does not match with a previous declaration", 
                TokenGetText(identifier).cstr
            );
        }

        if (functionParamsSymbolTable->count != functionSymbol->membersSymbolTable->count) {
            ReportError(
                syntax->functionDeclarationStmt.leftParen.location,
                "Function '%s' was previously declared with %d parameters wheras new declaration has %d parameters", 
                functionSymbol->name.cstr, functionSymbol->membersSymbolTable->count, functionParamsSymbolTable->count
            );
        }

        for (let int paramIndex = 0; paramIndex < functionParamsSymbolTable->count; paramIndex += 1) {
            let Type paramType = functionParamsSymbolTable->symbols[paramIndex]->type;
            let Type previosType = functionSymbol->membersSymbolTable->symbols[paramIndex]->type;

            if (!TypesIdentical(paramType, previosType)) {
                ReportError(
                    syntax->functionDeclarationStmt.leftParen.location,
                    "Previous function '%s' parameter %d declared type differs from current declared type", 
                    functionSymbol->name.cstr, paramIndex + 1
                );
            }
        }
    }
    functionSymbol->membersSymbolTable = functionParamsSymbolTable;
    functionSymbol->isVariadric = isVariadric;

    let ASTNode* body = nullptr;
    if (syntax->kind == SyntaxKind::FunctionDefinitionStatement) {
        if (isExternal) {
            ReportError(
                identifier.location,
                "Cannot define external function '%s'", 
                TokenGetText(identifier).cstr
            );
        }
        if (functionSymbol->alreadyDefined) {
            ReportError(
                identifier.location,
                "Duplicate function definition of '%s'", 
                TokenGetText(identifier).cstr
            );
        }
        // Bind function body
        binder->symbolTable = functionSymbol->membersSymbolTable;
        binder->currentFunctionSymbol = functionSymbol;
        body = BindBlockStatement(binder, syntax->functionDefinitionStmt.body);
        // TODO: make sure function returns something if its return type is not void
        functionSymbol->alreadyDefined = true;
        binder->currentFunctionSymbol = nullptr;
        binder->symbolTable = binder->symbolTable->parent;
    }

    let ASTNode* result = ASTNodeCreate2(syntax->kind == SyntaxKind::FunctionDeclarationStatement
        ? ASTNodeKind::FunctionDeclarationStatement 
        : ASTNodeKind::FunctionDefinitionStatement, 
        binder->symbolTable, syntax);
    result->symbol = functionSymbol;
    result->left = body;
    return result;
}

fun ASTNode* BindModuleStatement(Binder* binder, SyntaxNode* syntax) {
    switch (syntax->kind) {
        case SyntaxKind::ImportDeclarationStatement:
            return nullptr; // We ignore these as they are only relevant for the binder
        case SyntaxKind::GlobalVariableDeclarationStatement:
            return BindGlobalVariableDefinitionStatement(binder, syntax);
        case SyntaxKind::EnumDeclarationStatement:
        case SyntaxKind::EnumDefinitionStatement:
            return BindEnumDefinitionStatement(binder, syntax);
        case SyntaxKind::StructOrUnionDeclarationStatement:
        case SyntaxKind::StructOrUniontDefinitionStatement:
            return BindStructOrUnionDefinitionStatement(binder, syntax);
        case SyntaxKind::FunctionDeclarationStatement:
        case SyntaxKind::FunctionDefinitionStatement:
            return BindFunctionDefinitionStatement(binder, syntax);
        default:
            assert(false && "Unexpected module statement in binder");
    }
    return nullptr;
}

fun ASTNode* BindModule(Binder* binder, ModuleStatementSyntax* syntax) {
    assert(syntax->info.kind == SyntaxKind::Module);

    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::Module, binder->symbolTable, (as SyntaxNode*)syntax);
    for (let int index = 0; index < syntax->globalStatements.count; index += 1) {
        let SyntaxNode* statement = syntax->globalStatements.nodes[index];
        let ASTNode* boundStatement = BindModuleStatement(binder, statement);
        if (boundStatement != nullptr)
            ASTNodeArrayPush(&result->children, boundStatement);
    }
    return result;
}

fun ASTNode* BindCompilationUnit(Binder* binder, SyntaxTreeArray trees) {
    let ASTNode* result = ASTNodeCreate2(ASTNodeKind::Module, binder->symbolTable, nullptr);
    for (let int index = 0; index < trees.count; index += 1) {
        let SyntaxTree* tree = trees.trees[index];
        let ModuleStatementSyntax* syntax = tree->moduleRoot;
        for (let int index = 0; index < syntax->globalStatements.count; index += 1) {
            let SyntaxNode* statement = syntax->globalStatements.nodes[index];
            let ASTNode* boundStatement = BindModuleStatement(binder, statement);
            if (boundStatement != nullptr)
                ASTNodeArrayPush(&result->children, boundStatement);
        }
    }
    return result;
}