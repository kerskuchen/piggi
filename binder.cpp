#include "definitions.hpp"
// #if 0
// import "definitions.hpp"
// #endif

struct Binder {
    SymbolTable* symbolTable;

    Source source;
    Scanner scanner;
    SyntaxToken tokenPrev;
    SyntaxToken tokenCur;
    SyntaxToken tokenNext;
    SyntaxToken tokenNextAfter;

    int loopLevel;
    int switchCaseLevel;
    Symbol* currentFunctionSymbol;
};

fun Binder BinderCreate(Source source, SymbolTable* symbolTable) {
    let Binder result;
    result.symbolTable = symbolTable;
    result.source = source;
    result.scanner = ScannerCreate(source);
    result.tokenPrev = SyntaxTokenCreateEmpty(source);
    result.tokenCur = NextToken(&result.scanner);
    result.tokenNext = NextToken(&result.scanner);
    result.tokenNextAfter = NextToken(&result.scanner);
    result.loopLevel = 0;
    result.switchCaseLevel = 0;
    result.currentFunctionSymbol = nullptr;
    return result;
}

fun SyntaxToken AdvanceToken(Binder* binder) {
    let SyntaxToken result = binder->tokenCur;
    binder->tokenPrev = binder->tokenCur;
    binder->tokenCur = binder->tokenNext;
    binder->tokenNext = binder->tokenNextAfter;
    binder->tokenNextAfter = NextToken(&binder->scanner);
    return result;
}

fun SyntaxToken MatchAndAdvanceToken(Binder* binder, SyntaxKind kind) {
    if (kind == binder->tokenCur.kind) {
        return AdvanceToken(binder);
    } 

    ReportError(
        TokenGetLocation(binder->tokenCur),
        "Expected token '%s' but got token '%s'", 
        TokenKindToString(kind).cstr, TokenKindToString(binder->tokenCur.kind).cstr
    );
    exit(1);
}

fun ASTNode* BindStatement(Binder* binder);
fun ASTNode* BindExpression(Binder* binder);
fun ASTNode* BindVariableDefinitionStatement(Binder* binder, bool isExternal);
fun Type BindType(Binder* binder);

fun ASTNode* WrapInCompoundStatementIfNecessary(Binder* binder, ASTNode* node) {
    if (node->kind != ASTNodeKind::CompoundStatement) {
        let ASTNode* block = ASTNodeCreate(ASTNodeKind::CompoundStatement, binder->symbolTable, node->token);
        ASTNodeArrayPush(&block->children, node);
        node = block;
    }
    return node;
}

// Turns {{{ a; {b c} d; e; }}} -> { a; {b c} d; e; }
fun ASTNode* FlattenCompoundStatementIfNecessary(Binder* binder, ASTNode* node) {
    if (node->kind == ASTNodeKind::CompoundStatement) {
        if (node->children.count == 1 && node->children.nodes[0]->kind == ASTNodeKind::CompoundStatement) {
            let ASTNode* result = node->children.nodes[0];
            result->symbolTable->parent = node->symbolTable->parent;
            return FlattenCompoundStatementIfNecessary(binder, result);
        }
    }
    return node;
}

fun ASTNode* BindFunctionCallExpression(Binder* binder, ASTNode* left) {
    let SyntaxToken identifier = left->token;
    let String identifierText = TokenGetText(identifier);
    let Symbol* funcSymbol = GetSymbol(binder->symbolTable, identifierText);
    if (funcSymbol == nullptr) {
        ReportError(
            TokenGetLocation(identifier),
            "Undeclared function '%s'", 
            identifierText.cstr
        );
    }
    if (funcSymbol->kind != SymbolKind::Function) {
        ReportError(
            TokenGetLocation(identifier),
            "Identifier '%s' is not a callable function", 
            identifierText.cstr
        );
    }

    let SyntaxToken leftParen = MatchAndAdvanceToken(binder, SyntaxKind::LeftParenToken);
    let ASTNodeArray argumentList = ASTNodeArrayCreate();
    while (binder->tokenCur.kind != SyntaxKind::RightParenToken) {
        let ASTNode* argument = BindExpression(binder);
        ASTNodeArrayPush(&argumentList, argument);
        if (binder->tokenCur.kind == SyntaxKind::CommaToken)
            MatchAndAdvanceToken(binder, SyntaxKind::CommaToken);
        else
            break;
    }
    let SyntaxToken rightParen = MatchAndAdvanceToken(binder, SyntaxKind::RightParenToken);

    if (funcSymbol->isVariadric) {
        if (argumentList.count < funcSymbol->membersSymbolTable->count) {
            ReportError(
                TokenGetLocation(leftParen),
                "Function '%s' expects at least %d arguments but %d arguments were provided", 
                funcSymbol->name.cstr, funcSymbol->membersSymbolTable->count, argumentList.count
            );
        }
    } else {
        if (argumentList.count != funcSymbol->membersSymbolTable->count) {
            ReportError(
                TokenGetLocation(leftParen),
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
                TokenGetLocation(leftParen),
                "Passed incompatible type '%s' for argument %d to function '%s' - expected type '%s'", 
                TypeGetText(argumentType).cstr, argumentIndex + 1, funcSymbol->name.cstr, TypeGetText(expectedType).cstr 
            );
        }
        if (conversion == TypeConversionResult::ExplicitlyConvertible) {
            ReportError(
                TokenGetLocation(leftParen),
                "Cannot implicitly convert type '%s' of argument %d  to expected type '%s' in function call of '%s'", 
                TypeGetText(argumentType).cstr, argumentIndex + 1, TypeGetText(expectedType).cstr, funcSymbol->name.cstr
            );
        }
    }

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::FunccallExpression, binder->symbolTable, identifier);
    result->symbol = funcSymbol;
    result->type = funcSymbol->type;
    result->children = argumentList;
    return result;
}

fun ASTNode* BindArrayIndexingExpression(Binder* binder, ASTNode* left) {
    let SyntaxToken leftBracket = MatchAndAdvanceToken(binder, SyntaxKind::LeftBracketToken);
    let ASTNode* index = BindExpression(binder);
    let SyntaxToken rightBracket = MatchAndAdvanceToken(binder, SyntaxKind::RightBracketToken);

    if (!IsNumberType(index->type)) {
        ReportError(
            TokenGetLocation(leftBracket),
            "Array index after '%s' must be number type", 
            TokenKindToString(leftBracket.kind).cstr
        );
    }
    if (left->symbol == nullptr || TypeGetIndirectionLevel(left->symbol->type) == 0) {
        ReportError(
            TokenGetLocation(leftBracket),
            "Left hand side of array index operator '%s' is not a known array or pointer", 
            TokenKindToString(leftBracket.kind).cstr
        );
    }

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::Arrayindexing, binder->symbolTable, left->token);
    result->type = left->symbol->type;
    if (result->type.isArray)
        result->type.isArray = false; // Because we index it
    else
        result->type = GetBaseTypeForPointerType(result->type); // Because we index it
    result->left = left;
    result->right = index;
    return result;
}

fun ASTNode* BindMemberAccess(Binder* binder, ASTNode* left, bool isArrow) {
    let SyntaxToken accessorToken;
    if (isArrow) 
        accessorToken = MatchAndAdvanceToken(binder, SyntaxKind::ArrowToken);
    else
        accessorToken = MatchAndAdvanceToken(binder, SyntaxKind::DotToken);

    if (left->type.kind != TypeKind::Struct && left->type.kind != TypeKind::Union) {
        ReportError(
            TokenGetLocation(accessorToken),
            "Attempt to access member of non union or struct identifier '%s'", 
            left->symbol->name.cstr
        );
    }

    let Symbol* containerSymbol = GetSymbol(binder->symbolTable, left->type.name);
    assert(containerSymbol != nullptr);
    if (containerSymbol->kind != SymbolKind::Struct && containerSymbol->kind != SymbolKind::Union) {
        ReportError(
            TokenGetLocation(accessorToken),
            "Attempt to access member of non union or struct identifier '%s'", 
            containerSymbol->name.cstr
        );
    }
    if (!containerSymbol->alreadyDefined) {
        ReportError(
            TokenGetLocation(accessorToken),
            "Attempt to access member of forward declared but undefined union or struct '%s'", 
            containerSymbol->name.cstr
        );
    }
    if (isArrow && TypeGetIndirectionLevel(left->type) == 0) {
        ReportError(
            TokenGetLocation(accessorToken),
            "Member access of '%s' with '->' is only allowed for pointer types", 
            containerSymbol->name.cstr
        );
    }
    if (!isArrow && TypeGetIndirectionLevel(left->type) > 0) {
        ReportError(
            TokenGetLocation(accessorToken),
            "Member access of '%s' with '.' is only allowed for non-pointer types", 
            containerSymbol->name.cstr
        );
    }
    
    let SyntaxToken memberIdentifier = MatchAndAdvanceToken(binder, SyntaxKind::IdentifierToken);
    let String identifierText = TokenGetText(memberIdentifier);
    let Symbol* memberSymbol = GetSymbol(containerSymbol->membersSymbolTable, identifierText);
    if (memberSymbol == nullptr) {
        ReportError(
            TokenGetLocation(memberIdentifier),
            "Undeclared struct or union member '%s'", 
            identifierText.cstr
        );
    }
    assert(memberSymbol->kind == SymbolKind::Member);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::Memberaccess, binder->symbolTable, accessorToken);
    result->symbol = memberSymbol;
    result->type = memberSymbol->type;
    result->left = left;
    return result;
}

fun ASTNode* BindEnumLiteralExpression(Binder* binder) {
    let SyntaxToken enumIdentifier = MatchAndAdvanceToken(binder, SyntaxKind::IdentifierToken);
    let SyntaxToken coloncolon = MatchAndAdvanceToken(binder, SyntaxKind::ColonColonToken);
    let SyntaxToken valueIdentifier = MatchAndAdvanceToken(binder, SyntaxKind::IdentifierToken);

    let String enumText = TokenGetText(enumIdentifier);
    let Symbol* enumSymbol = GetSymbol(binder->symbolTable, enumText);
    if (enumSymbol == nullptr) {
        ReportError(
            TokenGetLocation(enumIdentifier),
            "Undeclared identifier '%s'", 
            enumText.cstr
        );
    }
    if (enumSymbol->kind != SymbolKind::Enum) {
        ReportError(
            TokenGetLocation(enumIdentifier),
            "Identifier '%s' is not an enum", 
            enumText.cstr
        );
    }

    let String valueText = TokenGetText(valueIdentifier);
    let Symbol* valueSymbol = GetSymbol(enumSymbol->membersSymbolTable, valueText);
    if (valueSymbol == nullptr) {
        ReportError(
            TokenGetLocation(enumIdentifier),
            "Identifier '%s' is not a member of enum '%s'", 
            valueText.cstr, enumText.cstr
        );
    }
    assert (valueSymbol->kind == SymbolKind::Enumvalue);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::EnumValueLiteral, binder->symbolTable, valueIdentifier);
    result->symbol = valueSymbol;
    result->type = valueSymbol->type;
    return result;
}

fun ASTNode* BindTypeExpression(Binder* binder) {
    let SyntaxToken startToken = binder->tokenCur;
    let Type type = BindType(binder);
    let ASTNode* result = ASTNodeCreate(ASTNodeKind::TypeExpression, binder->symbolTable, startToken);
    result->type = type;
    return result;
}

fun ASTNode* BindPrimaryExpression(Binder* binder) {
    switch (binder->tokenCur.kind) {
        case SyntaxKind::LeftParenToken: {
            let SyntaxToken leftParen = MatchAndAdvanceToken(binder, SyntaxKind::LeftParenToken);
            let ASTNode* inner = BindExpression(binder);
            let SyntaxToken rightParen = MatchAndAdvanceToken(binder, SyntaxKind::RightParenToken);

            let ASTNode* result = ASTNodeCreate(ASTNodeKind::ParenthesizedExpression, binder->symbolTable, leftParen);
            result->type = inner->type;
            result->left = inner;
            return result;
        }
        case SyntaxKind::SizeOfKeyword: {
            let SyntaxToken sizeofKeyword = MatchAndAdvanceToken(binder, SyntaxKind::SizeOfKeyword);
            let SyntaxToken leftParen = MatchAndAdvanceToken(binder, SyntaxKind::LeftParenToken);
            let ASTNode* typeExpr = BindTypeExpression(binder);
            let SyntaxToken rightParen = MatchAndAdvanceToken(binder, SyntaxKind::RightParenToken);

            let ASTNode* result = ASTNodeCreate(ASTNodeKind::SizeOfExpression, binder->symbolTable, sizeofKeyword);
            result->type = TypeCreatePrimitive(TypeKind::PrimitiveInt);
            result->left = typeExpr;
            result->isRValue = true;
            return result;
        }
        case SyntaxKind::IdentifierToken: {
            if (binder->tokenNext.kind == SyntaxKind::ColonColonToken)
                return BindEnumLiteralExpression(binder);

            let SyntaxToken identifier = MatchAndAdvanceToken(binder, SyntaxKind::IdentifierToken);
            let String identifierText = TokenGetText(identifier);
            let Symbol* symbol = GetSymbol(binder->symbolTable, identifierText);
            if (symbol == nullptr) {
                ReportError(
                    TokenGetLocation(identifier),
                    "Undeclared identifier '%s'", 
                    identifierText.cstr
                );
            }

            let ASTNode* result = ASTNodeCreate(ASTNodeKind::Identifier, binder->symbolTable, identifier);
            result->symbol = symbol;
            result->type = symbol->type;
            return result;
        }
        case SyntaxKind::StringLiteralToken: {
            let SyntaxToken token = MatchAndAdvanceToken(binder, SyntaxKind::StringLiteralToken);

            while (binder->tokenCur.kind == SyntaxKind::StringLiteralToken) {
                let SyntaxToken next = MatchAndAdvanceToken(binder, SyntaxKind::StringLiteralToken);
                token.location.end = next.location.end;
                token.debugString = TokenGetText(token);
                token.stringValueWithoutQuotes = StringAppend(token.stringValueWithoutQuotes, next.stringValueWithoutQuotes);
            }

            let ASTNode* result = ASTNodeCreate(ASTNodeKind::StringLiteral, binder->symbolTable, token);
            result->isRValue = true;
            result->stringvalue = token.stringValueWithoutQuotes;
            result->type = TypeCreate(TypeKind::PrimitiveChar, 1, StringCreateEmpty());
            return result;
        }
        case SyntaxKind::NullKeyword: {
            let SyntaxToken token = MatchAndAdvanceToken(binder, SyntaxKind::NullKeyword);
            let ASTNode* result = ASTNodeCreate(ASTNodeKind::NullLiteral, binder->symbolTable, token);
            result->isRValue = true;
            result->type = TypeCreate(TypeKind::PrimitiveNull, 1, StringCreateEmpty());
            return result;
        }
        case SyntaxKind::CharacterLiteralToken: {
            let SyntaxToken token = MatchAndAdvanceToken(binder, SyntaxKind::CharacterLiteralToken);
            let ASTNode* result = ASTNodeCreate(ASTNodeKind::CharacterLiteral, binder->symbolTable, token);
            result->isRValue = true;
            result->intvalue = token.intvalue;
            result->stringvalue = token.stringValueWithoutQuotes;
            result->type = TypeCreatePrimitive(TypeKind::PrimitiveChar);
            return result;
        }
        case SyntaxKind::TrueKeyword: {
            let SyntaxToken token = MatchAndAdvanceToken(binder, SyntaxKind::TrueKeyword);
            let ASTNode* result = ASTNodeCreate(ASTNodeKind::BoolLiteral, binder->symbolTable, token);
            result->isRValue = true;
            result->intvalue = 1;
            result->type = TypeCreatePrimitive(TypeKind::PrimitiveBool);
            return result;
        }
        case SyntaxKind::FalseKeyword: {
            let SyntaxToken token = MatchAndAdvanceToken(binder, SyntaxKind::FalseKeyword);
            let ASTNode* result = ASTNodeCreate(ASTNodeKind::BoolLiteral, binder->symbolTable, token);
            result->isRValue = true;
            result->intvalue = 0;
            result->type = TypeCreatePrimitive(TypeKind::PrimitiveBool);
            return result;
        }
        case SyntaxKind::IntegerLiteralToken:
        default: {
            let SyntaxToken token = MatchAndAdvanceToken(binder, SyntaxKind::IntegerLiteralToken);
            let ASTNode* result = ASTNodeCreate(ASTNodeKind::IntegerLiteral, binder->symbolTable, token);
            result->isRValue = true;
            result->intvalue = token.intvalue;
            if (CHAR_MIN <= token.intvalue && token.intvalue <= CHAR_MAX)
                result->type = TypeCreatePrimitive(TypeKind::PrimitiveByte);
            else if (SHRT_MIN <= token.intvalue && token.intvalue <= SHRT_MAX)
                result->type = TypeCreatePrimitive(TypeKind::PrimitiveShort);
            else if (INT_MIN <= token.intvalue && token.intvalue <= INT_MAX)
                result->type = TypeCreatePrimitive(TypeKind::PrimitiveInt);
            else
                result->type = TypeCreatePrimitive(TypeKind::PrimitiveLong);
            return result;
        }
    }
}

fun ASTNode* BindArrayLiteralExpression(Binder* binder, Symbol* arraySymbol) { 
    assert(arraySymbol->type.isArray);

    let SyntaxToken leftBrace = MatchAndAdvanceToken(binder, SyntaxKind::LeftBraceToken);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::ArrayLiteral, binder->symbolTable, leftBrace);

    let int elementCount = 0;
    while (binder->tokenCur.kind != SyntaxKind::RightBraceToken) {
        let ASTNode* expression = BindExpression(binder);
        elementCount += 1;
        let Type arrayElemType = GetElementTypeForArrayType(arraySymbol->type);
        let TypeConversionResult conversion = CanConvertTypeFromTo(expression->type, arrayElemType);
        if (conversion == TypeConversionResult::NonConvertible 
         || conversion == TypeConversionResult::ExplicitlyConvertible) {
            ReportError(
                TokenGetLocation(expression->token),
                "Cannot convert type '%s' of element %d in array initializer to array type '%s'",
                TypeGetText(expression->type).cstr, elementCount, TypeGetText(arrayElemType).cstr
            );
        } 
        ASTNodeArrayPush(&result->children, expression);
        if (binder->tokenCur.kind == SyntaxKind::RightBraceToken)
            break;
        else
            MatchAndAdvanceToken(binder, SyntaxKind::CommaToken);
    }
    let SyntaxToken rightBrace = MatchAndAdvanceToken(binder, SyntaxKind::RightBraceToken);

    if (arraySymbol->type.arrayElementCount == -1)
        arraySymbol->type.arrayElementCount = elementCount;
    if (elementCount == 0) {
        ReportError(
            TokenGetLocation(leftBrace),
            "Element count cannot be zero in array initializer of array '%s'",
            arraySymbol->name.cstr
        );
    }
    if (arraySymbol->type.arrayElementCount != elementCount) {
        ReportError(
            TokenGetLocation(leftBrace),
            "Element count %d of array initializer does not match element count %d of array '%s'",
            elementCount, arraySymbol->type.arrayElementCount, arraySymbol->name.cstr
        );
    }
    
    return result;
}


fun ASTNode* BindPostFixExpression(Binder* binder) {
    let ASTNode* left = BindPrimaryExpression(binder);

    let bool foundPostfix = false;
    do {
        foundPostfix = false;
        if (binder->tokenCur.kind == SyntaxKind::LeftParenToken) {
            foundPostfix = true;
            left =  BindFunctionCallExpression(binder, left);
        }
        if (binder->tokenCur.kind == SyntaxKind::DotToken) {
            foundPostfix = true;
            left = BindMemberAccess(binder, left, false);
        }
        if (binder->tokenCur.kind == SyntaxKind::ArrowToken) {
            foundPostfix = true;
            left = BindMemberAccess(binder, left, true);
        }
        if (binder->tokenCur.kind == SyntaxKind::LeftBracketToken) {
            foundPostfix = true;
            left = BindArrayIndexingExpression(binder, left);
        }
    } while(foundPostfix);

    if (left->type.isArray) {
        left->isRValue = true; // Arrays cannot be assigned to without indexing
    }

    return left;
}

fun ASTNode* BindBinaryExpression(Binder* binder, int parentPrecedence);
fun ASTNode* BindUnaryExpression(Binder* binder, int parentPrecedence);

fun ASTNode* BindCastExpression(Binder* binder) {
    if (binder->tokenCur.kind != SyntaxKind::LeftParenToken || binder->tokenNext.kind != SyntaxKind::AsKeyword)
        return BindPostFixExpression(binder);
    
    let SyntaxToken leftParen = MatchAndAdvanceToken(binder, SyntaxKind::LeftParenToken);
    let SyntaxToken asKeyword = MatchAndAdvanceToken(binder, SyntaxKind::AsKeyword);
    let Type targetType = BindType(binder);
    let SyntaxToken rightParen = MatchAndAdvanceToken(binder, SyntaxKind::RightParenToken);

    let ASTNode* expression = BindUnaryExpression(binder, 0);

    let TypeConversionResult conversion = CanConvertTypeFromTo(expression->type, targetType);
    if (conversion == TypeConversionResult::NonConvertible) {
        ReportError(
            TokenGetLocation(asKeyword),
            "Cast from type '%s' to type '%s' is impossible",
            TypeGetText(expression->type).cstr, TypeGetText(targetType).cstr
        );
    }

    let ASTNode* result =  ASTNodeCreate(ASTNodeKind::CastExpression, binder->symbolTable, asKeyword);
    result->type = targetType;
    result->left = expression;
    return result;
}

fun ASTNode* BindUnaryExpression(Binder* binder, int parentPrecedence) {
    let ASTNode* left = nullptr;
    let int unaryOperatorPrecedence = GetUnaryOperatorPrecedence(binder->tokenCur.kind);
    if ((unaryOperatorPrecedence != 0) && (unaryOperatorPrecedence >= parentPrecedence)) {
        let SyntaxToken operatorToken = AdvanceToken(binder);
        let ASTNode* operand = BindBinaryExpression(binder, unaryOperatorPrecedence);

        let UnaryOperator op = GetUnaryOperationForToken(operatorToken, operand->type);
        if (op.operandMustBeLValue && operand->isRValue) {
            ReportError(
                TokenGetLocation(operatorToken),
                "Operand of operator '%s' must be an storage location", 
                TokenKindToString(operatorToken.kind).cstr
            );
        }

        let ASTNode* result = ASTNodeCreate(op.operatorKind, binder->symbolTable, operatorToken);
        result->isRValue = op.resultIsRValue;
        result->type = op.resultType;
        result->left = operand;
        left = result;
    } else {
      left = BindCastExpression(binder);
    }
    return left;
}

fun ASTNode* BindBinaryExpression(Binder* binder, int parentPrecedence) {
    let ASTNode* left = BindUnaryExpression(binder, parentPrecedence);

    while (true) {
        let int precedence = GetBinaryOperatorPrecedence(binder->tokenCur.kind);
        if (precedence == 0
         || precedence < parentPrecedence 
         || precedence == parentPrecedence && !IsBinaryOperatorRightAssociative(binder->tokenCur.kind))
            break;

        let SyntaxToken operatorToken = AdvanceToken(binder);
        let ASTNode* right = BindBinaryExpression(binder, precedence);

        let BinaryOperator op = GetBinaryOperationForToken(operatorToken, left->type, right->type);
        if (op.leftMustBeLValue && left->isRValue) {
            ReportError(
                TokenGetLocation(operatorToken),
                "Left argument of operator '%s' must be an storage location", 
                TokenKindToString(operatorToken.kind).cstr
            );
        }
        if (op.rightMustBeLValue && right->isRValue) {
            ReportError(
                TokenGetLocation(operatorToken),
                "Right argument of operator '%s' must be a storage location", 
                TokenKindToString(operatorToken.kind).cstr
            );
        }

        let ASTNode* result = ASTNodeCreate(op.operatorKind, binder->symbolTable, operatorToken);
        result->isRValue = op.resultIsRValue;
        result->type = op.resultType;
        result->left = left;
        result->right = right;
        left = result;
    }
    return left;
}

fun ASTNode* BindTernaryConditionExpression(Binder* binder) {
    let ASTNode* condition = BindBinaryExpression(binder, 0);

    if (binder->tokenCur.kind == SyntaxKind::QuestionmarkToken) {
        // TODO: check truthyness of condition
        let SyntaxToken questionmark = MatchAndAdvanceToken(binder, SyntaxKind::QuestionmarkToken);
        let ASTNode* thenExpression = BindTernaryConditionExpression(binder);
        let SyntaxToken colon = MatchAndAdvanceToken(binder, SyntaxKind::ColonToken);
        let ASTNode* elseExpression = BindTernaryConditionExpression(binder);

        let Type type = GetTypeThatFitsBothTypes(thenExpression->type, elseExpression->type);
        if (IsVoidType(type)) {
            ReportError(
                TokenGetLocation(questionmark),
                "Incompatible expression types in ternary operator - then branch: '%s', else branch: '%s'",
                TypeGetText(thenExpression->type).cstr, TypeGetText(elseExpression->type).cstr
            );
        }

        let ASTNode* ternary = ASTNodeCreate(ASTNodeKind::TernaryConditionalExpression, binder->symbolTable, questionmark);
        ternary->left = condition;
        ternary->right = thenExpression;
        ternary->extra1 = elseExpression;
        ternary->type = type;
        ternary->isRValue = true;
        condition = ternary;
    }
    return condition;
}

fun ASTNode* BindAssignmentExpression(Binder* binder) {
    let ASTNode* left = BindTernaryConditionExpression(binder);

    // NOTE: The following is basically a duplicate of what the BindBinaryExpression does.
    // We need to do this here though to support ternary conditionals more easily.
    // Note that we still keep the GetBinaryOperationForToken(..) call so that we later 
    // can accept custom operator overloadings.
    switch (binder->tokenCur.kind) {
        case SyntaxKind::EqualsToken: 
        case SyntaxKind::PlusEqualsToken: 
        case SyntaxKind::MinusEqualsToken: 
        case SyntaxKind::StarEqualsToken: 
        case SyntaxKind::SlashEqualsToken:
        case SyntaxKind::PercentEqualsToken:
        case SyntaxKind::HatEqualsToken:
        case SyntaxKind::AmpersandEqualsToken:
        case SyntaxKind::PipeEqualsToken:
        case SyntaxKind::LessLessEqualsToken:
        case SyntaxKind::GreaterGreaterEqualsToken:
        {
            let SyntaxToken assignmentToken = AdvanceToken(binder);
            let ASTNode* right = BindAssignmentExpression(binder);
            let BinaryOperator op = GetBinaryOperationForToken(assignmentToken, left->type, right->type);
            if (left->isRValue) {
                ReportError(
                    TokenGetLocation(assignmentToken),
                    "Left argument of operator '%s' must be an storage location", 
                    TokenKindToString(assignmentToken.kind).cstr
                );
            }

            let ASTNode* assignment = ASTNodeCreate(op.operatorKind, binder->symbolTable, assignmentToken);
            assignment->isRValue = op.resultIsRValue;
            assignment->type = op.resultType;
            assignment->left = left;
            assignment->right = right;
            left = assignment;
            break;
        } 
        default:
            break;
    }
    
    return left;
}

fun ASTNode* BindExpression(Binder* binder) {
    return BindAssignmentExpression(binder);
}

fun ASTNode* BindExpressionStatement(Binder* binder) {
    let ASTNode* expression = BindExpression(binder);
    let SyntaxToken semicolonToken = MatchAndAdvanceToken(binder, SyntaxKind::SemicolonToken);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::ExpressionStatement, binder->symbolTable, expression->token);
    result->left = expression;
    return result;
}

fun ASTNode* BindIfStatement(Binder* binder) {
    let SyntaxToken ifKeyword = MatchAndAdvanceToken(binder, SyntaxKind::IfKeyword);

    let SyntaxToken leftParen = MatchAndAdvanceToken(binder, SyntaxKind::LeftParenToken);
    let ASTNode* condition = BindExpression(binder);
    // TODO we should check that conditions is truthy
    let SyntaxToken rightParen = MatchAndAdvanceToken(binder, SyntaxKind::RightParenToken);

    let ASTNode* thenBranch = BindStatement(binder);
    thenBranch = WrapInCompoundStatementIfNecessary(binder, thenBranch);

    let ASTNode* elseBranch = nullptr;
    if (binder->tokenCur.kind == SyntaxKind::ElseKeyword) {
        let SyntaxToken elseKeyword = MatchAndAdvanceToken(binder, SyntaxKind::ElseKeyword);
        elseBranch = BindStatement(binder);
        elseBranch = WrapInCompoundStatementIfNecessary(binder, elseBranch);
    }

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::IfStatement, binder->symbolTable, ifKeyword);
    result->left = condition;
    result->right = thenBranch;
    result->extra1 = elseBranch;
    return result;
}

fun ASTNode* BindWhileStatement(Binder* binder) {
    let SyntaxToken whileKeyword = MatchAndAdvanceToken(binder, SyntaxKind::WhileKeyword);

    let SyntaxToken leftParen = MatchAndAdvanceToken(binder, SyntaxKind::LeftParenToken);
    let ASTNode* condition = BindExpression(binder);
    // TODO we should check that conditions is truthy
    let SyntaxToken rightParen = MatchAndAdvanceToken(binder, SyntaxKind::RightParenToken);

    binder->loopLevel += 1;
    let ASTNode* body = BindStatement(binder);
    body = WrapInCompoundStatementIfNecessary(binder, body);
    binder->loopLevel -= 1;

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::WhileStatement, binder->symbolTable, whileKeyword);
    result->left = condition;
    result->right = body;
    return result;
}

fun ASTNode* BindDoWhileStatement(Binder* binder) {
    let SyntaxToken doKeyword = MatchAndAdvanceToken(binder, SyntaxKind::DoKeyword);
    binder->loopLevel += 1;
    let ASTNode* body = BindStatement(binder);
    body = WrapInCompoundStatementIfNecessary(binder, body);
    binder->loopLevel -= 1;

    let SyntaxToken whileKeyword = MatchAndAdvanceToken(binder, SyntaxKind::WhileKeyword);

    let SyntaxToken leftParen = MatchAndAdvanceToken(binder, SyntaxKind::LeftParenToken);
    let ASTNode* condition = BindExpression(binder);
    // TODO we should check that conditions is truthy
    let SyntaxToken rightParen = MatchAndAdvanceToken(binder, SyntaxKind::RightParenToken);
    let SyntaxToken semicolon = MatchAndAdvanceToken(binder, SyntaxKind::SemicolonToken);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::DoWhileStatement, binder->symbolTable, whileKeyword);
    result->left = condition;
    result->right = body;
    return result;
}

fun ASTNode* BindForStatement(Binder* binder) {
    let SyntaxToken forKeyword = MatchAndAdvanceToken(binder, SyntaxKind::ForKeyword);

    // Push symboltable scope to make the index local to the for statement
    binder->symbolTable = SymbolTableCreate(binder->symbolTable);
    let ASTNode* result = ASTNodeCreate(ASTNodeKind::ForStatement, binder->symbolTable, forKeyword);

    let SyntaxToken leftParen = MatchAndAdvanceToken(binder, SyntaxKind::LeftParenToken);

    let ASTNode* initializer = nullptr;
    if (binder->tokenCur.kind == SyntaxKind::LetKeyword)
        initializer = BindVariableDefinitionStatement(binder, false);
    else 
        initializer = BindExpressionStatement(binder);
    let ASTNode* condition = BindExpressionStatement(binder);
    // TODO we should check that conditions is truthy
    let ASTNode* iterator = BindExpression(binder);

    let SyntaxToken rightParen = MatchAndAdvanceToken(binder, SyntaxKind::RightParenToken);

    binder->loopLevel += 1;
    let ASTNode* body = BindStatement(binder);
    body = WrapInCompoundStatementIfNecessary(binder, body);
    binder->loopLevel -= 1;

    // Pop symboltable
    binder->symbolTable = binder->symbolTable->parent;

    result->left = initializer;
    result->right = condition;
    result->extra1 = iterator;
    result->extra2 = body;
    return result;
}

fun ASTNode* BindReturnStatement(Binder* binder) {
    let SyntaxToken returnKeyword = MatchAndAdvanceToken(binder, SyntaxKind::ReturnKeyword);
    if (binder->currentFunctionSymbol == nullptr) {
        ReportError(
            TokenGetLocation(returnKeyword),
            "Invalid 'return' statement found outside of function definition"
        );
    }

    let Type functionReturnType = binder->currentFunctionSymbol->type;
    if (IsVoidType(functionReturnType) && binder->tokenCur.kind != SyntaxKind::SemicolonToken) {
        ReportError(
            TokenGetLocation(returnKeyword),
            "Invalid return expression in void function"
        );
    }
    if (!IsVoidType(functionReturnType) && binder->tokenCur.kind == SyntaxKind::SemicolonToken) {
        ReportError(
            TokenGetLocation(returnKeyword),
            "Must return expression in non-void function"
        );
    }

    let ASTNode* expression = nullptr;
    if (binder->tokenCur.kind != SyntaxKind::SemicolonToken) {
        expression = BindExpression(binder);

        let TypeConversionResult conversion = CanConvertTypeFromTo(expression->type, functionReturnType);
        if (conversion == TypeConversionResult::NonConvertible) {
            ReportError(
                TokenGetLocation(returnKeyword),
                "Incompatible types for return expression '%s'", 
                TokenKindToString(returnKeyword.kind).cstr
            );
        }
        if (conversion == TypeConversionResult::ExplicitlyConvertible) {
            ReportError(
                TokenGetLocation(returnKeyword),
                "Types cannot be implicitly converted for return expression '%s'", 
                TokenKindToString(returnKeyword.kind).cstr
            );
        }
    }
    let SyntaxToken semicolonToken = MatchAndAdvanceToken(binder, SyntaxKind::SemicolonToken);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::ReturnStatement, binder->symbolTable, returnKeyword);
    result->left = expression;
    return result;
}

fun ASTNode* BindBreakStatement(Binder* binder) {
    let SyntaxToken breakKeyword = MatchAndAdvanceToken(binder, SyntaxKind::BreakKeyword);
    if (binder->loopLevel == 0 && binder->switchCaseLevel == 0) {
        ReportError(
            TokenGetLocation(breakKeyword),
            "Invalid 'break' statement found outside of loop or switch-case definition"
        );
    }
    let SyntaxToken semicolonToken = MatchAndAdvanceToken(binder, SyntaxKind::SemicolonToken);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::BreakStatement, binder->symbolTable, breakKeyword);
    return result;
}

fun ASTNode* BindContinueStatement(Binder* binder) {
    let SyntaxToken continueKeyword = MatchAndAdvanceToken(binder, SyntaxKind::ContinueKeyword);
    if (binder->loopLevel == 0) {
        ReportError(
            TokenGetLocation(continueKeyword),
            "Invalid 'continue' statement found outside of loop definition"
        );
    }
    let SyntaxToken semicolonToken = MatchAndAdvanceToken(binder, SyntaxKind::SemicolonToken);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::ContinueStatement, binder->symbolTable, continueKeyword);
    return result;
}


fun Type BindType(Binder* binder) {
    let Type type = TypeCreateVoid();
    let SyntaxToken startToken = binder->tokenCur;
    switch (binder->tokenCur.kind) {
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
                let String name = TokenGetText(binder->tokenCur);
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
                TokenGetLocation(binder->tokenCur),
                "SyntaxToken '%s' is not a type", 
                TokenGetText(binder->tokenCur).cstr
            );
    }
    AdvanceToken(binder);

    while (binder->tokenCur.kind == SyntaxKind::StarToken) {
        AdvanceToken(binder);
        type = GetPointerTypeForBaseType(type);
    }

    if (type.kind == TypeKind::Struct) {
        let Symbol* symbol = GetSymbol(binder->symbolTable, type.name);
        if (!symbol->alreadyDefined && type.baseIndirectionLevel == 0) {
            ReportError(
                TokenGetLocation(startToken),
                "Usage of undefined but forward declared type '%s' is only allowed as pointer", 
                type.name.cstr
            );
        }
    }

    return type;
}

fun ASTNode* BindCompoundStatement(Binder* binder, bool inSwitch) {
    let bool startsWithBrace = false;

    let SyntaxToken leftBrace = binder->tokenCur;
    if (binder->tokenCur.kind == SyntaxKind::LeftBraceToken || !inSwitch) {
        leftBrace = MatchAndAdvanceToken(binder, SyntaxKind::LeftBraceToken);
        startsWithBrace = true;
    }

    // Push symboltable scope
    binder->symbolTable = SymbolTableCreate(binder->symbolTable);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::CompoundStatement, binder->symbolTable, leftBrace);

    while (binder->tokenCur.kind != SyntaxKind::RightBraceToken) {
        if (inSwitch && binder->tokenCur.kind == SyntaxKind::CaseKeyword)
            break;
        if (inSwitch && binder->tokenCur.kind == SyntaxKind::DefaultKeyword)
            break;
        let ASTNode* statement = BindStatement(binder);
        ASTNodeArrayPush(&result->children, statement);
    }

    // Pop symboltable
    binder->symbolTable = binder->symbolTable->parent;

    if (startsWithBrace) {
        // Then it also must end with a brace
        let SyntaxToken rightBrace = MatchAndAdvanceToken(binder, SyntaxKind::RightBraceToken);
    }

    return FlattenCompoundStatementIfNecessary(binder, result);
}

fun ASTNode* BindVariableDeclarationWithoutTerminator(Binder* binder, Type type, SyntaxToken identifier, SymbolScopeKind symbolScopeKind) {
    let Symbol* varSymbol = AddSymbol(binder->symbolTable, TokenGetText(identifier), SymbolKind::Variable, symbolScopeKind, type);
    if (varSymbol == nullptr) {
        ReportError(
            TokenGetLocation(identifier),
            "Symbol was '%s' already declared in current scope", 
            TokenGetText(identifier).cstr
        );
    }

    if (IsVoidType(type)) {
        ReportError(
            TokenGetLocation(identifier),
            "'void' not allowed as variables '%s' storage type", 
            TokenGetText(identifier).cstr
        );
    }

    if (binder->tokenCur.kind == SyntaxKind::LeftBracketToken) {
        // Array definition
        let longint arrayElementCount = -1;
        let SyntaxToken leftBracket = MatchAndAdvanceToken(binder, SyntaxKind::LeftBracketToken);
        if (binder->tokenCur.kind == SyntaxKind::IntegerLiteralToken) {
            let SyntaxToken intLiteral = MatchAndAdvanceToken(binder, SyntaxKind::IntegerLiteralToken);
            arrayElementCount = intLiteral.intvalue;
        }
        let SyntaxToken rightBracket = MatchAndAdvanceToken(binder, SyntaxKind::RightBracketToken);

        if (arrayElementCount == 0) {
            ReportError(
                TokenGetLocation(identifier),
                "Array size cannot be zero for '%s'", 
                TokenGetText(identifier).cstr
            );
        }

        varSymbol->type.isArray = true;
        varSymbol->type.arrayElementCount = arrayElementCount;
    }

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::VariableDeclarationStatement, binder->symbolTable, identifier);
    result->symbol = varSymbol;
    return result;
}

fun ASTNode* BindUnionOrStructDefinitionStatement(Binder* binder, bool isExternal) {
    let SymbolScopeKind symbolScopeKind = isExternal ? SymbolScopeKind::Extern : SymbolScopeKind::Global;

    let SyntaxToken structKeyword;
    if (binder->tokenCur.kind == SyntaxKind::StructKeyword)
        structKeyword = MatchAndAdvanceToken(binder, SyntaxKind::StructKeyword);
    else 
        structKeyword = MatchAndAdvanceToken(binder, SyntaxKind::UnionKeyword);
    let bool isUnion = structKeyword.kind == SyntaxKind::UnionKeyword;

    let SyntaxToken identifier = MatchAndAdvanceToken(binder, SyntaxKind::IdentifierToken);
    let String name = TokenGetText(identifier);

    if (binder->currentFunctionSymbol != nullptr) {
        ReportError(
            TokenGetLocation(identifier),
            "Unexpected struct or union declaration of '%s' while already parsing function", 
            name.cstr
        );
    }

    let Symbol* structSymbol = GetSymbol(binder->symbolTable, name);
    if (structSymbol != nullptr) {
        if (isUnion &&  structSymbol->kind != SymbolKind::Union) {
            ReportError(
                TokenGetLocation(identifier),
                "Another symbol with the same name '%s' but different type was already declared in current scope", 
                TokenGetText(identifier).cstr
            );
        }
        if (!isUnion &&  structSymbol->kind != SymbolKind::Struct) {
            ReportError(
                TokenGetLocation(identifier),
                "Another symbol with the same name '%s' but different type was already declared in current scope", 
                TokenGetText(identifier).cstr
            );
        }
        if (structSymbol->scopeKind != symbolScopeKind) {
            ReportError(
                TokenGetLocation(identifier),
                "Struct or union '%s was previously declared but with different scope attribute", 
                TokenGetText(identifier).cstr
            );
        }
    }
    if (structSymbol == nullptr) {
        let Type type = TypeCreate(isUnion ? TypeKind::Union : TypeKind::Struct, 0, name);
        structSymbol = AddSymbol(binder->symbolTable, name, isUnion ? SymbolKind::Union : SymbolKind::Struct, symbolScopeKind, type);
    }

    if (binder->tokenCur.kind == SyntaxKind::SemicolonToken) {
        MatchAndAdvanceToken(binder, SyntaxKind::SemicolonToken);
        let ASTNode* result = ASTNodeCreate(
            isUnion ? ASTNodeKind::UnionDeclarationStatement : ASTNodeKind::StructDeclarationStatement, binder->symbolTable, identifier);
        result->symbol = structSymbol;
        return result;
    } else {
        structSymbol->membersSymbolTable = SymbolTableCreate(binder->symbolTable);

        // Push union/struct member symboltable scope to parse variable declarations as members
        binder->symbolTable = structSymbol->membersSymbolTable;

        let SyntaxToken leftBrace = MatchAndAdvanceToken(binder, SyntaxKind::LeftBraceToken);
        while (binder->tokenCur.kind != SyntaxKind::RightBraceToken) {
            let Type memberType = BindType(binder);
            let SyntaxToken memberIdent = MatchAndAdvanceToken(binder, SyntaxKind::IdentifierToken);
            let ASTNode* memberNode = BindVariableDeclarationWithoutTerminator(binder, memberType, memberIdent, SymbolScopeKind::Local);
            memberNode->symbol->kind = SymbolKind::Member;
            MatchAndAdvanceToken(binder, SyntaxKind::SemicolonToken);
        }
        let SyntaxToken rightBrace = MatchAndAdvanceToken(binder, SyntaxKind::RightBraceToken);

        // Pop symboltable
        binder->symbolTable = binder->symbolTable->parent;

        if (structSymbol->membersSymbolTable->count == 0){
            ReportError(
                TokenGetLocation(identifier),
                "Struct or union '%s' needs at least one member", 
                name.cstr
            );
        }

        MatchAndAdvanceToken(binder, SyntaxKind::SemicolonToken);

        if (structSymbol->alreadyDefined) {
            ReportError(
                TokenGetLocation(identifier),
                "Duplicate struct or union definition of '%s'", 
                name.cstr
            );
        }
        structSymbol->alreadyDefined = true;

        let ASTNode* result = ASTNodeCreate(
            isUnion ? ASTNodeKind::UnionDefinitionStatement : ASTNodeKind::StructDefinitionStatement, binder->symbolTable, identifier);
        result->symbol = structSymbol;
        return result;
    }
}

fun ASTNode* BindEnumDefinitionStatement(Binder* binder, bool isExternal) {
    let SymbolScopeKind symbolScopeKind = isExternal ? SymbolScopeKind::Extern : SymbolScopeKind::Global;

    let SyntaxToken enumKeyword = MatchAndAdvanceToken(binder, SyntaxKind::EnumKeyword);
    let SyntaxToken classKeyword = MatchAndAdvanceToken(binder, SyntaxKind::ClassKeyword);
    let SyntaxToken identifier = MatchAndAdvanceToken(binder, SyntaxKind::IdentifierToken);
    let String name = TokenGetText(identifier);

    if (binder->currentFunctionSymbol != nullptr) {
        ReportError(
            TokenGetLocation(identifier),
            "Unexpected enum declaration of '%s' while already parsing function", 
            name.cstr
        );
    }

    let Symbol* enumSymbol = GetSymbol(binder->symbolTable, name);
    if (enumSymbol != nullptr) {
        if (enumSymbol->kind != SymbolKind::Enum) {
            ReportError(
                TokenGetLocation(identifier),
                "Another symbol with the same name '%s' but different type was already declared in current scope",
                TokenGetText(identifier).cstr
            );
        }
        if (enumSymbol->scopeKind != symbolScopeKind) {
            ReportError(
                TokenGetLocation(identifier),
                "Enum '%s was previously declared but with different scope attribute", 
                TokenGetText(identifier).cstr
            );
        }
    }
    if (enumSymbol == nullptr) {
        let Type type = TypeCreate(TypeKind::Enum, 0, name);
        enumSymbol = AddSymbol(binder->symbolTable, name, SymbolKind::Enum, symbolScopeKind, type);
    }

    if (binder->tokenCur.kind == SyntaxKind::SemicolonToken) {
        MatchAndAdvanceToken(binder, SyntaxKind::SemicolonToken);
        let ASTNode* result = ASTNodeCreate(ASTNodeKind::EnumDeclarationStatement, binder->symbolTable, identifier);
        result->symbol = enumSymbol;
        return result;
    } else {
        enumSymbol->membersSymbolTable = SymbolTableCreate(binder->symbolTable);

        // Push enum member symboltable scope to parse declarations as members
        binder->symbolTable = enumSymbol->membersSymbolTable;

        let longint valueCounter = 0;
        let SyntaxToken leftBrace = MatchAndAdvanceToken(binder, SyntaxKind::LeftBraceToken);
        while (binder->tokenCur.kind != SyntaxKind::RightBraceToken) {
            let Type memberType = TypeCreate(TypeKind::Enum, 0, enumSymbol->name);
            let SyntaxToken memberIdent = MatchAndAdvanceToken(binder, SyntaxKind::IdentifierToken);
            let ASTNode* memberNode = BindVariableDeclarationWithoutTerminator(binder, memberType, memberIdent, SymbolScopeKind::Local);
            if (binder->tokenCur.kind == SyntaxKind::EqualsToken) {
                let SyntaxToken equalsToken = MatchAndAdvanceToken(binder, SyntaxKind::EqualsToken);
                let SyntaxToken valueToken = MatchAndAdvanceToken(binder, SyntaxKind::IntegerLiteralToken);
                if (valueToken.intvalue < valueCounter)
                {
                    ReportError(
                        TokenGetLocation(identifier),
                        "Assigned value of enum value literal '%s' must chosen such that all enum values of '%s' are unique - chosen value '%lld' would lead to duplicates", 
                        TokenGetText(memberIdent).cstr, enumSymbol->name.cstr, valueToken.intvalue
                    );
                }
                valueCounter = valueToken.intvalue;
            }
            memberNode->symbol->kind = SymbolKind::Enumvalue;
            memberNode->symbol->enumValue = valueCounter;
            if (binder->tokenCur.kind == SyntaxKind::RightBraceToken)
                break;
            MatchAndAdvanceToken(binder, SyntaxKind::CommaToken);
            valueCounter += 1;
        }
        let SyntaxToken rightBrace = MatchAndAdvanceToken(binder, SyntaxKind::RightBraceToken);

        // Pop symboltable
        binder->symbolTable = binder->symbolTable->parent;

        if (enumSymbol->membersSymbolTable->count == 0){
            ReportError(
                TokenGetLocation(identifier),
                "Enum '%s' needs at least one member", 
                name.cstr
            );
        }

        MatchAndAdvanceToken(binder, SyntaxKind::SemicolonToken);

        if (enumSymbol->alreadyDefined) {
            ReportError(
                TokenGetLocation(identifier),
                "Duplicate enum definition of '%s'", 
                name.cstr
            );
        }
        enumSymbol->alreadyDefined = true;

        let ASTNode* result = ASTNodeCreate(ASTNodeKind::EnumDefinitionStatement, binder->symbolTable, identifier);
        result->symbol = enumSymbol;
        return result;
    }
}

fun Symbol* BindFunctionDeclarationStatementWithoutTerminator(Binder* binder, Type returnType, SyntaxToken identifier, bool isExternal) {
    if (binder->currentFunctionSymbol != nullptr) {
        ReportError(
            TokenGetLocation(identifier),
            "Unexpected function declaration of '%s' while already parsing function", 
            TokenGetText(identifier).cstr
        );
    }

    let SymbolScopeKind symbolScopeKind = isExternal ? SymbolScopeKind::Extern : SymbolScopeKind::Global;

    let Symbol* functionSymbol = GetSymbol(binder->symbolTable, TokenGetText(identifier));
    if (functionSymbol != nullptr) {
        if (functionSymbol->kind != SymbolKind::Function) {
            ReportError(
                TokenGetLocation(identifier),
                "Another symbol with the same name '%s' but different type was already declared in current scope", 
                TokenGetText(identifier).cstr
            );
        }
        if (functionSymbol->scopeKind != symbolScopeKind) {
            ReportError(
                TokenGetLocation(identifier),
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
    let SyntaxToken leftParen = MatchAndAdvanceToken(binder, SyntaxKind::LeftParenToken);
    while (binder->tokenCur.kind != SyntaxKind::RightParenToken) {
        if (binder->tokenCur.kind == SyntaxKind::DotDotDotToken) {
            MatchAndAdvanceToken(binder, SyntaxKind::DotDotDotToken);
            isVariadric = true;
            break;
        }
        let Type paramType = BindType(binder);
        let SyntaxToken paramIdent = MatchAndAdvanceToken(binder, SyntaxKind::IdentifierToken);
        let ASTNode* paramNode = BindVariableDeclarationWithoutTerminator(binder, paramType, paramIdent, SymbolScopeKind::Local);
        paramNode->symbol->kind = SymbolKind::Parameter;
        if (binder->tokenCur.kind == SyntaxKind::CommaToken)
            MatchAndAdvanceToken(binder, SyntaxKind::CommaToken);
        else
            break;
    }
    let SyntaxToken rightParen = MatchAndAdvanceToken(binder, SyntaxKind::RightParenToken);

    // Pop symboltable
    binder->symbolTable = binder->symbolTable->parent;

    if (functionSymbol->membersSymbolTable != nullptr) {
        // The function was already declared before, we need to make sure its types and 
        // parameters match up with the previous defined type and parameters
        if (!TypesIdentical(returnType, functionSymbol->type)) {
            ReportError(
                TokenGetLocation(identifier),
                "Return type of function '%s' does not match return type of a previous declaration", 
                TokenGetText(identifier).cstr
            );
        }

        if (functionSymbol->isVariadric != isVariadric) {
            ReportError(
                TokenGetLocation(identifier),
                "Vadriaticity of function '%s' does not match with a previous declaration", 
                TokenGetText(identifier).cstr
            );
        }

        if (functionParamsSymbolTable->count != functionSymbol->membersSymbolTable->count) {
            ReportError(
                TokenGetLocation(leftParen),
                "Function '%s' was previously declared with %d parameters wheras new declaration has %d parameters", 
                functionSymbol->name.cstr, functionSymbol->membersSymbolTable->count, functionParamsSymbolTable->count
            );
        }

        for (let int paramIndex = 0; paramIndex < functionParamsSymbolTable->count; paramIndex += 1) {
            let Type paramType = functionParamsSymbolTable->symbols[paramIndex]->type;
            let Type previosType = functionSymbol->membersSymbolTable->symbols[paramIndex]->type;

            if (!TypesIdentical(paramType, previosType)) {
                ReportError(
                    TokenGetLocation(leftParen),
                    "Previous function '%s' parameter %d declared type differs from current declared type", 
                    functionSymbol->name.cstr, paramIndex + 1
                );
            }
        }
    }
    functionSymbol->membersSymbolTable = functionParamsSymbolTable;
    functionSymbol->isVariadric = isVariadric;

    return functionSymbol;
}

fun ASTNode* BindFunctionDefinitionStatement(Binder* binder, bool isExternal) {
    let SyntaxToken funKeyword = MatchAndAdvanceToken(binder, SyntaxKind::FunKeyword);
    let Type type = BindType(binder);
    let SyntaxToken identifier = MatchAndAdvanceToken(binder, SyntaxKind::IdentifierToken);
    let Symbol* functionSymbol = BindFunctionDeclarationStatementWithoutTerminator(binder, type, identifier, isExternal);

    let ASTNode* body = nullptr;
    if (binder->tokenCur.kind == SyntaxKind::SemicolonToken) {
        // Just a forward declaration
        MatchAndAdvanceToken(binder, SyntaxKind::SemicolonToken);
    } else {
        if (isExternal) {
            ReportError(
                TokenGetLocation(identifier),
                "Cannot define external function '%s'", 
                TokenGetText(identifier).cstr
            );
        }
        if (functionSymbol->alreadyDefined) {
            ReportError(
                TokenGetLocation(identifier),
                "Duplicate function definition of '%s'", 
                TokenGetText(identifier).cstr
            );
        }
        // Bind function body
        binder->symbolTable = functionSymbol->membersSymbolTable;
        binder->currentFunctionSymbol = functionSymbol;
        body = BindCompoundStatement(binder, false);
        // TODO: make sure function returns something if its return type is not void
        functionSymbol->alreadyDefined = true;
        binder->currentFunctionSymbol = nullptr;
        binder->symbolTable = binder->symbolTable->parent;
    }

    let ASTNode* result = ASTNodeCreate(
        body == nullptr ? ASTNodeKind::FunctionDeclarationStatement : ASTNodeKind::FunctionDefinitionStatement, 
        binder->symbolTable, identifier);
    result->symbol = functionSymbol;
    result->left = body;
    return result;
}

fun ASTNode* BindVariableDefinitionStatement(Binder* binder, bool isExternal) {
    let SyntaxToken letKeyword = MatchAndAdvanceToken(binder, SyntaxKind::LetKeyword);

    let bool isLocalPersist = false;
    if (binder->tokenCur.kind == SyntaxKind::LocalPersistKeyword) {
        let SyntaxToken localPersist = MatchAndAdvanceToken(binder, SyntaxKind::LocalPersistKeyword);
        isLocalPersist = true;
    }

    let Type type = BindType(binder);
    let SyntaxToken identifier = MatchAndAdvanceToken(binder, SyntaxKind::IdentifierToken);

    let SymbolScopeKind symbolScopeKind = binder->currentFunctionSymbol == nullptr
        ? SymbolScopeKind::Global 
        : SymbolScopeKind::Local;
    
    if (isLocalPersist) {
        if (symbolScopeKind != SymbolScopeKind::Local) {
            ReportError(
                TokenGetLocation(identifier),
                "Cannot mark global variable as localpersist '%s'", 
                TokenGetText(identifier).cstr
            );
        }
        symbolScopeKind = SymbolScopeKind::LocalPersist;
    }
    if (isExternal) {
        if (symbolScopeKind != SymbolScopeKind::Global) {
            ReportError(
                TokenGetLocation(identifier),
                "Cannot mark local variable as external '%s'", 
                TokenGetText(identifier).cstr
            );
        }
        symbolScopeKind = SymbolScopeKind::Extern;
    }

    let ASTNode* result = BindVariableDeclarationWithoutTerminator(binder, type, identifier, symbolScopeKind);

    if (binder->tokenCur.kind == SyntaxKind::EqualsToken) {
        let SyntaxToken equalsToken = MatchAndAdvanceToken(binder, SyntaxKind::EqualsToken);
        let ASTNode* initializer = nullptr;
        if (result->symbol->type.isArray)
            initializer = BindArrayLiteralExpression(binder, result->symbol);
        else
            initializer = BindExpression(binder);
        result->left = initializer;
    } 
    let SyntaxToken semicolonToken = MatchAndAdvanceToken(binder, SyntaxKind::SemicolonToken);
    return result;
}

fun ASTNode* BindDefinitionStatement(Binder* binder) {
    let bool isExternal = false;
    if (binder->tokenCur.kind == SyntaxKind::ExternKeyword) {
        isExternal = true;
        AdvanceToken(binder);
    }

    if (binder->tokenCur.kind == SyntaxKind::EnumKeyword)
        return BindEnumDefinitionStatement(binder, isExternal);
    else if (binder->tokenCur.kind == SyntaxKind::StructKeyword || binder->tokenCur.kind == SyntaxKind::UnionKeyword)
        return BindUnionOrStructDefinitionStatement(binder, isExternal);
    else if (binder->tokenCur.kind == SyntaxKind::FunKeyword)
        return BindFunctionDefinitionStatement(binder, isExternal);
    else 
        return BindVariableDefinitionStatement(binder, isExternal);
}

fun ASTNode* BindCaseStatement(Binder* binder, ASTNode* switchExpression) {
    let SyntaxToken caseLabel = SyntaxTokenCreateEmpty(binder->source);
    let ASTNode* caseExpression = nullptr;

    if (binder->tokenCur.kind == SyntaxKind::CaseKeyword) {
        caseLabel = MatchAndAdvanceToken(binder, SyntaxKind::CaseKeyword);
        if (binder->switchCaseLevel == 0) {
            ReportError(
                TokenGetLocation(caseLabel), 
                "Unexpected case label outside of switch statement"
            );
        }

        caseExpression = BindExpression(binder);
        if (caseExpression->kind != ASTNodeKind::IntegerLiteral 
        && caseExpression->kind != ASTNodeKind::StringLiteral 
        && caseExpression->kind != ASTNodeKind::CharacterLiteral 
        && caseExpression->kind != ASTNodeKind::EnumValueLiteral) {
            ReportError(
                TokenGetLocation(caseExpression->token), 
                "Expected literal in case label but got '%s'",
                TokenKindToString(caseExpression->token.kind).cstr
            );
        }
        let TypeConversionResult conversion = CanConvertTypeFromTo(caseExpression->type, switchExpression->type);
        if (conversion != TypeConversionResult::Identical && conversion != TypeConversionResult::ImplictlyConvertible) {
            ReportError(
                TokenGetLocation(caseExpression->token), 
                "Cannot convert type '%s' of case label literal '%s' to its switch expression type '%s'",
                TypeGetText(caseExpression->type).cstr, TokenGetText(caseExpression->token).cstr, TypeGetText(switchExpression->type).cstr
            );
        }
    } else {
        caseLabel = MatchAndAdvanceToken(binder, SyntaxKind::DefaultKeyword);
        if (binder->switchCaseLevel == 0) {
            ReportError(
                TokenGetLocation(caseLabel), 
                "Unexpected default case label outside of switch statement"
            );
        }
    }
    let SyntaxToken colonToken = MatchAndAdvanceToken(binder, SyntaxKind::ColonToken);

    let ASTNode* body = BindCompoundStatement(binder, true);
    if (body->children.count == 0)
        body = nullptr;

    let ASTNode* result = ASTNodeCreate(
        caseLabel.kind == SyntaxKind::CaseKeyword
        ? ASTNodeKind::CaseStatement 
        : ASTNodeKind::DefaultStatement, 
        binder->symbolTable, caseLabel);
    result->left = body;
    result->right = caseExpression;
    return result;
}

fun ASTNode* BindSwitchStatement(Binder* binder) {
    let SyntaxToken switchKeyword = MatchAndAdvanceToken(binder, SyntaxKind::SwitchKeyword);

    let SyntaxToken leftParen = MatchAndAdvanceToken(binder, SyntaxKind::LeftParenToken);
    let ASTNode* switchExpression = BindExpression(binder);
    let SyntaxToken rightParen = MatchAndAdvanceToken(binder, SyntaxKind::RightParenToken);

    binder->switchCaseLevel += 1;
    let SyntaxToken leftBrace = MatchAndAdvanceToken(binder, SyntaxKind::LeftBraceToken);

    let int defaultTokenEncountered = false;
    let ASTNodeArray caseStatements = ASTNodeArrayCreate();
    while (binder->tokenCur.kind != SyntaxKind::EndOfFileToken) {
        if (binder->tokenCur.kind == SyntaxKind::RightBraceToken)
            break;
        let ASTNode* caseStatement = BindCaseStatement(binder, switchExpression);
        if (defaultTokenEncountered) {
            ReportError(
                TokenGetLocation(caseStatement->token),
                "Unexpected case statement after default statement was already defined"
            );
        }
        if (caseStatement->kind == ASTNodeKind::DefaultStatement)
            defaultTokenEncountered = true;
        ASTNodeArrayPush(&caseStatements, caseStatement);
    }

    let SyntaxToken rightBrace = MatchAndAdvanceToken(binder, SyntaxKind::RightBraceToken);
    binder->switchCaseLevel -= 1;

    if (caseStatements.count == 0) {
        ReportError(
            TokenGetLocation(switchKeyword),
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
                    TokenGetLocation(b->token),
                    "Duplicate switch case literal '%s'",
                    TokenGetText(b->right->token).cstr
                );
            }
        }
    }

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::SwitchStatement, binder->symbolTable, switchKeyword);
    result->left = switchExpression;
    result->children = caseStatements;
    return result;
}


fun ASTNode* BindStatement(Binder* binder) {
    switch (binder->tokenCur.kind) {
        case SyntaxKind::LeftBraceToken:
            return BindCompoundStatement(binder, false);
        case SyntaxKind::IfKeyword:
            return BindIfStatement(binder);
        case SyntaxKind::DoKeyword:
            return BindDoWhileStatement(binder);
        case SyntaxKind::WhileKeyword:
            return BindWhileStatement(binder);
        case SyntaxKind::ForKeyword:
            return BindForStatement(binder);
        case SyntaxKind::ReturnKeyword:
            return BindReturnStatement(binder);
        case SyntaxKind::BreakKeyword:
            return BindBreakStatement(binder);
        case SyntaxKind::ContinueKeyword:
            return BindContinueStatement(binder);
        case SyntaxKind::SwitchKeyword:
            return BindSwitchStatement(binder);
        case SyntaxKind::ExternKeyword:
        case SyntaxKind::StructKeyword:
        case SyntaxKind::UnionKeyword:
        case SyntaxKind::EnumKeyword:
        case SyntaxKind::FunKeyword:
        case SyntaxKind::LetKeyword:
            return BindDefinitionStatement(binder);
        default:
            return BindExpressionStatement(binder);
    }
}

fun ASTNode* BindModuleStatement(Binder* binder, SyntaxNode* syntax) {
    // TODO
    return nullptr;
}

fun ASTNode* BindModule(Binder* binder, ModuleStatementSyntax* syntax) {
    assert(syntax->info.kind == SyntaxKind::Module);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::Root, binder->symbolTable, (as SyntaxNode*)syntax);
    for (let int index = 0; index < syntax->globalStatements.count; index += 1) {
        let SyntaxNode* statement = syntax->globalStatements.nodes[index];
        let ASTNode* boundStatement = BindModuleStatement(binder, statement);
        ASTNodeArrayPush(&result->children, boundStatement);
    }
    return result;
}