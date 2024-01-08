#include "scanner.cpp"
#include "definitions.hpp"
// #if 0
// import "definitions.hpp"
// import "scanner.cpp"
// #endif

struct Parser {
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

fun Parser ParserCreate(Source source, SymbolTable* symbolTable) {
    let Parser result;
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

fun SyntaxToken AdvanceToken(Parser* parser) {
    let SyntaxToken result = parser->tokenCur;
    parser->tokenPrev = parser->tokenCur;
    parser->tokenCur = parser->tokenNext;
    parser->tokenNext = parser->tokenNextAfter;
    parser->tokenNextAfter = NextToken(&parser->scanner);
    return result;
}

fun SyntaxToken MatchAndAdvanceToken(Parser* parser, SyntaxKind kind) {
    if (kind == parser->tokenCur.kind) {
        return AdvanceToken(parser);
    } 

    ReportError(
        TokenGetLocation(parser->tokenCur),
        "Expected token '%s' but got token '%s'", 
        TokenKindToString(kind).cstr, TokenKindToString(parser->tokenCur.kind).cstr
    );
    exit(1);
}

fun ASTNode* ParseStatement(Parser* parser);
fun ASTNode* ParseExpression(Parser* parser);
fun ASTNode* ParseVariableDefinitionStatement(Parser* parser, bool isExternal);
fun Type ParseType(Parser* parser);

fun ASTNode* WrapInCompoundStatementIfNecessary(Parser* parser, ASTNode* node) {
    if (node->kind != ASTNodeKind::CompoundStatement) {
        let ASTNode* block = ASTNodeCreate(ASTNodeKind::CompoundStatement, parser->symbolTable, node->token);
        ASTNodeArrayPush(&block->children, node);
        node = block;
    }
    return node;
}

// Turns {{{ a; {b c} d; e; }}} -> { a; {b c} d; e; }
fun ASTNode* FlattenCompoundStatementIfNecessary(Parser* parser, ASTNode* node) {
    if (node->kind == ASTNodeKind::CompoundStatement) {
        if (node->children.count == 1 && node->children.nodes[0]->kind == ASTNodeKind::CompoundStatement) {
            let ASTNode* result = node->children.nodes[0];
            result->symbolTable->parent = node->symbolTable->parent;
            return FlattenCompoundStatementIfNecessary(parser, result);
        }
    }
    return node;
}

fun ASTNode* ParseFunctionCallExpression(Parser* parser, ASTNode* left) {
    let SyntaxToken identifier = left->token;
    let String identifierText = TokenGetText(identifier);
    let Symbol* funcSymbol = GetSymbol(parser->symbolTable, identifierText);
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

    let SyntaxToken leftParen = MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
    let ASTNodeArray argumentList = ASTNodeArrayCreate();
    while (parser->tokenCur.kind != SyntaxKind::RightParenToken) {
        let ASTNode* argument = ParseExpression(parser);
        ASTNodeArrayPush(&argumentList, argument);
        if (parser->tokenCur.kind == SyntaxKind::CommaToken)
            MatchAndAdvanceToken(parser, SyntaxKind::CommaToken);
        else
            break;
    }
    let SyntaxToken rightParen = MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);

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

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::FunccallExpression, parser->symbolTable, identifier);
    result->symbol = funcSymbol;
    result->type = funcSymbol->type;
    result->children = argumentList;
    return result;
}

fun ASTNode* ParseArrayIndexingExpression(Parser* parser, ASTNode* left) {
    let SyntaxToken leftBracket = MatchAndAdvanceToken(parser, SyntaxKind::LeftBracketToken);
    let ASTNode* index = ParseExpression(parser);
    let SyntaxToken rightBracket = MatchAndAdvanceToken(parser, SyntaxKind::RightBracketToken);

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

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::Arrayindexing, parser->symbolTable, left->token);
    result->type = left->symbol->type;
    if (result->type.isArray)
        result->type.isArray = false; // Because we index it
    else
        result->type = GetBaseTypeForPointerType(result->type); // Because we index it
    result->left = left;
    result->right = index;
    return result;
}

fun ASTNode* ParseMemberAccess(Parser* parser, ASTNode* left, bool isArrow) {
    let SyntaxToken accessorToken;
    if (isArrow) 
        accessorToken = MatchAndAdvanceToken(parser, SyntaxKind::ArrowToken);
    else
        accessorToken = MatchAndAdvanceToken(parser, SyntaxKind::DotToken);

    if (left->type.kind != TypeKind::Struct && left->type.kind != TypeKind::Union) {
        ReportError(
            TokenGetLocation(accessorToken),
            "Attempt to access member of non union or struct identifier '%s'", 
            left->symbol->name.cstr
        );
    }

    let Symbol* containerSymbol = GetSymbol(parser->symbolTable, left->type.name);
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
    
    let SyntaxToken memberIdentifier = MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
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

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::Memberaccess, parser->symbolTable, accessorToken);
    result->symbol = memberSymbol;
    result->type = memberSymbol->type;
    result->left = left;
    return result;
}

fun ASTNode* ParseEnumLiteralExpression(Parser* parser) {
    let SyntaxToken enumIdentifier = MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
    let SyntaxToken coloncolon = MatchAndAdvanceToken(parser, SyntaxKind::ColonColonToken);
    let SyntaxToken valueIdentifier = MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);

    let String enumText = TokenGetText(enumIdentifier);
    let Symbol* enumSymbol = GetSymbol(parser->symbolTable, enumText);
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

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::EnumValueLiteral, parser->symbolTable, valueIdentifier);
    result->symbol = valueSymbol;
    result->type = valueSymbol->type;
    return result;
}

fun ASTNode* ParseTypeExpression(Parser* parser) {
    let SyntaxToken startToken = parser->tokenCur;
    let Type type = ParseType(parser);
    let ASTNode* result = ASTNodeCreate(ASTNodeKind::TypeExpression, parser->symbolTable, startToken);
    result->type = type;
    return result;
}

fun ASTNode* ParsePrimaryExpression(Parser* parser) {
    switch (parser->tokenCur.kind) {
        case SyntaxKind::LeftParenToken: {
            let SyntaxToken leftParen = MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
            let ASTNode* inner = ParseExpression(parser);
            let SyntaxToken rightParen = MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);

            let ASTNode* result = ASTNodeCreate(ASTNodeKind::ParenthesizedExpression, parser->symbolTable, leftParen);
            result->type = inner->type;
            result->left = inner;
            return result;
        }
        case SyntaxKind::SizeOfKeyword: {
            let SyntaxToken sizeofKeyword = MatchAndAdvanceToken(parser, SyntaxKind::SizeOfKeyword);
            let SyntaxToken leftParen = MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
            let ASTNode* typeExpr = ParseTypeExpression(parser);
            let SyntaxToken rightParen = MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);

            let ASTNode* result = ASTNodeCreate(ASTNodeKind::SizeOfExpression, parser->symbolTable, sizeofKeyword);
            result->type = TypeCreatePrimitive(TypeKind::PrimitiveInt);
            result->left = typeExpr;
            result->isRValue = true;
            return result;
        }
        case SyntaxKind::IdentifierToken: {
            if (parser->tokenNext.kind == SyntaxKind::ColonColonToken)
                return ParseEnumLiteralExpression(parser);

            let SyntaxToken identifier = MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
            let String identifierText = TokenGetText(identifier);
            let Symbol* symbol = GetSymbol(parser->symbolTable, identifierText);
            if (symbol == nullptr) {
                ReportError(
                    TokenGetLocation(identifier),
                    "Undeclared identifier '%s'", 
                    identifierText.cstr
                );
            }

            let ASTNode* result = ASTNodeCreate(ASTNodeKind::Identifier, parser->symbolTable, identifier);
            result->symbol = symbol;
            result->type = symbol->type;
            return result;
        }
        case SyntaxKind::StringLiteralToken: {
            let SyntaxToken token = MatchAndAdvanceToken(parser, SyntaxKind::StringLiteralToken);

            while (parser->tokenCur.kind == SyntaxKind::StringLiteralToken) {
                let SyntaxToken next = MatchAndAdvanceToken(parser, SyntaxKind::StringLiteralToken);
                token.location.end = next.location.end;
                token.debugString = TokenGetText(token);
                token.stringValueWithoutQuotes = StringAppend(token.stringValueWithoutQuotes, next.stringValueWithoutQuotes);
            }

            let ASTNode* result = ASTNodeCreate(ASTNodeKind::StringLiteral, parser->symbolTable, token);
            result->isRValue = true;
            result->stringvalue = token.stringValueWithoutQuotes;
            result->type = TypeCreate(TypeKind::PrimitiveChar, 1, StringCreateEmpty());
            return result;
        }
        case SyntaxKind::NullKeyword: {
            let SyntaxToken token = MatchAndAdvanceToken(parser, SyntaxKind::NullKeyword);
            let ASTNode* result = ASTNodeCreate(ASTNodeKind::NullLiteral, parser->symbolTable, token);
            result->isRValue = true;
            result->type = TypeCreate(TypeKind::PrimitiveNull, 1, StringCreateEmpty());
            return result;
        }
        case SyntaxKind::CharacterLiteralToken: {
            let SyntaxToken token = MatchAndAdvanceToken(parser, SyntaxKind::CharacterLiteralToken);
            let ASTNode* result = ASTNodeCreate(ASTNodeKind::CharacterLiteral, parser->symbolTable, token);
            result->isRValue = true;
            result->intvalue = token.intvalue;
            result->stringvalue = token.stringValueWithoutQuotes;
            result->type = TypeCreatePrimitive(TypeKind::PrimitiveChar);
            return result;
        }
        case SyntaxKind::TrueKeyword: {
            let SyntaxToken token = MatchAndAdvanceToken(parser, SyntaxKind::TrueKeyword);
            let ASTNode* result = ASTNodeCreate(ASTNodeKind::BoolLiteral, parser->symbolTable, token);
            result->isRValue = true;
            result->intvalue = 1;
            result->type = TypeCreatePrimitive(TypeKind::PrimitiveBool);
            return result;
        }
        case SyntaxKind::FalseKeyword: {
            let SyntaxToken token = MatchAndAdvanceToken(parser, SyntaxKind::FalseKeyword);
            let ASTNode* result = ASTNodeCreate(ASTNodeKind::BoolLiteral, parser->symbolTable, token);
            result->isRValue = true;
            result->intvalue = 0;
            result->type = TypeCreatePrimitive(TypeKind::PrimitiveBool);
            return result;
        }
        case SyntaxKind::IntegerLiteralToken:
        default: {
            let SyntaxToken token = MatchAndAdvanceToken(parser, SyntaxKind::IntegerLiteralToken);
            let ASTNode* result = ASTNodeCreate(ASTNodeKind::IntegerLiteral, parser->symbolTable, token);
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

fun ASTNode* ParseArrayLiteralExpression(Parser* parser, Symbol* arraySymbol) { 
    assert(arraySymbol->type.isArray);

    let SyntaxToken leftBrace = MatchAndAdvanceToken(parser, SyntaxKind::LeftBraceToken);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::ArrayLiteral, parser->symbolTable, leftBrace);

    let int elementCount = 0;
    while (parser->tokenCur.kind != SyntaxKind::RightBraceToken) {
        let ASTNode* expression = ParseExpression(parser);
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
        if (parser->tokenCur.kind == SyntaxKind::RightBraceToken)
            break;
        else
            MatchAndAdvanceToken(parser, SyntaxKind::CommaToken);
    }
    let SyntaxToken rightBrace = MatchAndAdvanceToken(parser, SyntaxKind::RightBraceToken);

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


fun ASTNode* ParsePostFixExpression(Parser* parser) {
    let ASTNode* left = ParsePrimaryExpression(parser);

    let bool foundPostfix = false;
    do {
        foundPostfix = false;
        if (parser->tokenCur.kind == SyntaxKind::LeftParenToken) {
            foundPostfix = true;
            left =  ParseFunctionCallExpression(parser, left);
        }
        if (parser->tokenCur.kind == SyntaxKind::DotToken) {
            foundPostfix = true;
            left = ParseMemberAccess(parser, left, false);
        }
        if (parser->tokenCur.kind == SyntaxKind::ArrowToken) {
            foundPostfix = true;
            left = ParseMemberAccess(parser, left, true);
        }
        if (parser->tokenCur.kind == SyntaxKind::LeftBracketToken) {
            foundPostfix = true;
            left = ParseArrayIndexingExpression(parser, left);
        }
    } while(foundPostfix);

    if (left->type.isArray) {
        left->isRValue = true; // Arrays cannot be assigned to without indexing
    }

    return left;
}

fun ASTNode* ParseBinaryExpression(Parser* parser, int parentPrecedence);
fun ASTNode* ParseUnaryExpression(Parser* parser, int parentPrecedence);

fun ASTNode* ParseCastExpression(Parser* parser) {
    if (parser->tokenCur.kind != SyntaxKind::LeftParenToken || parser->tokenNext.kind != SyntaxKind::AsKeyword)
        return ParsePostFixExpression(parser);
    
    let SyntaxToken leftParen = MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
    let SyntaxToken asKeyword = MatchAndAdvanceToken(parser, SyntaxKind::AsKeyword);
    let Type targetType = ParseType(parser);
    let SyntaxToken rightParen = MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);

    let ASTNode* expression = ParseUnaryExpression(parser, 0);

    let TypeConversionResult conversion = CanConvertTypeFromTo(expression->type, targetType);
    if (conversion == TypeConversionResult::NonConvertible) {
        ReportError(
            TokenGetLocation(asKeyword),
            "Cast from type '%s' to type '%s' is impossible",
            TypeGetText(expression->type).cstr, TypeGetText(targetType).cstr
        );
    }

    let ASTNode* result =  ASTNodeCreate(ASTNodeKind::CastExpression, parser->symbolTable, asKeyword);
    result->type = targetType;
    result->left = expression;
    return result;
}

fun ASTNode* ParseUnaryExpression(Parser* parser, int parentPrecedence) {
    let ASTNode* left = nullptr;
    let int unaryOperatorPrecedence = GetUnaryOperatorPrecedence(parser->tokenCur.kind);
    if ((unaryOperatorPrecedence != 0) && (unaryOperatorPrecedence >= parentPrecedence)) {
        let SyntaxToken operatorToken = AdvanceToken(parser);
        let ASTNode* operand = ParseBinaryExpression(parser, unaryOperatorPrecedence);

        let UnaryOperator op = GetUnaryOperationForToken(operatorToken, operand->type);
        if (op.operandMustBeLValue && operand->isRValue) {
            ReportError(
                TokenGetLocation(operatorToken),
                "Operand of operator '%s' must be an storage location", 
                TokenKindToString(operatorToken.kind).cstr
            );
        }

        let ASTNode* result = ASTNodeCreate(op.operatorKind, parser->symbolTable, operatorToken);
        result->isRValue = op.resultIsRValue;
        result->type = op.resultType;
        result->left = operand;
        left = result;
    } else {
      left = ParseCastExpression(parser);
    }
    return left;
}

fun ASTNode* ParseBinaryExpression(Parser* parser, int parentPrecedence) {
    let ASTNode* left = ParseUnaryExpression(parser, parentPrecedence);

    while (true) {
        let int precedence = GetBinaryOperatorPrecedence(parser->tokenCur.kind);
        if (precedence == 0
         || precedence < parentPrecedence 
         || precedence == parentPrecedence && !IsBinaryOperatorRightAssociative(parser->tokenCur.kind))
            break;

        let SyntaxToken operatorToken = AdvanceToken(parser);
        let ASTNode* right = ParseBinaryExpression(parser, precedence);

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

        let ASTNode* result = ASTNodeCreate(op.operatorKind, parser->symbolTable, operatorToken);
        result->isRValue = op.resultIsRValue;
        result->type = op.resultType;
        result->left = left;
        result->right = right;
        left = result;
    }
    return left;
}

fun ASTNode* ParseTernaryConditionExpression(Parser* parser) {
    let ASTNode* condition = ParseBinaryExpression(parser, 0);

    if (parser->tokenCur.kind == SyntaxKind::QuestionmarkToken) {
        // TODO: check truthyness of condition
        let SyntaxToken questionmark = MatchAndAdvanceToken(parser, SyntaxKind::QuestionmarkToken);
        let ASTNode* thenExpression = ParseTernaryConditionExpression(parser);
        let SyntaxToken colon = MatchAndAdvanceToken(parser, SyntaxKind::ColonToken);
        let ASTNode* elseExpression = ParseTernaryConditionExpression(parser);

        let Type type = GetTypeThatFitsBothTypes(thenExpression->type, elseExpression->type);
        if (IsVoidType(type)) {
            ReportError(
                TokenGetLocation(questionmark),
                "Incompatible expression types in ternary operator - then branch: '%s', else branch: '%s'",
                TypeGetText(thenExpression->type).cstr, TypeGetText(elseExpression->type).cstr
            );
        }

        let ASTNode* ternary = ASTNodeCreate(ASTNodeKind::TernaryConditionalExpression, parser->symbolTable, questionmark);
        ternary->left = condition;
        ternary->right = thenExpression;
        ternary->extra1 = elseExpression;
        ternary->type = type;
        ternary->isRValue = true;
        condition = ternary;
    }
    return condition;
}

fun ASTNode* ParseAssignmentExpression(Parser* parser) {
    let ASTNode* left = ParseTernaryConditionExpression(parser);

    // NOTE: The following is basically a duplicate of what the ParseBinaryExpression does.
    // We need to do this here though to support ternary conditionals more easily.
    // Note that we still keep the GetBinaryOperationForToken(..) call so that we later 
    // can accept custom operator overloadings.
    switch (parser->tokenCur.kind) {
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
            let SyntaxToken assignmentToken = AdvanceToken(parser);
            let ASTNode* right = ParseAssignmentExpression(parser);
            let BinaryOperator op = GetBinaryOperationForToken(assignmentToken, left->type, right->type);
            if (left->isRValue) {
                ReportError(
                    TokenGetLocation(assignmentToken),
                    "Left argument of operator '%s' must be an storage location", 
                    TokenKindToString(assignmentToken.kind).cstr
                );
            }

            let ASTNode* assignment = ASTNodeCreate(op.operatorKind, parser->symbolTable, assignmentToken);
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

fun ASTNode* ParseExpression(Parser* parser) {
    return ParseAssignmentExpression(parser);
}

fun ASTNode* ParseExpressionStatement(Parser* parser) {
    let ASTNode* expression = ParseExpression(parser);
    let SyntaxToken semicolonToken = MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::ExpressionStatement, parser->symbolTable, expression->token);
    result->left = expression;
    return result;
}

fun ASTNode* ParseIfStatement(Parser* parser) {
    let SyntaxToken ifKeyword = MatchAndAdvanceToken(parser, SyntaxKind::IfKeyword);

    let SyntaxToken leftParen = MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
    let ASTNode* condition = ParseExpression(parser);
    // TODO we should check that conditions is truthy
    let SyntaxToken rightParen = MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);

    let ASTNode* thenBranch = ParseStatement(parser);
    thenBranch = WrapInCompoundStatementIfNecessary(parser, thenBranch);

    let ASTNode* elseBranch = nullptr;
    if (parser->tokenCur.kind == SyntaxKind::ElseKeyword) {
        let SyntaxToken elseKeyword = MatchAndAdvanceToken(parser, SyntaxKind::ElseKeyword);
        elseBranch = ParseStatement(parser);
        elseBranch = WrapInCompoundStatementIfNecessary(parser, elseBranch);
    }

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::IfStatement, parser->symbolTable, ifKeyword);
    result->left = condition;
    result->right = thenBranch;
    result->extra1 = elseBranch;
    return result;
}

fun ASTNode* ParseWhileStatement(Parser* parser) {
    let SyntaxToken whileKeyword = MatchAndAdvanceToken(parser, SyntaxKind::WhileKeyword);

    let SyntaxToken leftParen = MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
    let ASTNode* condition = ParseExpression(parser);
    // TODO we should check that conditions is truthy
    let SyntaxToken rightParen = MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);

    parser->loopLevel += 1;
    let ASTNode* body = ParseStatement(parser);
    body = WrapInCompoundStatementIfNecessary(parser, body);
    parser->loopLevel -= 1;

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::WhileStatement, parser->symbolTable, whileKeyword);
    result->left = condition;
    result->right = body;
    return result;
}

fun ASTNode* ParseDoWhileStatement(Parser* parser) {
    let SyntaxToken doKeyword = MatchAndAdvanceToken(parser, SyntaxKind::DoKeyword);
    parser->loopLevel += 1;
    let ASTNode* body = ParseStatement(parser);
    body = WrapInCompoundStatementIfNecessary(parser, body);
    parser->loopLevel -= 1;

    let SyntaxToken whileKeyword = MatchAndAdvanceToken(parser, SyntaxKind::WhileKeyword);

    let SyntaxToken leftParen = MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
    let ASTNode* condition = ParseExpression(parser);
    // TODO we should check that conditions is truthy
    let SyntaxToken rightParen = MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);
    let SyntaxToken semicolon = MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::DoWhileStatement, parser->symbolTable, whileKeyword);
    result->left = condition;
    result->right = body;
    return result;
}

fun ASTNode* ParseForStatement(Parser* parser) {
    let SyntaxToken forKeyword = MatchAndAdvanceToken(parser, SyntaxKind::ForKeyword);

    // Push symboltable scope to make the index local to the for statement
    parser->symbolTable = SymbolTableCreate(parser->symbolTable);
    let ASTNode* result = ASTNodeCreate(ASTNodeKind::ForStatement, parser->symbolTable, forKeyword);

    let SyntaxToken leftParen = MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);

    let ASTNode* initializer = nullptr;
    if (parser->tokenCur.kind == SyntaxKind::LetKeyword)
        initializer = ParseVariableDefinitionStatement(parser, false);
    else 
        initializer = ParseExpressionStatement(parser);
    let ASTNode* condition = ParseExpressionStatement(parser);
    // TODO we should check that conditions is truthy
    let ASTNode* iterator = ParseExpression(parser);

    let SyntaxToken rightParen = MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);

    parser->loopLevel += 1;
    let ASTNode* body = ParseStatement(parser);
    body = WrapInCompoundStatementIfNecessary(parser, body);
    parser->loopLevel -= 1;

    // Pop symboltable
    parser->symbolTable = parser->symbolTable->parent;

    result->left = initializer;
    result->right = condition;
    result->extra1 = iterator;
    result->extra2 = body;
    return result;
}

fun ASTNode* ParseReturnStatement(Parser* parser) {
    let SyntaxToken returnKeyword = MatchAndAdvanceToken(parser, SyntaxKind::ReturnKeyword);
    if (parser->currentFunctionSymbol == nullptr) {
        ReportError(
            TokenGetLocation(returnKeyword),
            "Invalid 'return' statement found outside of function definition"
        );
    }

    let Type functionReturnType = parser->currentFunctionSymbol->type;
    if (IsVoidType(functionReturnType) && parser->tokenCur.kind != SyntaxKind::SemicolonToken) {
        ReportError(
            TokenGetLocation(returnKeyword),
            "Invalid return expression in void function"
        );
    }
    if (!IsVoidType(functionReturnType) && parser->tokenCur.kind == SyntaxKind::SemicolonToken) {
        ReportError(
            TokenGetLocation(returnKeyword),
            "Must return expression in non-void function"
        );
    }

    let ASTNode* expression = nullptr;
    if (parser->tokenCur.kind != SyntaxKind::SemicolonToken) {
        expression = ParseExpression(parser);

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
    let SyntaxToken semicolonToken = MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::ReturnStatement, parser->symbolTable, returnKeyword);
    result->left = expression;
    return result;
}

fun ASTNode* ParseBreakStatement(Parser* parser) {
    let SyntaxToken breakKeyword = MatchAndAdvanceToken(parser, SyntaxKind::BreakKeyword);
    if (parser->loopLevel == 0 && parser->switchCaseLevel == 0) {
        ReportError(
            TokenGetLocation(breakKeyword),
            "Invalid 'break' statement found outside of loop or switch-case definition"
        );
    }
    let SyntaxToken semicolonToken = MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::BreakStatement, parser->symbolTable, breakKeyword);
    return result;
}

fun ASTNode* ParseContinueStatement(Parser* parser) {
    let SyntaxToken continueKeyword = MatchAndAdvanceToken(parser, SyntaxKind::ContinueKeyword);
    if (parser->loopLevel == 0) {
        ReportError(
            TokenGetLocation(continueKeyword),
            "Invalid 'continue' statement found outside of loop definition"
        );
    }
    let SyntaxToken semicolonToken = MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::ContinueStatement, parser->symbolTable, continueKeyword);
    return result;
}


fun Type ParseType(Parser* parser) {
    let Type type = TypeCreateVoid();
    let SyntaxToken startToken = parser->tokenCur;
    switch (parser->tokenCur.kind) {
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
                let String name = TokenGetText(parser->tokenCur);
                let Symbol* symbol = GetSymbol(parser->symbolTable, name);
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
                TokenGetLocation(parser->tokenCur),
                "SyntaxToken '%s' is not a type", 
                TokenGetText(parser->tokenCur).cstr
            );
    }
    AdvanceToken(parser);

    while (parser->tokenCur.kind == SyntaxKind::StarToken) {
        AdvanceToken(parser);
        type = GetPointerTypeForBaseType(type);
    }

    if (type.kind == TypeKind::Struct) {
        let Symbol* symbol = GetSymbol(parser->symbolTable, type.name);
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

fun ASTNode* ParseCompoundStatement(Parser* parser, bool inSwitch) {
    let bool startsWithBrace = false;

    let SyntaxToken leftBrace = parser->tokenCur;
    if (parser->tokenCur.kind == SyntaxKind::LeftBraceToken || !inSwitch) {
        leftBrace = MatchAndAdvanceToken(parser, SyntaxKind::LeftBraceToken);
        startsWithBrace = true;
    }

    // Push symboltable scope
    parser->symbolTable = SymbolTableCreate(parser->symbolTable);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::CompoundStatement, parser->symbolTable, leftBrace);

    while (parser->tokenCur.kind != SyntaxKind::RightBraceToken) {
        if (inSwitch && parser->tokenCur.kind == SyntaxKind::CaseKeyword)
            break;
        if (inSwitch && parser->tokenCur.kind == SyntaxKind::DefaultKeyword)
            break;
        let ASTNode* statement = ParseStatement(parser);
        ASTNodeArrayPush(&result->children, statement);
    }

    // Pop symboltable
    parser->symbolTable = parser->symbolTable->parent;

    if (startsWithBrace) {
        // Then it also must end with a brace
        let SyntaxToken rightBrace = MatchAndAdvanceToken(parser, SyntaxKind::RightBraceToken);
    }

    return FlattenCompoundStatementIfNecessary(parser, result);
}

fun ASTNode* ParseVariableDeclarationWithoutTerminator(Parser* parser, Type type, SyntaxToken identifier, SymbolScopeKind symbolScopeKind) {
    let Symbol* varSymbol = AddSymbol(parser->symbolTable, TokenGetText(identifier), SymbolKind::Variable, symbolScopeKind, type);
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

    if (parser->tokenCur.kind == SyntaxKind::LeftBracketToken) {
        // Array definition
        let longint arrayElementCount = -1;
        let SyntaxToken leftBracket = MatchAndAdvanceToken(parser, SyntaxKind::LeftBracketToken);
        if (parser->tokenCur.kind == SyntaxKind::IntegerLiteralToken) {
            let SyntaxToken intLiteral = MatchAndAdvanceToken(parser, SyntaxKind::IntegerLiteralToken);
            arrayElementCount = intLiteral.intvalue;
        }
        let SyntaxToken rightBracket = MatchAndAdvanceToken(parser, SyntaxKind::RightBracketToken);

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

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::VariableDeclarationStatement, parser->symbolTable, identifier);
    result->symbol = varSymbol;
    return result;
}

fun ASTNode* ParseUnionOrStructDefinitionStatement(Parser* parser, bool isExternal) {
    let SymbolScopeKind symbolScopeKind = isExternal ? SymbolScopeKind::Extern : SymbolScopeKind::Global;

    let SyntaxToken structKeyword;
    if (parser->tokenCur.kind == SyntaxKind::StructKeyword)
        structKeyword = MatchAndAdvanceToken(parser, SyntaxKind::StructKeyword);
    else 
        structKeyword = MatchAndAdvanceToken(parser, SyntaxKind::UnionKeyword);
    let bool isUnion = structKeyword.kind == SyntaxKind::UnionKeyword;

    let SyntaxToken identifier = MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
    let String name = TokenGetText(identifier);

    if (parser->currentFunctionSymbol != nullptr) {
        ReportError(
            TokenGetLocation(identifier),
            "Unexpected struct or union declaration of '%s' while already parsing function", 
            name.cstr
        );
    }

    let Symbol* structSymbol = GetSymbol(parser->symbolTable, name);
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
        structSymbol = AddSymbol(parser->symbolTable, name, isUnion ? SymbolKind::Union : SymbolKind::Struct, symbolScopeKind, type);
    }

    if (parser->tokenCur.kind == SyntaxKind::SemicolonToken) {
        MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
        let ASTNode* result = ASTNodeCreate(
            isUnion ? ASTNodeKind::UnionDeclarationStatement : ASTNodeKind::StructDeclarationStatement, parser->symbolTable, identifier);
        result->symbol = structSymbol;
        return result;
    } else {
        structSymbol->membersSymbolTable = SymbolTableCreate(parser->symbolTable);

        // Push union/struct member symboltable scope to parse variable declarations as members
        parser->symbolTable = structSymbol->membersSymbolTable;

        let SyntaxToken leftBrace = MatchAndAdvanceToken(parser, SyntaxKind::LeftBraceToken);
        while (parser->tokenCur.kind != SyntaxKind::RightBraceToken) {
            let Type memberType = ParseType(parser);
            let SyntaxToken memberIdent = MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
            let ASTNode* memberNode = ParseVariableDeclarationWithoutTerminator(parser, memberType, memberIdent, SymbolScopeKind::Local);
            memberNode->symbol->kind = SymbolKind::Member;
            MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
        }
        let SyntaxToken rightBrace = MatchAndAdvanceToken(parser, SyntaxKind::RightBraceToken);

        // Pop symboltable
        parser->symbolTable = parser->symbolTable->parent;

        if (structSymbol->membersSymbolTable->count == 0){
            ReportError(
                TokenGetLocation(identifier),
                "Struct or union '%s' needs at least one member", 
                name.cstr
            );
        }

        MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);

        if (structSymbol->alreadyDefined) {
            ReportError(
                TokenGetLocation(identifier),
                "Duplicate struct or union definition of '%s'", 
                name.cstr
            );
        }
        structSymbol->alreadyDefined = true;

        let ASTNode* result = ASTNodeCreate(
            isUnion ? ASTNodeKind::UnionDefinitionStatement : ASTNodeKind::StructDefinitionStatement, parser->symbolTable, identifier);
        result->symbol = structSymbol;
        return result;
    }
}

fun ASTNode* ParseEnumDefinitionStatement(Parser* parser, bool isExternal) {
    let SymbolScopeKind symbolScopeKind = isExternal ? SymbolScopeKind::Extern : SymbolScopeKind::Global;

    let SyntaxToken enumKeyword = MatchAndAdvanceToken(parser, SyntaxKind::EnumKeyword);
    let SyntaxToken classKeyword = MatchAndAdvanceToken(parser, SyntaxKind::ClassKeyword);
    let SyntaxToken identifier = MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
    let String name = TokenGetText(identifier);

    if (parser->currentFunctionSymbol != nullptr) {
        ReportError(
            TokenGetLocation(identifier),
            "Unexpected enum declaration of '%s' while already parsing function", 
            name.cstr
        );
    }

    let Symbol* enumSymbol = GetSymbol(parser->symbolTable, name);
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
        enumSymbol = AddSymbol(parser->symbolTable, name, SymbolKind::Enum, symbolScopeKind, type);
    }

    if (parser->tokenCur.kind == SyntaxKind::SemicolonToken) {
        MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
        let ASTNode* result = ASTNodeCreate(ASTNodeKind::EnumDeclarationStatement, parser->symbolTable, identifier);
        result->symbol = enumSymbol;
        return result;
    } else {
        enumSymbol->membersSymbolTable = SymbolTableCreate(parser->symbolTable);

        // Push enum member symboltable scope to parse declarations as members
        parser->symbolTable = enumSymbol->membersSymbolTable;

        let longint valueCounter = 0;
        let SyntaxToken leftBrace = MatchAndAdvanceToken(parser, SyntaxKind::LeftBraceToken);
        while (parser->tokenCur.kind != SyntaxKind::RightBraceToken) {
            let Type memberType = TypeCreate(TypeKind::Enum, 0, enumSymbol->name);
            let SyntaxToken memberIdent = MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
            let ASTNode* memberNode = ParseVariableDeclarationWithoutTerminator(parser, memberType, memberIdent, SymbolScopeKind::Local);
            if (parser->tokenCur.kind == SyntaxKind::EqualsToken) {
                let SyntaxToken equalsToken = MatchAndAdvanceToken(parser, SyntaxKind::EqualsToken);
                let SyntaxToken valueToken = MatchAndAdvanceToken(parser, SyntaxKind::IntegerLiteralToken);
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
            if (parser->tokenCur.kind == SyntaxKind::RightBraceToken)
                break;
            MatchAndAdvanceToken(parser, SyntaxKind::CommaToken);
            valueCounter += 1;
        }
        let SyntaxToken rightBrace = MatchAndAdvanceToken(parser, SyntaxKind::RightBraceToken);

        // Pop symboltable
        parser->symbolTable = parser->symbolTable->parent;

        if (enumSymbol->membersSymbolTable->count == 0){
            ReportError(
                TokenGetLocation(identifier),
                "Enum '%s' needs at least one member", 
                name.cstr
            );
        }

        MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);

        if (enumSymbol->alreadyDefined) {
            ReportError(
                TokenGetLocation(identifier),
                "Duplicate enum definition of '%s'", 
                name.cstr
            );
        }
        enumSymbol->alreadyDefined = true;

        let ASTNode* result = ASTNodeCreate(ASTNodeKind::EnumDefinitionStatement, parser->symbolTable, identifier);
        result->symbol = enumSymbol;
        return result;
    }
}

fun Symbol* ParseFunctionDeclarationStatementWithoutTerminator(Parser* parser, Type returnType, SyntaxToken identifier, bool isExternal) {
    if (parser->currentFunctionSymbol != nullptr) {
        ReportError(
            TokenGetLocation(identifier),
            "Unexpected function declaration of '%s' while already parsing function", 
            TokenGetText(identifier).cstr
        );
    }

    let SymbolScopeKind symbolScopeKind = isExternal ? SymbolScopeKind::Extern : SymbolScopeKind::Global;

    let Symbol* functionSymbol = GetSymbol(parser->symbolTable, TokenGetText(identifier));
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
        functionSymbol = AddSymbol(parser->symbolTable, TokenGetText(identifier), SymbolKind::Function, symbolScopeKind, returnType);

    let SymbolTable* functionParamsSymbolTable = SymbolTableCreate(parser->symbolTable);

    // Push function symboltable scope to parse variables as function parameters
    parser->symbolTable = functionParamsSymbolTable;

    let bool isVariadric = false;
    let SyntaxToken leftParen = MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
    while (parser->tokenCur.kind != SyntaxKind::RightParenToken) {
        if (parser->tokenCur.kind == SyntaxKind::DotDotDotToken) {
            MatchAndAdvanceToken(parser, SyntaxKind::DotDotDotToken);
            isVariadric = true;
            break;
        }
        let Type paramType = ParseType(parser);
        let SyntaxToken paramIdent = MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
        let ASTNode* paramNode = ParseVariableDeclarationWithoutTerminator(parser, paramType, paramIdent, SymbolScopeKind::Local);
        paramNode->symbol->kind = SymbolKind::Parameter;
        if (parser->tokenCur.kind == SyntaxKind::CommaToken)
            MatchAndAdvanceToken(parser, SyntaxKind::CommaToken);
        else
            break;
    }
    let SyntaxToken rightParen = MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);

    // Pop symboltable
    parser->symbolTable = parser->symbolTable->parent;

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

fun ASTNode* ParseFunctionDefinitionStatement(Parser* parser, bool isExternal) {
    let SyntaxToken funKeyword = MatchAndAdvanceToken(parser, SyntaxKind::FunKeyword);
    let Type type = ParseType(parser);
    let SyntaxToken identifier = MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
    let Symbol* functionSymbol = ParseFunctionDeclarationStatementWithoutTerminator(parser, type, identifier, isExternal);

    let ASTNode* body = nullptr;
    if (parser->tokenCur.kind == SyntaxKind::SemicolonToken) {
        // Just a forward declaration
        MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
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
        // Parse function body
        parser->symbolTable = functionSymbol->membersSymbolTable;
        parser->currentFunctionSymbol = functionSymbol;
        body = ParseCompoundStatement(parser, false);
        // TODO: make sure function returns something if its return type is not void
        functionSymbol->alreadyDefined = true;
        parser->currentFunctionSymbol = nullptr;
        parser->symbolTable = parser->symbolTable->parent;
    }

    let ASTNode* result = ASTNodeCreate(
        body == nullptr ? ASTNodeKind::FunctionDeclarationStatement : ASTNodeKind::FunctionDefinitionStatement, 
        parser->symbolTable, identifier);
    result->symbol = functionSymbol;
    result->left = body;
    return result;
}

fun ASTNode* ParseVariableDefinitionStatement(Parser* parser, bool isExternal) {
    let SyntaxToken letKeyword = MatchAndAdvanceToken(parser, SyntaxKind::LetKeyword);

    let bool isLocalPersist = false;
    if (parser->tokenCur.kind == SyntaxKind::LocalPersistKeyword) {
        let SyntaxToken localPersist = MatchAndAdvanceToken(parser, SyntaxKind::LocalPersistKeyword);
        isLocalPersist = true;
    }

    let Type type = ParseType(parser);
    let SyntaxToken identifier = MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);

    let SymbolScopeKind symbolScopeKind = parser->currentFunctionSymbol == nullptr
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

    let ASTNode* result = ParseVariableDeclarationWithoutTerminator(parser, type, identifier, symbolScopeKind);

    if (parser->tokenCur.kind == SyntaxKind::EqualsToken) {
        let SyntaxToken equalsToken = MatchAndAdvanceToken(parser, SyntaxKind::EqualsToken);
        let ASTNode* initializer = nullptr;
        if (result->symbol->type.isArray)
            initializer = ParseArrayLiteralExpression(parser, result->symbol);
        else
            initializer = ParseExpression(parser);
        result->left = initializer;
    } 
    let SyntaxToken semicolonToken = MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
    return result;
}

fun ASTNode* ParseDefinitionStatement(Parser* parser) {
    let bool isExternal = false;
    if (parser->tokenCur.kind == SyntaxKind::ExternKeyword) {
        isExternal = true;
        AdvanceToken(parser);
    }

    if (parser->tokenCur.kind == SyntaxKind::EnumKeyword)
        return ParseEnumDefinitionStatement(parser, isExternal);
    else if (parser->tokenCur.kind == SyntaxKind::StructKeyword || parser->tokenCur.kind == SyntaxKind::UnionKeyword)
        return ParseUnionOrStructDefinitionStatement(parser, isExternal);
    else if (parser->tokenCur.kind == SyntaxKind::FunKeyword)
        return ParseFunctionDefinitionStatement(parser, isExternal);
    else 
        return ParseVariableDefinitionStatement(parser, isExternal);
}

fun ASTNode* ParseCaseStatement(Parser* parser, ASTNode* switchExpression) {
    let SyntaxToken caseLabel = SyntaxTokenCreateEmpty(parser->source);
    let ASTNode* caseExpression = nullptr;

    if (parser->tokenCur.kind == SyntaxKind::CaseKeyword) {
        caseLabel = MatchAndAdvanceToken(parser, SyntaxKind::CaseKeyword);
        if (parser->switchCaseLevel == 0) {
            ReportError(
                TokenGetLocation(caseLabel), 
                "Unexpected case label outside of switch statement"
            );
        }

        caseExpression = ParseExpression(parser);
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
        caseLabel = MatchAndAdvanceToken(parser, SyntaxKind::DefaultKeyword);
        if (parser->switchCaseLevel == 0) {
            ReportError(
                TokenGetLocation(caseLabel), 
                "Unexpected default case label outside of switch statement"
            );
        }
    }
    let SyntaxToken colonToken = MatchAndAdvanceToken(parser, SyntaxKind::ColonToken);

    let ASTNode* body = ParseCompoundStatement(parser, true);
    if (body->children.count == 0)
        body = nullptr;

    let ASTNode* result = ASTNodeCreate(
        caseLabel.kind == SyntaxKind::CaseKeyword
        ? ASTNodeKind::CaseStatement 
        : ASTNodeKind::DefaultStatement, 
        parser->symbolTable, caseLabel);
    result->left = body;
    result->right = caseExpression;
    return result;
}

fun ASTNode* ParseSwitchStatement(Parser* parser) {
    let SyntaxToken switchKeyword = MatchAndAdvanceToken(parser, SyntaxKind::SwitchKeyword);

    let SyntaxToken leftParen = MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
    let ASTNode* switchExpression = ParseExpression(parser);
    let SyntaxToken rightParen = MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);

    parser->switchCaseLevel += 1;
    let SyntaxToken leftBrace = MatchAndAdvanceToken(parser, SyntaxKind::LeftBraceToken);

    let int defaultTokenEncountered = false;
    let ASTNodeArray caseStatements = ASTNodeArrayCreate();
    while (parser->tokenCur.kind != SyntaxKind::EndOfFileToken) {
        if (parser->tokenCur.kind == SyntaxKind::RightBraceToken)
            break;
        let ASTNode* caseStatement = ParseCaseStatement(parser, switchExpression);
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

    let SyntaxToken rightBrace = MatchAndAdvanceToken(parser, SyntaxKind::RightBraceToken);
    parser->switchCaseLevel -= 1;

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

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::SwitchStatement, parser->symbolTable, switchKeyword);
    result->left = switchExpression;
    result->children = caseStatements;
    return result;
}


fun ASTNode* ParseStatement(Parser* parser) {
    switch (parser->tokenCur.kind) {
        case SyntaxKind::LeftBraceToken:
            return ParseCompoundStatement(parser, false);
        case SyntaxKind::IfKeyword:
            return ParseIfStatement(parser);
        case SyntaxKind::DoKeyword:
            return ParseDoWhileStatement(parser);
        case SyntaxKind::WhileKeyword:
            return ParseWhileStatement(parser);
        case SyntaxKind::ForKeyword:
            return ParseForStatement(parser);
        case SyntaxKind::ReturnKeyword:
            return ParseReturnStatement(parser);
        case SyntaxKind::BreakKeyword:
            return ParseBreakStatement(parser);
        case SyntaxKind::ContinueKeyword:
            return ParseContinueStatement(parser);
        case SyntaxKind::SwitchKeyword:
            return ParseSwitchStatement(parser);
        case SyntaxKind::ExternKeyword:
        case SyntaxKind::StructKeyword:
        case SyntaxKind::UnionKeyword:
        case SyntaxKind::EnumKeyword:
        case SyntaxKind::FunKeyword:
        case SyntaxKind::LetKeyword:
            return ParseDefinitionStatement(parser);
        default:
            return ParseExpressionStatement(parser);
    }
}

fun ASTNode* ParseGlobalStatements(Parser* parser) {
    let SyntaxToken root = SyntaxTokenCreateEmpty(parser->source);
    let ASTNode* result = ASTNodeCreate(ASTNodeKind::Root, parser->symbolTable, root);

    while (parser->tokenCur.kind != SyntaxKind::EndOfFileToken) {
        // TODO get rid of these hacks when we do our own language
        // Skip pragmas, include directives, define directives, typedefs
        let bool foundDirectives = true;
        while (foundDirectives) { 
            foundDirectives = false;
            while (parser->tokenCur.kind == SyntaxKind::TypedefKeyword) {
                MatchAndAdvanceToken(parser, SyntaxKind::TypedefKeyword);
                while (parser->tokenCur.kind != SyntaxKind::SemicolonToken) {
                    AdvanceToken(parser); // Ignore everything in the typedef
                }
                MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
                foundDirectives = true;
            }
            while (parser->tokenCur.kind == SyntaxKind::IncludeDirectiveKeyword) {
                MatchAndAdvanceToken(parser, SyntaxKind::IncludeDirectiveKeyword);
                if (parser->tokenCur.kind == SyntaxKind::LessToken) {
                    MatchAndAdvanceToken(parser, SyntaxKind::LessToken);
                    AdvanceToken(parser); // ex. stdio
                    MatchAndAdvanceToken(parser, SyntaxKind::DotToken);
                    AdvanceToken(parser); // ex. hpp
                    MatchAndAdvanceToken(parser, SyntaxKind::GreaterToken);
                } else {
                    MatchAndAdvanceToken(parser, SyntaxKind::StringLiteralToken);
                }
                foundDirectives = true;
            }
            while (parser->tokenCur.kind == SyntaxKind::PragmaDirectiveKeyword) {
                MatchAndAdvanceToken(parser, SyntaxKind::PragmaDirectiveKeyword);
                MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
                foundDirectives = true;
            }
            while (parser->tokenCur.kind == SyntaxKind::IfDirectiveKeyword) {
                MatchAndAdvanceToken(parser, SyntaxKind::IfDirectiveKeyword);
                AdvanceToken(parser); // Ignore 0
                foundDirectives = true;
            }
            while (parser->tokenCur.kind == SyntaxKind::EndIfDefinedDirectiveKeyword) {
                MatchAndAdvanceToken(parser, SyntaxKind::EndIfDefinedDirectiveKeyword);
                foundDirectives = true;
            }
            while (parser->tokenCur.kind == SyntaxKind::DefineDirectiveKeyword) {
                MatchAndAdvanceToken(parser, SyntaxKind::DefineDirectiveKeyword);
                if (parser->tokenCur.kind == SyntaxKind::LetKeyword) {
                    MatchAndAdvanceToken(parser, SyntaxKind::LetKeyword);
                } else if (parser->tokenCur.kind == SyntaxKind::AsKeyword) {
                    MatchAndAdvanceToken(parser, SyntaxKind::AsKeyword);
                } else if (parser->tokenCur.kind == SyntaxKind::ByteKeyword) {
                    MatchAndAdvanceToken(parser, SyntaxKind::ByteKeyword);
                    MatchAndAdvanceToken(parser, SyntaxKind::CharKeyword);
                } else if (parser->tokenCur.kind == SyntaxKind::LocalPersistKeyword) {
                    MatchAndAdvanceToken(parser, SyntaxKind::LocalPersistKeyword);
                    MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
                } else {
                    MatchAndAdvanceToken(parser, SyntaxKind::FunKeyword);
                }
                foundDirectives = true;
            }
        }
        let ASTNode* statement = ParseDefinitionStatement(parser);
        ASTNodeArrayPush(&result->children, statement);
    }

    return result;
}