#include "definitions.hpp"
#include "scanner.cpp"

////////////////////////////////////////////////////////////////////////////////////////////////////
// Helpers

fun int GetUnaryOperatorPrecedence(TokenKind kind)
{
    switch (kind) {
        case TokenKind::Plus:      // Identity
        case TokenKind::Minus:     // Negation
        case TokenKind::Bang:      // Logical negation
        case TokenKind::Tilde:     // Bitwise negation

        case TokenKind::Star:      // Dereference
        case TokenKind::Ampersand: // Addressof
            return 13;
        default:
            return 0;
    }
}

fun bool IsBinaryOperatorRightAssociative(TokenKind kind) {
    switch (kind) {
        case TokenKind::Equals:
        case TokenKind::PlusEquals: 
        case TokenKind::MinusEquals: 
        case TokenKind::StarEquals: 
        case TokenKind::SlashEquals:
        case TokenKind::PercentEquals:
        case TokenKind::HatEquals:
        case TokenKind::AmpersandEquals:
        case TokenKind::PipeEquals:
        case TokenKind::LessLessEquals:
        case TokenKind::GreaterGreaterEquals:
        return true;

        default:
        return false;
    }
}

fun int GetBinaryOperatorPrecedence(TokenKind kind)
{
    switch (kind) {
        case TokenKind::Star:
        case TokenKind::Slash:
        case TokenKind::Percent:
            return 12;

        case TokenKind::Plus:
        case TokenKind::Minus:
            return 11;

        // BitshiftIng
        case TokenKind::LessLess:
        case TokenKind::GreaterGreater:
            return 10;

        case TokenKind::Less:
        case TokenKind::LessEquals:
        case TokenKind::Greater:
        case TokenKind::GreaterEquals:
            return 9;

        case TokenKind::EqualsEquals:
        case TokenKind::BangEquals:
            return 8;

        case TokenKind::Ampersand: // Bitwise AND
            return 7;

        case TokenKind::Hat:       // Bitwise XOR
            return 6;

        case TokenKind::Pipe:      // Bitwise OR
            return 5;

        case TokenKind::AmpersandAmpersand: // Logical AND
            return 4;

        case TokenKind::PipePipe: // Logical OR
            return 3;
        
        // Ternary conditional
        // case TokenKind::QuestionMark:
        //  return 2

        // Assignment
        // case TokenKind::Equals: 
        // case TokenKind::PlusEquals: 
        // case TokenKind::MinusEquals: 
        // case TokenKind::StarEquals: 
        // case TokenKind::SlashEquals:
        // case TokenKind::PercentEquals:
        // case TokenKind::HatEquals:
        // case TokenKind::AmpersandEquals:
        // case TokenKind::PipeEquals:
        // case TokenKind::LessLessEquals:
        // case TokenKind::GreaterGreaterEquals:
        //      return 1;

        default:
            return 0;
    }
}

struct UnaryOperator {
    TokenKind tokenKind;
    ASTNodeKind operatorKind;
    Type operandType;
    Type resultType;
    bool resultIsRValue;
    bool operandMustBeLValue;
};

fun UnaryOperator GetUnaryOperationForToken(Token token, Type operandType) {
    let UnaryOperator result;
    result.tokenKind = token.kind;
    result.operandType = operandType;
    result.resultType = operandType;
    result.resultIsRValue = true;
    result.operandMustBeLValue = false;

    if (IsPointerType(operandType) || operandType.kind == TypeKind::PrimitiveCString) {
        if (token.kind == TokenKind::Star) {
            result.operatorKind = ASTNodeKind::Dereference;
            result.resultType = GetBaseTypeForPointerType(operandType);
            result.resultIsRValue = false;
            result.operandMustBeLValue = true;
            return result;
        }
    } else if (IsNumberType(operandType)) {
        if (token.kind == TokenKind::Plus) {
            result.operatorKind = ASTNodeKind::Identity;
            return result;
        }
        if (token.kind == TokenKind::Minus)
        {
            result.operatorKind = ASTNodeKind::Negation;
            return result;
        }
        if (token.kind == TokenKind::Bang) 
        {
            result.operatorKind = ASTNodeKind::LogicalNegation;
            return result;
        }
        if (token.kind == TokenKind::Tilde) {
            result.operatorKind = ASTNodeKind::BitwiseNegation;
            return result;
        }
    }
    if (token.kind == TokenKind::Ampersand) {
        result.operatorKind = ASTNodeKind::Address;
        result.resultType = GetPointerTypeForBaseType(operandType);
        result.resultIsRValue = false;
        result.operandMustBeLValue = true;
        return result;
    }

    ReportError(
        TokenGetLocation(token), 
        "No applicable unary operation for combination token '%s', type '%s'", 
        TokenKindToString(token.kind).cstr, TypeGetText(operandType).cstr
    );
    exit(1);
}

struct BinaryOperator {
    TokenKind tokenKind;
    ASTNodeKind operatorKind;
    Type leftType;
    Type rightType;
    Type resultType;
    bool resultIsRValue;
    bool leftMustBeLValue;
    bool rightMustBeLValue;
};

fun BinaryOperator GetBinaryOperationForToken(Token token, Type leftType, Type rightType) {
    let BinaryOperator result;
    result.tokenKind = token.kind;
    result.leftType = leftType;
    result.rightType = rightType;
    result.resultType = leftType;
    result.resultIsRValue = true;
    result.leftMustBeLValue = false;
    result.rightMustBeLValue = false;

    if (token.kind == TokenKind::Equals) { 
        let TypeConversionResult conversion = CanConvertTypeFromTo(rightType, leftType);
        if (conversion == TypeConversionResult::NonConvertible) {
            ReportError(
                TokenGetLocation(token),
                "Incompatible types for assignment '%s' = '%s'", 
                TypeGetText(leftType).cstr, TypeGetText(rightType).cstr
            );
        }
        if (conversion == TypeConversionResult::ExplicitlyConvertible) {
            ReportError(
                TokenGetLocation(token),
                "Cannot implicitly convert types for assignment '%s' = '%s'", 
                TypeGetText(leftType).cstr, TypeGetText(rightType).cstr
            );
        }
        result.operatorKind = ASTNodeKind::Assignment;
        result.leftMustBeLValue = true;
        return result;
    }

    if ((token.kind == TokenKind::Plus) && IsPointerType(leftType) && IsNumberType(rightType)) { 
        result.operatorKind = ASTNodeKind::AddToPointer;
        result.resultIsRValue = false;
        result.leftMustBeLValue = true;
        return result;
    }
    if ((token.kind == TokenKind::PlusEquals) && IsPointerType(leftType) && IsNumberType(rightType)) { 
        result.operatorKind = ASTNodeKind::AddToPointerAssignment;
        result.resultIsRValue = false;
        result.leftMustBeLValue = true;
        return result;
    }
    if ((token.kind == TokenKind::Minus) && IsPointerType(leftType) && IsNumberType(rightType)) {
        result.operatorKind = ASTNodeKind::SubtractFromPointer;
        result.resultIsRValue = false;
        result.leftMustBeLValue = true;
        return result;
    }
    if ((token.kind == TokenKind::MinusEquals) && IsPointerType(leftType) && IsNumberType(rightType)) {
        result.operatorKind = ASTNodeKind::SubtractFromPointerAssignment;
        result.resultIsRValue = false;
        result.leftMustBeLValue = true;
        return result;
    }
    if ((token.kind == TokenKind::Minus) 
        && IsPointerType(leftType) && IsPointerType(rightType)
        && leftType.baseIndirectionLevel == rightType.baseIndirectionLevel 
        && (leftType.kind == rightType.kind)) {
        result.operatorKind = ASTNodeKind::DistanceBetweenPointers;
        result.leftMustBeLValue = true;
        result.rightMustBeLValue = true;
        result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveLong);
        return result;
    }

    if (token.kind == TokenKind::EqualsEquals) {
        let TypeConversionResult conversion = CanConvertTypeFromTo(leftType, rightType);
        if (conversion == TypeConversionResult::ImplictlyConvertible 
        || conversion == TypeConversionResult::Identical) {
            result.operatorKind = ASTNodeKind::Equals;
            result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
            return result;
        }
    }
    if (token.kind == TokenKind::BangEquals) {
        let TypeConversionResult conversion = CanConvertTypeFromTo(leftType, rightType);
        if (conversion == TypeConversionResult::ImplictlyConvertible 
        || conversion == TypeConversionResult::Identical) {
            result.operatorKind = ASTNodeKind::NotEquals;
            result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
            return result;
        }
    }

    if (leftType.kind == TypeKind::Enum && rightType.kind == TypeKind::Enum) {
        switch (token.kind) {
            case TokenKind::EqualsEquals: {
                result.operatorKind = ASTNodeKind::Equals;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
            case TokenKind::BangEquals: {
                result.operatorKind = ASTNodeKind::NotEquals;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
            case TokenKind::Less: {
                result.operatorKind = ASTNodeKind::Less;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
            case TokenKind::LessEquals: {
                result.operatorKind = ASTNodeKind::LessEquals;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
            case TokenKind::Greater: {
                result.operatorKind = ASTNodeKind::Greater;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
            case TokenKind::GreaterEquals: {
                result.operatorKind = ASTNodeKind::GreaterEquals;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
        }
    }

    // TODO: introduce concept of 'truthyness' 
    // We basically allow almost any types for the logical operators
    if (token.kind == TokenKind::AmpersandAmpersand && !IsVoidType(leftType) && !IsVoidType(rightType)) {
        result.operatorKind = ASTNodeKind::LogicalAnd;
        return result;
    }
    if (token.kind == TokenKind::PipePipe && !IsVoidType(leftType) && !IsVoidType(rightType)) {
        result.operatorKind = ASTNodeKind::LogicalOr;
        return result;
    }

    if (IsNumberType(leftType) && IsNumberType(rightType) || IsCharType(leftType) && IsCharType(rightType)) {
        switch (token.kind) {
            case TokenKind::Plus: {
                result.operatorKind = ASTNodeKind::Add;
                return result;
            }
            case TokenKind::PlusEquals: {
                result.operatorKind = ASTNodeKind::AddAssignment;
                result.resultIsRValue = false;
                result.leftMustBeLValue = true;
                return result;
            }
            case TokenKind::Minus: {
                result.operatorKind = ASTNodeKind::Subtract;
                return result;
            }
            case TokenKind::MinusEquals: {
                result.operatorKind = ASTNodeKind::SubtractAssignment;
                result.resultIsRValue = false;
                result.leftMustBeLValue = true;
                return result;
            }
            case TokenKind::Star: {
                result.operatorKind = ASTNodeKind::Multiply;
                return result;
            }
            case TokenKind::StarEquals: {
                result.operatorKind = ASTNodeKind::MultiplyAssignment;
                result.resultIsRValue = false;
                result.leftMustBeLValue = true;
                return result;
            }
            case TokenKind::Slash: {
                result.operatorKind = ASTNodeKind::Divide;
                return result;
            }
            case TokenKind::SlashEquals: {
                result.operatorKind = ASTNodeKind::DivideAssignment;
                result.resultIsRValue = false;
                result.leftMustBeLValue = true;
                return result;
            }
            case TokenKind::Percent: {
                result.operatorKind = ASTNodeKind::Remainder;
                return result;
            }
            case TokenKind::PercentEquals: {
                result.operatorKind = ASTNodeKind::RemainderAssignment;
                result.resultIsRValue = false;
                result.leftMustBeLValue = true;
                return result;
            }

            case TokenKind::LessLess: {
                result.operatorKind = ASTNodeKind::BitshiftLeft;
                return result;
            }
            case TokenKind::LessLessEquals: {
                result.operatorKind = ASTNodeKind::BitshiftLeftAssignment;
                result.resultIsRValue = false;
                result.leftMustBeLValue = true;
                return result;
            }
            case TokenKind::GreaterGreater: {
                result.operatorKind = ASTNodeKind::BitshiftRight;
                return result;
            }
            case TokenKind::GreaterGreaterEquals: {
                result.operatorKind = ASTNodeKind::BitshiftRightAssignment;
                result.resultIsRValue = false;
                result.leftMustBeLValue = true;
                return result;
            }
            case TokenKind::Hat: {
                result.operatorKind = ASTNodeKind::BitwiseXor;
                return result;
            }
            case TokenKind::HatEquals: {
                result.operatorKind = ASTNodeKind::BitwiseXorAssignment;
                result.resultIsRValue = false;
                result.leftMustBeLValue = true;
                return result;
            }
            case TokenKind::Ampersand: {
                result.operatorKind = ASTNodeKind::BitwiseAnd;
                return result;
            }
            case TokenKind::AmpersandEquals: {
                result.operatorKind = ASTNodeKind::BitwiseAndAssignment;
                result.resultIsRValue = false;
                result.leftMustBeLValue = true;
                return result;
            }
            case TokenKind::Pipe: {
                result.operatorKind = ASTNodeKind::BitwiseOr;
                return result;
            }
            case TokenKind::PipeEquals: {
                result.operatorKind = ASTNodeKind::BitwiseOrAssignment;
                result.resultIsRValue = false;
                result.leftMustBeLValue = true;
                return result;
            }

            case TokenKind::EqualsEquals: {
                result.operatorKind = ASTNodeKind::Equals;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
            case TokenKind::BangEquals: {
                result.operatorKind = ASTNodeKind::NotEquals;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
            case TokenKind::Less: {
                result.operatorKind = ASTNodeKind::Less;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
            case TokenKind::LessEquals: {
                result.operatorKind = ASTNodeKind::LessEquals;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
            case TokenKind::Greater: {
                result.operatorKind = ASTNodeKind::Greater;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
            case TokenKind::GreaterEquals: {
                result.operatorKind = ASTNodeKind::GreaterEquals;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
        }
    }

    ReportError(
        TokenGetLocation(token),
        "No applicable binary operation for combination token '%s', left type '%s', right type '%s'", 
        TokenKindToString(token.kind).cstr, TypeGetText(leftType).cstr, TypeGetText(rightType).cstr
    );
    exit(1);
}

fun bool AreLiteralsEqual(ASTNode* a, ASTNode* b) {
    if (a->kind == ASTNodeKind::EnumValueLiteral) {
        assert(b->kind == ASTNodeKind::EnumValueLiteral);
        let Symbol* aSym = a->symbol;
        let Symbol* bSym = b->symbol;
        assert(a != nullptr); 
        assert(b != nullptr);
        return StringEquals(aSym->name, bSym->name);
    } else if (a->kind == ASTNodeKind::StringLiteral) {
        assert(b->kind == ASTNodeKind::StringLiteral);
        return StringEquals(a->stringvalue, b->stringvalue);
    } else if (a->kind == ASTNodeKind::IntegerLiteral) {
        assert(b->kind == ASTNodeKind::IntegerLiteral);
        return a->intvalue == b->intvalue;
    } else if (a->kind == ASTNodeKind::CharacterLiteral) {
        assert(b->kind == ASTNodeKind::CharacterLiteral);
        return a->intvalue == b->intvalue;
    } else {
        assert(false && "Unexpected literal type");
    }
    return false;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Parser

struct Parser {
    SymbolTable* symbolTable;

    Source source;
    Scanner scanner;
    Token tokenPrev;
    Token tokenCur;
    Token tokenNext;
    Token tokenNextAfter;

    int loopLevel;
    int switchCaseLevel;
    Symbol* currentFunctionSymbol;
};

fun Parser ParserCreate(Source source, SymbolTable* symbolTable) {
    let Parser result;
    result.symbolTable = symbolTable;
    result.source = source;
    result.scanner = ScannerCreate(source);
    result.tokenPrev = TokenCreateEmpty(source);
    result.tokenCur = NextToken(&result.scanner);
    result.tokenNext = NextToken(&result.scanner);
    result.tokenNextAfter = NextToken(&result.scanner);
    result.loopLevel = 0;
    result.switchCaseLevel = 0;
    result.currentFunctionSymbol = nullptr;
    return result;
}

fun Token AdvanceToken(Parser* parser) {
    let Token result = parser->tokenCur;
    parser->tokenPrev = parser->tokenCur;
    parser->tokenCur = parser->tokenNext;
    parser->tokenNext = parser->tokenNextAfter;
    parser->tokenNextAfter = NextToken(&parser->scanner);
    return result;
}

fun Token MatchAndAdvanceToken(Parser* parser, TokenKind kind) {
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
    let Token identifier = left->token;
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

    let Token leftParen = MatchAndAdvanceToken(parser, TokenKind::LeftParen);
    let ASTNodeArray argumentList = ASTNodeArrayCreate();
    while (parser->tokenCur.kind != TokenKind::RightParen) {
        let ASTNode* argument = ParseExpression(parser);
        ASTNodeArrayPush(&argumentList, argument);
        if (parser->tokenCur.kind == TokenKind::Comma)
            MatchAndAdvanceToken(parser, TokenKind::Comma);
        else
            break;
    }
    let Token rightParen = MatchAndAdvanceToken(parser, TokenKind::RightParen);

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
    let Token leftBracket = MatchAndAdvanceToken(parser, TokenKind::LeftBracket);
    let ASTNode* index = ParseExpression(parser);
    let Token rightBracket = MatchAndAdvanceToken(parser, TokenKind::RightBracket);

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
    let Token accessorToken;
    if (isArrow) 
        accessorToken = MatchAndAdvanceToken(parser, TokenKind::Arrow);
    else
        accessorToken = MatchAndAdvanceToken(parser, TokenKind::Dot);

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
    
    let Token memberIdentifier = MatchAndAdvanceToken(parser, TokenKind::Identifier);
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
    let Token enumIdentifier = MatchAndAdvanceToken(parser, TokenKind::Identifier);
    let Token coloncolon = MatchAndAdvanceToken(parser, TokenKind::ColonColon);
    let Token valueIdentifier = MatchAndAdvanceToken(parser, TokenKind::Identifier);

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
    let Token startToken = parser->tokenCur;
    let Type type = ParseType(parser);
    let ASTNode* result = ASTNodeCreate(ASTNodeKind::TypeExpression, parser->symbolTable, startToken);
    result->type = type;
    return result;
}

fun ASTNode* ParsePrimaryExpression(Parser* parser) {
    switch (parser->tokenCur.kind) {
        case TokenKind::LeftParen: {
            let Token leftParen = MatchAndAdvanceToken(parser, TokenKind::LeftParen);
            let ASTNode* inner = ParseExpression(parser);
            let Token rightParen = MatchAndAdvanceToken(parser, TokenKind::RightParen);

            let ASTNode* result = ASTNodeCreate(ASTNodeKind::ParenthesizedExpression, parser->symbolTable, leftParen);
            result->type = inner->type;
            result->left = inner;
            return result;
        }
        case TokenKind::SizeOf: {
            let Token sizeofKeyword = MatchAndAdvanceToken(parser, TokenKind::SizeOf);
            let Token leftParen = MatchAndAdvanceToken(parser, TokenKind::LeftParen);
            let ASTNode* typeExpr = ParseTypeExpression(parser);
            let Token rightParen = MatchAndAdvanceToken(parser, TokenKind::RightParen);

            let ASTNode* result = ASTNodeCreate(ASTNodeKind::SizeOfExpression, parser->symbolTable, sizeofKeyword);
            result->type = TypeCreatePrimitive(TypeKind::PrimitiveInt);
            result->left = typeExpr;
            result->isRValue = true;
            return result;
        }
        case TokenKind::Identifier: {
            if (parser->tokenNext.kind == TokenKind::ColonColon)
                return ParseEnumLiteralExpression(parser);

            let Token identifier = MatchAndAdvanceToken(parser, TokenKind::Identifier);
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
        case TokenKind::StringLiteral: {
            let Token token = MatchAndAdvanceToken(parser, TokenKind::StringLiteral);

            while (parser->tokenCur.kind == TokenKind::StringLiteral) {
                let Token next = MatchAndAdvanceToken(parser, TokenKind::StringLiteral);
                token.sourceEnd = next.sourceEnd;
                token.sourceString = TokenGetText(token);
                token.stringValue = StringAppend(token.stringValue, next.stringValue);
            }

            let ASTNode* result = ASTNodeCreate(ASTNodeKind::StringLiteral, parser->symbolTable, token);
            result->isRValue = true;
            result->stringvalue = token.stringValue;
            result->type = TypeCreate(TypeKind::PrimitiveChar, 1, StringCreateEmpty());
            return result;
        }
        case TokenKind::Null: {
            let Token token = MatchAndAdvanceToken(parser, TokenKind::Null);
            let ASTNode* result = ASTNodeCreate(ASTNodeKind::NullLiteral, parser->symbolTable, token);
            result->isRValue = true;
            result->type = TypeCreate(TypeKind::PrimitiveNull, 1, StringCreateEmpty());
            return result;
        }
        case TokenKind::CharacterLiteral: {
            let Token token = MatchAndAdvanceToken(parser, TokenKind::CharacterLiteral);
            let ASTNode* result = ASTNodeCreate(ASTNodeKind::CharacterLiteral, parser->symbolTable, token);
            result->isRValue = true;
            result->intvalue = token.intvalue;
            result->stringvalue = token.stringValue;
            result->type = TypeCreatePrimitive(TypeKind::PrimitiveChar);
            return result;
        }
        case TokenKind::True: {
            let Token token = MatchAndAdvanceToken(parser, TokenKind::True);
            let ASTNode* result = ASTNodeCreate(ASTNodeKind::BoolLiteral, parser->symbolTable, token);
            result->isRValue = true;
            result->intvalue = 1;
            result->type = TypeCreatePrimitive(TypeKind::PrimitiveBool);
            return result;
        }
        case TokenKind::False: {
            let Token token = MatchAndAdvanceToken(parser, TokenKind::False);
            let ASTNode* result = ASTNodeCreate(ASTNodeKind::BoolLiteral, parser->symbolTable, token);
            result->isRValue = true;
            result->intvalue = 0;
            result->type = TypeCreatePrimitive(TypeKind::PrimitiveBool);
            return result;
        }
        case TokenKind::IntegerLiteral:
        default: {
            let Token token = MatchAndAdvanceToken(parser, TokenKind::IntegerLiteral);
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

    let Token leftBrace = MatchAndAdvanceToken(parser, TokenKind::LeftBrace);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::ArrayLiteral, parser->symbolTable, leftBrace);

    let int elementCount = 0;
    while (parser->tokenCur.kind != TokenKind::RightBrace) {
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
        if (parser->tokenCur.kind == TokenKind::RightBrace)
            break;
        else
            MatchAndAdvanceToken(parser, TokenKind::Comma);
    }
    let Token rightBrace = MatchAndAdvanceToken(parser, TokenKind::RightBrace);

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
        if (parser->tokenCur.kind == TokenKind::LeftParen) {
            foundPostfix = true;
            left =  ParseFunctionCallExpression(parser, left);
        }
        if (parser->tokenCur.kind == TokenKind::Dot) {
            foundPostfix = true;
            left = ParseMemberAccess(parser, left, false);
        }
        if (parser->tokenCur.kind == TokenKind::Arrow) {
            foundPostfix = true;
            left = ParseMemberAccess(parser, left, true);
        }
        if (parser->tokenCur.kind == TokenKind::LeftBracket) {
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
    if (parser->tokenCur.kind != TokenKind::LeftParen || parser->tokenNext.kind != TokenKind::As)
        return ParsePostFixExpression(parser);
    
    let Token leftParen = MatchAndAdvanceToken(parser, TokenKind::LeftParen);
    let Token asKeyword = MatchAndAdvanceToken(parser, TokenKind::As);
    let Type targetType = ParseType(parser);
    let Token rightParen = MatchAndAdvanceToken(parser, TokenKind::RightParen);

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
        let Token operatorToken = AdvanceToken(parser);
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

        let Token operatorToken = AdvanceToken(parser);
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

    if (parser->tokenCur.kind == TokenKind::Questionmark) {
        // TODO: check truthyness of condition
        let Token questionmark = MatchAndAdvanceToken(parser, TokenKind::Questionmark);
        let ASTNode* thenExpression = ParseTernaryConditionExpression(parser);
        let Token colon = MatchAndAdvanceToken(parser, TokenKind::Colon);
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
        case TokenKind::Equals: 
        case TokenKind::PlusEquals: 
        case TokenKind::MinusEquals: 
        case TokenKind::StarEquals: 
        case TokenKind::SlashEquals:
        case TokenKind::PercentEquals:
        case TokenKind::HatEquals:
        case TokenKind::AmpersandEquals:
        case TokenKind::PipeEquals:
        case TokenKind::LessLessEquals:
        case TokenKind::GreaterGreaterEquals:
        {
            let Token assignmentToken = AdvanceToken(parser);
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
    let Token semicolonToken = MatchAndAdvanceToken(parser, TokenKind::Semicolon);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::ExpressionStatement, parser->symbolTable, expression->token);
    result->left = expression;
    return result;
}

fun ASTNode* ParseIfStatement(Parser* parser) {
    let Token ifKeyword = MatchAndAdvanceToken(parser, TokenKind::If);

    let Token leftParen = MatchAndAdvanceToken(parser, TokenKind::LeftParen);
    let ASTNode* condition = ParseExpression(parser);
    // TODO we should check that conditions is truthy
    let Token rightParen = MatchAndAdvanceToken(parser, TokenKind::RightParen);

    let ASTNode* thenBranch = ParseStatement(parser);
    thenBranch = WrapInCompoundStatementIfNecessary(parser, thenBranch);

    let ASTNode* elseBranch = nullptr;
    if (parser->tokenCur.kind == TokenKind::Else) {
        let Token elseKeyword = MatchAndAdvanceToken(parser, TokenKind::Else);
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
    let Token whileKeyword = MatchAndAdvanceToken(parser, TokenKind::While);

    let Token leftParen = MatchAndAdvanceToken(parser, TokenKind::LeftParen);
    let ASTNode* condition = ParseExpression(parser);
    // TODO we should check that conditions is truthy
    let Token rightParen = MatchAndAdvanceToken(parser, TokenKind::RightParen);

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
    let Token doKeyword = MatchAndAdvanceToken(parser, TokenKind::Do);
    parser->loopLevel += 1;
    let ASTNode* body = ParseStatement(parser);
    body = WrapInCompoundStatementIfNecessary(parser, body);
    parser->loopLevel -= 1;

    let Token whileKeyword = MatchAndAdvanceToken(parser, TokenKind::While);

    let Token leftParen = MatchAndAdvanceToken(parser, TokenKind::LeftParen);
    let ASTNode* condition = ParseExpression(parser);
    // TODO we should check that conditions is truthy
    let Token rightParen = MatchAndAdvanceToken(parser, TokenKind::RightParen);
    let Token semicolon = MatchAndAdvanceToken(parser, TokenKind::Semicolon);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::DoWhileStatement, parser->symbolTable, whileKeyword);
    result->left = condition;
    result->right = body;
    return result;
}

fun ASTNode* ParseForStatement(Parser* parser) {
    let Token forKeyword = MatchAndAdvanceToken(parser, TokenKind::For);

    // Push symboltable scope to make the index local to the for statement
    parser->symbolTable = SymbolTableCreate(parser->symbolTable);
    let ASTNode* result = ASTNodeCreate(ASTNodeKind::ForStatement, parser->symbolTable, forKeyword);

    let Token leftParen = MatchAndAdvanceToken(parser, TokenKind::LeftParen);

    let ASTNode* initializer = nullptr;
    if (parser->tokenCur.kind == TokenKind::Let)
        initializer = ParseVariableDefinitionStatement(parser, false);
    else 
        initializer = ParseExpressionStatement(parser);
    let ASTNode* condition = ParseExpressionStatement(parser);
    // TODO we should check that conditions is truthy
    let ASTNode* iterator = ParseExpression(parser);

    let Token rightParen = MatchAndAdvanceToken(parser, TokenKind::RightParen);

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
    let Token returnKeyword = MatchAndAdvanceToken(parser, TokenKind::Return);
    if (parser->currentFunctionSymbol == nullptr) {
        ReportError(
            TokenGetLocation(returnKeyword),
            "Invalid 'return' statement found outside of function definition"
        );
    }

    let Type functionReturnType = parser->currentFunctionSymbol->type;
    if (IsVoidType(functionReturnType) && parser->tokenCur.kind != TokenKind::Semicolon) {
        ReportError(
            TokenGetLocation(returnKeyword),
            "Invalid return expression in void function"
        );
    }
    if (!IsVoidType(functionReturnType) && parser->tokenCur.kind == TokenKind::Semicolon) {
        ReportError(
            TokenGetLocation(returnKeyword),
            "Must return expression in non-void function"
        );
    }

    let ASTNode* expression = nullptr;
    if (parser->tokenCur.kind != TokenKind::Semicolon) {
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
    let Token semicolonToken = MatchAndAdvanceToken(parser, TokenKind::Semicolon);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::ReturnStatement, parser->symbolTable, returnKeyword);
    result->left = expression;
    return result;
}

fun ASTNode* ParseBreakStatement(Parser* parser) {
    let Token breakKeyword = MatchAndAdvanceToken(parser, TokenKind::Break);
    if (parser->loopLevel == 0 && parser->switchCaseLevel == 0) {
        ReportError(
            TokenGetLocation(breakKeyword),
            "Invalid 'break' statement found outside of loop or switch-case definition"
        );
    }
    let Token semicolonToken = MatchAndAdvanceToken(parser, TokenKind::Semicolon);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::BreakStatement, parser->symbolTable, breakKeyword);
    return result;
}

fun ASTNode* ParseContinueStatement(Parser* parser) {
    let Token continueKeyword = MatchAndAdvanceToken(parser, TokenKind::Continue);
    if (parser->loopLevel == 0) {
        ReportError(
            TokenGetLocation(continueKeyword),
            "Invalid 'continue' statement found outside of loop definition"
        );
    }
    let Token semicolonToken = MatchAndAdvanceToken(parser, TokenKind::Semicolon);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::ContinueStatement, parser->symbolTable, continueKeyword);
    return result;
}


fun Type ParseType(Parser* parser) {
    let Type type = TypeCreateVoid();
    let Token startToken = parser->tokenCur;
    switch (parser->tokenCur.kind) {
        case TokenKind::Void:
            type.kind = TypeKind::PrimitiveVoid;
            break;
        case TokenKind::Char:
            type.kind = TypeKind::PrimitiveChar;
            break;
        case TokenKind::Bool:
            type.kind = TypeKind::PrimitiveBool;
            break;
        case TokenKind::Byte:
            type.kind = TypeKind::PrimitiveByte;
            break;
        case TokenKind::Short:
            type.kind = TypeKind::PrimitiveShort;
            break;
        case TokenKind::Int:
            type.kind = TypeKind::PrimitiveInt;
            break;
        case TokenKind::Long:
            type.kind = TypeKind::PrimitiveLong;
            break;
        case TokenKind::CString:
            type.kind = TypeKind::PrimitiveCString;
            break;
        case TokenKind::Identifier: {
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
                "Token '%s' is not a type", 
                TokenGetText(parser->tokenCur).cstr
            );
    }
    AdvanceToken(parser);

    while (parser->tokenCur.kind == TokenKind::Star) {
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

    let Token leftBrace = parser->tokenCur;
    if (parser->tokenCur.kind == TokenKind::LeftBrace || !inSwitch) {
        leftBrace = MatchAndAdvanceToken(parser, TokenKind::LeftBrace);
        startsWithBrace = true;
    }

    // Push symboltable scope
    parser->symbolTable = SymbolTableCreate(parser->symbolTable);

    let ASTNode* result = ASTNodeCreate(ASTNodeKind::CompoundStatement, parser->symbolTable, leftBrace);

    while (parser->tokenCur.kind != TokenKind::RightBrace) {
        if (inSwitch && parser->tokenCur.kind == TokenKind::Case)
            break;
        if (inSwitch && parser->tokenCur.kind == TokenKind::Default)
            break;
        let ASTNode* statement = ParseStatement(parser);
        ASTNodeArrayPush(&result->children, statement);
    }

    // Pop symboltable
    parser->symbolTable = parser->symbolTable->parent;

    if (startsWithBrace) {
        // Then it also must end with a brace
        let Token rightBrace = MatchAndAdvanceToken(parser, TokenKind::RightBrace);
    }

    return FlattenCompoundStatementIfNecessary(parser, result);
}

fun ASTNode* ParseVariableDeclarationWithoutTerminator(Parser* parser, Type type, Token identifier, SymbolScopeKind symbolScopeKind) {
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

    if (parser->tokenCur.kind == TokenKind::LeftBracket) {
        // Array definition
        let longint arrayElementCount = -1;
        let Token leftBracket = MatchAndAdvanceToken(parser, TokenKind::LeftBracket);
        if (parser->tokenCur.kind == TokenKind::IntegerLiteral) {
            let Token intLiteral = MatchAndAdvanceToken(parser, TokenKind::IntegerLiteral);
            arrayElementCount = intLiteral.intvalue;
        }
        let Token rightBracket = MatchAndAdvanceToken(parser, TokenKind::RightBracket);

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

    let Token structKeyword;
    if (parser->tokenCur.kind == TokenKind::Struct)
        structKeyword = MatchAndAdvanceToken(parser, TokenKind::Struct);
    else 
        structKeyword = MatchAndAdvanceToken(parser, TokenKind::Union);
    let bool isUnion = structKeyword.kind == TokenKind::Union;

    let Token identifier = MatchAndAdvanceToken(parser, TokenKind::Identifier);
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

    if (parser->tokenCur.kind == TokenKind::Semicolon) {
        MatchAndAdvanceToken(parser, TokenKind::Semicolon);
        let ASTNode* result = ASTNodeCreate(
            isUnion ? ASTNodeKind::UnionDeclarationStatement : ASTNodeKind::StructDeclarationStatement, parser->symbolTable, identifier);
        result->symbol = structSymbol;
        return result;
    } else {
        structSymbol->membersSymbolTable = SymbolTableCreate(parser->symbolTable);

        // Push union/struct member symboltable scope to parse variable declarations as members
        parser->symbolTable = structSymbol->membersSymbolTable;

        let Token leftBrace = MatchAndAdvanceToken(parser, TokenKind::LeftBrace);
        while (parser->tokenCur.kind != TokenKind::RightBrace) {
            let Type memberType = ParseType(parser);
            let Token memberIdent = MatchAndAdvanceToken(parser, TokenKind::Identifier);
            let ASTNode* memberNode = ParseVariableDeclarationWithoutTerminator(parser, memberType, memberIdent, SymbolScopeKind::Local);
            memberNode->symbol->kind = SymbolKind::Member;
            MatchAndAdvanceToken(parser, TokenKind::Semicolon);
        }
        let Token rightBrace = MatchAndAdvanceToken(parser, TokenKind::RightBrace);

        // Pop symboltable
        parser->symbolTable = parser->symbolTable->parent;

        if (structSymbol->membersSymbolTable->count == 0){
            ReportError(
                TokenGetLocation(identifier),
                "Struct or union '%s' needs at least one member", 
                name.cstr
            );
        }

        MatchAndAdvanceToken(parser, TokenKind::Semicolon);

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

    let Token enumKeyword = MatchAndAdvanceToken(parser, TokenKind::Enum);
    let Token classKeyword = MatchAndAdvanceToken(parser, TokenKind::Class);
    let Token identifier = MatchAndAdvanceToken(parser, TokenKind::Identifier);
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

    if (parser->tokenCur.kind == TokenKind::Semicolon) {
        MatchAndAdvanceToken(parser, TokenKind::Semicolon);
        let ASTNode* result = ASTNodeCreate(ASTNodeKind::EnumDeclarationStatement, parser->symbolTable, identifier);
        result->symbol = enumSymbol;
        return result;
    } else {
        enumSymbol->membersSymbolTable = SymbolTableCreate(parser->symbolTable);

        // Push enum member symboltable scope to parse declarations as members
        parser->symbolTable = enumSymbol->membersSymbolTable;

        let longint valueCounter = 0;
        let Token leftBrace = MatchAndAdvanceToken(parser, TokenKind::LeftBrace);
        while (parser->tokenCur.kind != TokenKind::RightBrace) {
            let Type memberType = TypeCreate(TypeKind::Enum, 0, enumSymbol->name);
            let Token memberIdent = MatchAndAdvanceToken(parser, TokenKind::Identifier);
            let ASTNode* memberNode = ParseVariableDeclarationWithoutTerminator(parser, memberType, memberIdent, SymbolScopeKind::Local);
            if (parser->tokenCur.kind == TokenKind::Equals) {
                let Token equalsToken = MatchAndAdvanceToken(parser, TokenKind::Equals);
                let Token valueToken = MatchAndAdvanceToken(parser, TokenKind::IntegerLiteral);
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
            if (parser->tokenCur.kind == TokenKind::RightBrace)
                break;
            MatchAndAdvanceToken(parser, TokenKind::Comma);
            valueCounter += 1;
        }
        let Token rightBrace = MatchAndAdvanceToken(parser, TokenKind::RightBrace);

        // Pop symboltable
        parser->symbolTable = parser->symbolTable->parent;

        if (enumSymbol->membersSymbolTable->count == 0){
            ReportError(
                TokenGetLocation(identifier),
                "Enum '%s' needs at least one member", 
                name.cstr
            );
        }

        MatchAndAdvanceToken(parser, TokenKind::Semicolon);

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

fun Symbol* ParseFunctionDeclarationStatementWithoutTerminator(Parser* parser, Type returnType, Token identifier, bool isExternal) {
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
    let Token leftParen = MatchAndAdvanceToken(parser, TokenKind::LeftParen);
    while (parser->tokenCur.kind != TokenKind::RightParen) {
        if (parser->tokenCur.kind == TokenKind::DotDotDot) {
            MatchAndAdvanceToken(parser, TokenKind::DotDotDot);
            isVariadric = true;
            break;
        }
        let Type paramType = ParseType(parser);
        let Token paramIdent = MatchAndAdvanceToken(parser, TokenKind::Identifier);
        let ASTNode* paramNode = ParseVariableDeclarationWithoutTerminator(parser, paramType, paramIdent, SymbolScopeKind::Local);
        paramNode->symbol->kind = SymbolKind::Parameter;
        if (parser->tokenCur.kind == TokenKind::Comma)
            MatchAndAdvanceToken(parser, TokenKind::Comma);
        else
            break;
    }
    let Token rightParen = MatchAndAdvanceToken(parser, TokenKind::RightParen);

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
    let Token funKeyword = MatchAndAdvanceToken(parser, TokenKind::Fun);
    let Type type = ParseType(parser);
    let Token identifier = MatchAndAdvanceToken(parser, TokenKind::Identifier);
    let Symbol* functionSymbol = ParseFunctionDeclarationStatementWithoutTerminator(parser, type, identifier, isExternal);

    let ASTNode* body = nullptr;
    if (parser->tokenCur.kind == TokenKind::Semicolon) {
        // Just a forward declaration
        MatchAndAdvanceToken(parser, TokenKind::Semicolon);
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
    let Token letKeyword = MatchAndAdvanceToken(parser, TokenKind::Let);

    let bool isLocalPersist = false;
    if (parser->tokenCur.kind == TokenKind::LocalPersist) {
        let Token localPersist = MatchAndAdvanceToken(parser, TokenKind::LocalPersist);
        isLocalPersist = true;
    }

    let Type type = ParseType(parser);
    let Token identifier = MatchAndAdvanceToken(parser, TokenKind::Identifier);

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

    if (parser->tokenCur.kind == TokenKind::Equals) {
        let Token equalsToken = MatchAndAdvanceToken(parser, TokenKind::Equals);
        let ASTNode* initializer = nullptr;
        if (result->symbol->type.isArray)
            initializer = ParseArrayLiteralExpression(parser, result->symbol);
        else
            initializer = ParseExpression(parser);
        result->left = initializer;
    } 
    let Token semicolonToken = MatchAndAdvanceToken(parser, TokenKind::Semicolon);
    return result;
}

fun ASTNode* ParseDefinitionStatement(Parser* parser) {
    let bool isExternal = false;
    if (parser->tokenCur.kind == TokenKind::Extern) {
        isExternal = true;
        AdvanceToken(parser);
    }

    if (parser->tokenCur.kind == TokenKind::Enum)
        return ParseEnumDefinitionStatement(parser, isExternal);
    else if (parser->tokenCur.kind == TokenKind::Struct || parser->tokenCur.kind == TokenKind::Union)
        return ParseUnionOrStructDefinitionStatement(parser, isExternal);
    else if (parser->tokenCur.kind == TokenKind::Fun)
        return ParseFunctionDefinitionStatement(parser, isExternal);
    else 
        return ParseVariableDefinitionStatement(parser, isExternal);
}

fun ASTNode* ParseCaseStatement(Parser* parser, ASTNode* switchExpression) {
    let Token caseLabel = TokenCreateEmpty(parser->source);
    let ASTNode* caseExpression = nullptr;

    if (parser->tokenCur.kind == TokenKind::Case) {
        caseLabel = MatchAndAdvanceToken(parser, TokenKind::Case);
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
        caseLabel = MatchAndAdvanceToken(parser, TokenKind::Default);
        if (parser->switchCaseLevel == 0) {
            ReportError(
                TokenGetLocation(caseLabel), 
                "Unexpected default case label outside of switch statement"
            );
        }
    }
    let Token colonToken = MatchAndAdvanceToken(parser, TokenKind::Colon);

    let ASTNode* body = ParseCompoundStatement(parser, true);
    if (body->children.count == 0)
        body = nullptr;

    let ASTNode* result = ASTNodeCreate(
        caseLabel.kind == TokenKind::Case 
        ? ASTNodeKind::CaseStatement 
        : ASTNodeKind::DefaultStatement, 
        parser->symbolTable, caseLabel);
    result->left = body;
    result->right = caseExpression;
    return result;
}

fun ASTNode* ParseSwitchStatement(Parser* parser) {
    let Token switchKeyword = MatchAndAdvanceToken(parser, TokenKind::Switch);

    let Token leftParen = MatchAndAdvanceToken(parser, TokenKind::LeftParen);
    let ASTNode* switchExpression = ParseExpression(parser);
    let Token rightParen = MatchAndAdvanceToken(parser, TokenKind::RightParen);

    parser->switchCaseLevel += 1;
    let Token leftBrace = MatchAndAdvanceToken(parser, TokenKind::LeftBrace);

    let int defaultTokenEncountered = false;
    let ASTNodeArray caseStatements = ASTNodeArrayCreate();
    while (parser->tokenCur.kind != TokenKind::EndOfFile) {
        if (parser->tokenCur.kind == TokenKind::RightBrace)
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

    let Token rightBrace = MatchAndAdvanceToken(parser, TokenKind::RightBrace);
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
        case TokenKind::LeftBrace:
            return ParseCompoundStatement(parser, false);
        case TokenKind::If:
            return ParseIfStatement(parser);
        case TokenKind::Do:
            return ParseDoWhileStatement(parser);
        case TokenKind::While:
            return ParseWhileStatement(parser);
        case TokenKind::For:
            return ParseForStatement(parser);
        case TokenKind::Return:
            return ParseReturnStatement(parser);
        case TokenKind::Break:
            return ParseBreakStatement(parser);
        case TokenKind::Continue:
            return ParseContinueStatement(parser);
        case TokenKind::Switch:
            return ParseSwitchStatement(parser);
        case TokenKind::Extern:
        case TokenKind::Struct:
        case TokenKind::Union:
        case TokenKind::Enum:
        case TokenKind::Fun:
        case TokenKind::Let:
            return ParseDefinitionStatement(parser);
        default:
            return ParseExpressionStatement(parser);
    }
}

fun ASTNode* ParseGlobalStatements(Parser* parser) {
    let Token root = TokenCreateEmpty(parser->source);
    let ASTNode* result = ASTNodeCreate(ASTNodeKind::Root, parser->symbolTable, root);

    while (parser->tokenCur.kind != TokenKind::EndOfFile) {
        // TODO get rid of these hacks when we do our own language
        // Skip pragmas, include directives, define directives, typedefs
        let bool foundDirectives = true;
        while (foundDirectives) { 
            foundDirectives = false;
            while (parser->tokenCur.kind == TokenKind::Typedef) {
                MatchAndAdvanceToken(parser, TokenKind::Typedef);
                while (parser->tokenCur.kind != TokenKind::Semicolon) {
                    AdvanceToken(parser); // Ignore everything in the typedef
                }
                MatchAndAdvanceToken(parser, TokenKind::Semicolon);
                foundDirectives = true;
            }
            while (parser->tokenCur.kind == TokenKind::IncludeDirective) {
                MatchAndAdvanceToken(parser, TokenKind::IncludeDirective);
                if (parser->tokenCur.kind == TokenKind::Less) {
                    MatchAndAdvanceToken(parser, TokenKind::Less);
                    AdvanceToken(parser); // ex. stdio
                    MatchAndAdvanceToken(parser, TokenKind::Dot);
                    AdvanceToken(parser); // ex. hpp
                    MatchAndAdvanceToken(parser, TokenKind::Greater);
                } else {
                    MatchAndAdvanceToken(parser, TokenKind::StringLiteral);
                }
                foundDirectives = true;
            }
            while (parser->tokenCur.kind == TokenKind::PragmaDirective) {
                MatchAndAdvanceToken(parser, TokenKind::PragmaDirective);
                MatchAndAdvanceToken(parser, TokenKind::Identifier);
                foundDirectives = true;
            }
            while (parser->tokenCur.kind == TokenKind::IfDirective) {
                MatchAndAdvanceToken(parser, TokenKind::IfDirective);
                AdvanceToken(parser); // Ignore 0
                foundDirectives = true;
            }
            while (parser->tokenCur.kind == TokenKind::EndIfDefinedDirective) {
                MatchAndAdvanceToken(parser, TokenKind::EndIfDefinedDirective);
                foundDirectives = true;
            }
            while (parser->tokenCur.kind == TokenKind::DefineDirective) {
                MatchAndAdvanceToken(parser, TokenKind::DefineDirective);
                if (parser->tokenCur.kind == TokenKind::Let) {
                    MatchAndAdvanceToken(parser, TokenKind::Let);
                } else if (parser->tokenCur.kind == TokenKind::As) {
                    MatchAndAdvanceToken(parser, TokenKind::As);
                } else if (parser->tokenCur.kind == TokenKind::Byte) {
                    MatchAndAdvanceToken(parser, TokenKind::Byte);
                    MatchAndAdvanceToken(parser, TokenKind::Char);
                } else if (parser->tokenCur.kind == TokenKind::LocalPersist) {
                    MatchAndAdvanceToken(parser, TokenKind::LocalPersist);
                    MatchAndAdvanceToken(parser, TokenKind::Identifier);
                } else {
                    MatchAndAdvanceToken(parser, TokenKind::Fun);
                }
                foundDirectives = true;
            }
        }
        let ASTNode* statement = ParseDefinitionStatement(parser);
        ASTNodeArrayPush(&result->children, statement);
    }

    return result;
}