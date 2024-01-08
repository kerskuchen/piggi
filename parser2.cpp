#include "scanner.cpp"
#include "definitions.hpp"
// #if 0
// import "definitions.hpp"
// import "scanner.cpp"
// #endif

struct Parser2 {
    Scanner scanner;
    SyntaxTree* tree;
    SyntaxToken tokenPrev;
    SyntaxToken tokenCur;
    SyntaxToken tokenNext;
    SyntaxToken tokenNextAfter;

    int loopLevel;
    int switchCaseLevel;
    int functionLevel;
};

fun Parser2 Parser2Create(Source source) {
    let SyntaxTree* tree = (as SyntaxTree*)malloc(sizeof(SyntaxTree));
    tree->source = source;
    tree->moduleRoot = nullptr; // Will be set `_ParseModule(..)`

    let Parser2 result;
    result.tree = tree;
    result.scanner = ScannerCreate(tree);
    result.tokenPrev = SyntaxTokenCreateEmpty(tree);
    result.tokenCur = NextToken(&result.scanner);
    result.tokenNext = NextToken(&result.scanner);
    result.tokenNextAfter = NextToken(&result.scanner);
    result.loopLevel = 0;
    result.switchCaseLevel = 0;
    result.functionLevel = 0;
    return result;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Basics

fun SyntaxToken _AdvanceToken(Parser2* parser) {
    let SyntaxToken result = parser->tokenCur;
    parser->tokenPrev = parser->tokenCur;
    parser->tokenCur = parser->tokenNext;
    parser->tokenNext = parser->tokenNextAfter;
    parser->tokenNextAfter = NextToken(&parser->scanner);
    return result;
}

fun SyntaxToken _MatchAndAdvanceToken(Parser2* parser, SyntaxKind kind) {
    if (kind == parser->tokenCur.kind) {
        return _AdvanceToken(parser);
    } 

    ReportError(
        TokenGetLocation(parser->tokenCur),
        "Expected token '%s' but got token '%s'", 
        TokenKindToString(kind).cstr, TokenKindToString(parser->tokenCur.kind).cstr
    );
    exit(1);
}

fun SyntaxNode* WrapTokenInNode(Parser2* parser, SyntaxToken token) {
    let SyntaxNode* wrapper = SyntaxNodeCreate(token.kind, parser->tree);
    wrapper->token = token;
    return wrapper;
}

fun SyntaxNode* _ParseStatement(Parser2* parser);
fun SyntaxNode* _ParseExpression(Parser2* parser);
fun SyntaxNode* _ParseBinaryExpression(Parser2* parser, int parentPrecedence);
fun SyntaxNode* _ParseUnaryExpression(Parser2* parser, int parentPrecedence);

////////////////////////////////////////////////////////////////////////////////////////////////////
// Expressions

fun SyntaxNode* _ParseTypeExpression(Parser2* parser) {
    switch (parser->tokenCur.kind) {
        case SyntaxKind::VoidKeyword:
        case SyntaxKind::CharKeyword:
        case SyntaxKind::BoolKeyword:
        case SyntaxKind::ByteKeyword:
        case SyntaxKind::ShortKeyword:
        case SyntaxKind::IntKeyword:
        case SyntaxKind::LongKeyword:
        case SyntaxKind::CStringKeyword:
        case SyntaxKind::IdentifierToken:
            break;
        default: 
            ReportError(
                TokenGetLocation(parser->tokenCur),
                "Expected primitive type or identifier token - got token '%s'", 
                TokenGetText(parser->tokenCur).cstr
            );
    }

    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::TypeExpression, parser->tree);
    result->typeExpr.typeTokens = SyntaxNodeArrayCreate();

    let SyntaxToken identifier = _AdvanceToken(parser);
    let SyntaxNode* identifierWrapper = WrapTokenInNode(parser, identifier);
    SyntaxNodeArrayPush(&result->typeExpr.typeTokens, identifierWrapper);

    while (parser->tokenCur.kind == SyntaxKind::StarToken) {
        let SyntaxToken star = _AdvanceToken(parser);
        let SyntaxNode* starWrapper = WrapTokenInNode(parser, star);
        SyntaxNodeArrayPush(&result->typeExpr.typeTokens, starWrapper);
    }

    return result;
}

fun SyntaxNode* _ParseFunctionCallExpression(Parser2* parser, SyntaxNode* func) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::FuncCallExpression, parser->tree);
    result->funcCallExpr.func = func;
    result->funcCallExpr.leftParen = _MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);

    result->funcCallExpr.argumentsWithSeparators = SyntaxNodeArrayCreate();
    while (parser->tokenCur.kind != SyntaxKind::RightParenToken) {
        let SyntaxNode* argument = _ParseExpression(parser);
        SyntaxNodeArrayPush(&result->funcCallExpr.argumentsWithSeparators, argument);
        
        if (parser->tokenCur.kind == SyntaxKind::CommaToken) {
            let SyntaxToken comma = _MatchAndAdvanceToken(parser, SyntaxKind::CommaToken);
            let SyntaxNode* commaWrapper = WrapTokenInNode(parser, comma);
            SyntaxNodeArrayPush(&result->funcCallExpr.argumentsWithSeparators, commaWrapper);
        } else {
            break;
        }
    }
    result->funcCallExpr.rightParen = _MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);
    return result;
}

fun SyntaxNode* _ParseArrayIndexingExpression(Parser2* parser, SyntaxNode* arr) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::ArrayIndexExpression, parser->tree);
    result->arrayIndexExpr.arr = arr;
    result->arrayIndexExpr.leftBracket = _MatchAndAdvanceToken(parser, SyntaxKind::LeftBracketToken);
    result->arrayIndexExpr.indexExpression = _ParseExpression(parser);
    result->arrayIndexExpr.rightBracket = _MatchAndAdvanceToken(parser, SyntaxKind::RightBracketToken);
    return result;
}

fun SyntaxNode* _ParseMemberAccess(Parser2* parser, SyntaxNode* container) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::MemberAccessExpression, parser->tree);
    result->memberAccessExpr.container = container;
    if (parser->tokenCur.kind == SyntaxKind::ArrowToken) 
        result->memberAccessExpr.accessToken = _MatchAndAdvanceToken(parser, SyntaxKind::ArrowToken);
    else
        result->memberAccessExpr.accessToken = _MatchAndAdvanceToken(parser, SyntaxKind::DotToken);
    result->memberAccessExpr.memberIdentifier = _MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
    return result;
}

fun SyntaxNode* _ParseEnumLiteralExpression(Parser2* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::EnumValueLiteralExpression, parser->tree);
    result->enumLiteralExpr.enumIdentifier = _MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
    result->enumLiteralExpr.coloncolon = _MatchAndAdvanceToken(parser, SyntaxKind::ColonColonToken);
    result->enumLiteralExpr.valueIdentifier = _MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
    return result;
}

fun SyntaxNode* _ParsePrimaryExpression(Parser2* parser) {
    switch (parser->tokenCur.kind) {
        case SyntaxKind::LeftParenToken: {
            let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::ParenthesizedExpression, parser->tree);
            result->parenthesizedExpr.leftParen = _MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
            result->parenthesizedExpr.expression = _ParseExpression(parser);
            result->parenthesizedExpr.rightParen = _MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);
            return result;
        }
        case SyntaxKind::SizeOfKeyword: {
            let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::SizeOfExpression, parser->tree);
            result->sizeofExpr.sizeofKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::SizeOfKeyword);
            result->sizeofExpr.leftParen = _MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
            result->sizeofExpr.typeExpression = _ParseTypeExpression(parser);
            result->sizeofExpr.rightParen = _MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);
            return result;
        }
        case SyntaxKind::IdentifierToken: {
            if (parser->tokenNext.kind == SyntaxKind::ColonColonToken)
                return _ParseEnumLiteralExpression(parser);

            let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::NameExpression, parser->tree);
            result->nameExpr.identifier = _MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
            return result;
        }
        case SyntaxKind::StringLiteralToken: {
            let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::StringLiteralExpression, parser->tree);
            result->stringLiteralExpr.stringLiteralTokens = SyntaxNodeArrayCreate();
            while (parser->tokenCur.kind == SyntaxKind::StringLiteralToken) {
                let SyntaxToken literal = _MatchAndAdvanceToken(parser, SyntaxKind::StringLiteralToken);
                let SyntaxNode* literalWrapper = WrapTokenInNode(parser, literal);
                SyntaxNodeArrayPush(&result->stringLiteralExpr.stringLiteralTokens, literalWrapper);
            }
            return result;
        }
        case SyntaxKind::NullKeyword: {
            let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::NullLiteralExpression, parser->tree);
            result->nullLiteralExpr.nullLiteral = _MatchAndAdvanceToken(parser, SyntaxKind::NullKeyword);
            return result;
        }
        case SyntaxKind::CharacterLiteralToken: {
            let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::CharacterLiteralExpression, parser->tree);
            result->characterLiteralExpr.characterLiteral = _MatchAndAdvanceToken(parser, SyntaxKind::CharacterLiteralToken);
            return result;
        }
        case SyntaxKind::FalseKeyword:
        case SyntaxKind::TrueKeyword: {
            let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::BoolLiteralExpression, parser->tree);
            result->boolLiteralExpr.boolLiteral = _AdvanceToken(parser);
            return result;
        }
        case SyntaxKind::IntegerLiteralToken:
        default: {
            let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::IntegerLiteralExpression, parser->tree);
            result->integerLiteralExpr.integerLiteral = _MatchAndAdvanceToken(parser, SyntaxKind::IntegerLiteralToken);
            return result;
        }
    }
}

fun SyntaxNode* _ParseArrayLiteralExpression(Parser2* parser) { 
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::ArrayLiteralExpression, parser->tree);
    result->arrayLiteralExpr.leftBrace = _MatchAndAdvanceToken(parser, SyntaxKind::LeftBraceToken);

    result->arrayLiteralExpr.elemsWithSeparators = SyntaxNodeArrayCreate();
    while (parser->tokenCur.kind != SyntaxKind::RightBraceToken) {
        let SyntaxNode* expression = _ParseExpression(parser);
        SyntaxNodeArrayPush(&result->arrayLiteralExpr.elemsWithSeparators, expression);
        if (parser->tokenCur.kind == SyntaxKind::CommaToken) {
            let SyntaxToken comma = _MatchAndAdvanceToken(parser, SyntaxKind::CommaToken);
            let SyntaxNode* commaWrapper = WrapTokenInNode(parser, comma);
            SyntaxNodeArrayPush(&result->arrayLiteralExpr.elemsWithSeparators, commaWrapper);
        } else {
            break;
        }
    }
    result->arrayLiteralExpr.rightBrace = _MatchAndAdvanceToken(parser, SyntaxKind::RightBraceToken);
    return result;
}

fun SyntaxNode* _ParsePostFixExpression(Parser2* parser) {
    let SyntaxNode* left = _ParsePrimaryExpression(parser);

    let bool foundPostfix = false;
    do {
        foundPostfix = false;
        if (parser->tokenCur.kind == SyntaxKind::LeftParenToken) {
            foundPostfix = true;
            left =  _ParseFunctionCallExpression(parser, left);
        }
        if (parser->tokenCur.kind == SyntaxKind::DotToken || parser->tokenCur.kind == SyntaxKind::ArrowToken) {
            foundPostfix = true;
            left = _ParseMemberAccess(parser, left);
        }
        if (parser->tokenCur.kind == SyntaxKind::LeftBracketToken) {
            foundPostfix = true;
            left = _ParseArrayIndexingExpression(parser, left);
        }
    } while(foundPostfix);

    return left;
}

fun SyntaxNode* _ParseCastExpression(Parser2* parser) {
    if (parser->tokenCur.kind != SyntaxKind::LeftParenToken || parser->tokenNext.kind != SyntaxKind::AsKeyword)
        return _ParsePostFixExpression(parser);

    let SyntaxNode* result =  SyntaxNodeCreate(SyntaxKind::TypeCastExpression, parser->tree);
    result->typeCastExpr.leftParen = _MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
    result->typeCastExpr.asKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::AsKeyword);
    result->typeCastExpr.targetTypeExpression = _ParseTypeExpression(parser);
    result->typeCastExpr.rightParen = _MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);
    result->typeCastExpr.expression = _ParseUnaryExpression(parser, 0);
    return result;
}

fun SyntaxNode* _ParseUnaryExpression(Parser2* parser, int parentPrecedence) {
    let SyntaxNode* left = nullptr;
    let int unaryOperatorPrecedence = GetUnaryOperatorPrecedence(parser->tokenCur.kind);
    if ((unaryOperatorPrecedence != 0) && (unaryOperatorPrecedence >= parentPrecedence)) {
        let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::UnaryExpression, parser->tree);
        result->unaryExpr.operatorToken = _AdvanceToken(parser);
        result->unaryExpr.operand = _ParseBinaryExpression(parser, unaryOperatorPrecedence);
        left = result;
    } else {
      left = _ParseCastExpression(parser);
    }
    return left;
}

fun SyntaxNode* _ParseBinaryExpression(Parser2* parser, int parentPrecedence) {
    let SyntaxNode* left = _ParseUnaryExpression(parser, parentPrecedence);

    while (true) {
        let int precedence = GetBinaryOperatorPrecedence(parser->tokenCur.kind);
        if (precedence == 0
         || precedence < parentPrecedence 
         || precedence == parentPrecedence && !IsBinaryOperatorRightAssociative(parser->tokenCur.kind)) {
            break;
        }
        let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::BinaryExpression, parser->tree);
        result->binaryExpr.left = left;
        result->binaryExpr.operatorToken = _AdvanceToken(parser);
        result->binaryExpr.right = _ParseBinaryExpression(parser, precedence);
        left = result;
    }
    return left;
}

fun SyntaxNode* _ParseTernaryConditionExpression(Parser2* parser) {
    let SyntaxNode* left = _ParseBinaryExpression(parser, 0);

    if (parser->tokenCur.kind == SyntaxKind::QuestionmarkToken) {
        let SyntaxNode* ternary = SyntaxNodeCreate(SyntaxKind::TernaryConditionalExpression, parser->tree);
        ternary->ternaryConditionalExpr.conditionExpression = left;
        ternary->ternaryConditionalExpr.questionmark = _MatchAndAdvanceToken(parser, SyntaxKind::QuestionmarkToken);
        ternary->ternaryConditionalExpr.thenExpression = _ParseTernaryConditionExpression(parser);
        ternary->ternaryConditionalExpr.colon = _MatchAndAdvanceToken(parser, SyntaxKind::ColonToken);
        ternary->ternaryConditionalExpr.elseExpression = _ParseTernaryConditionExpression(parser);
        return ternary;
    }
    return left;
}

fun SyntaxNode* _ParseAssignmentExpression(Parser2* parser) {
    let SyntaxNode* left = _ParseTernaryConditionExpression(parser);

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
            let SyntaxNode* assignment = SyntaxNodeCreate(SyntaxKind::BinaryExpression, parser->tree);
            assignment->binaryExpr.left = left;
            assignment->binaryExpr.operatorToken = _AdvanceToken(parser);
            assignment->binaryExpr.right = _ParseAssignmentExpression(parser);
            return assignment;
        } 
    }
    return left;
}

fun SyntaxNode* _ParseExpression(Parser2* parser) {
    return _ParseAssignmentExpression(parser);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Statements

fun SyntaxNode* _ParseExpressionStatement(Parser2* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::ExpressionStatement, parser->tree);
    result->expressionStmt.expression = _ParseExpression(parser);
    result->expressionStmt.semicolon = _MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
    return result;
}

fun SyntaxNode* _ParseVariableDefinitionStatement(Parser2* parser, bool skipLet, bool allowInitializer, SyntaxKind terminator) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::VariableDeclarationStatement, parser->tree);

    if (skipLet) {
        result->variableDeclarationStmt.letKeyword = SyntaxTokenCreateEmpty(parser->tree);
    } else {
        if (parser->tokenCur.kind == SyntaxKind::LetLocalPersistKeyword) 
            result->variableDeclarationStmt.letKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::LetLocalPersistKeyword);
        else
            result->variableDeclarationStmt.letKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::LetKeyword);
    }

    result->variableDeclarationStmt.typeExpression = _ParseTypeExpression(parser);
    result->variableDeclarationStmt.identifier = _MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);

    if (parser->tokenCur.kind == SyntaxKind::LeftBracketToken) {
        // Array definition
        result->variableDeclarationStmt.leftBracket = _MatchAndAdvanceToken(parser, SyntaxKind::LeftBracketToken);
        if (parser->tokenCur.kind == SyntaxKind::IntegerLiteralToken)
            result->variableDeclarationStmt.arraySizeLiteral = _MatchAndAdvanceToken(parser, SyntaxKind::IntegerLiteralToken);
        result->variableDeclarationStmt.rightBracket = _MatchAndAdvanceToken(parser, SyntaxKind::RightBracketToken);

        if (parser->tokenCur.kind == SyntaxKind::EqualsToken && allowInitializer) {
            result->variableDeclarationStmt.equalsToken = _MatchAndAdvanceToken(parser, SyntaxKind::EqualsToken);
            result->variableDeclarationStmt.initializerExpression = _ParseArrayLiteralExpression(parser);
        } else {
            result->variableDeclarationStmt.equalsToken = SyntaxTokenCreateEmpty(parser->tree);
            result->variableDeclarationStmt.initializerExpression = nullptr;
        }
    } else {
        // Regular non-array variable
        result->variableDeclarationStmt.leftBracket = SyntaxTokenCreateEmpty(parser->tree);
        result->variableDeclarationStmt.arraySizeLiteral = SyntaxTokenCreateEmpty(parser->tree);
        result->variableDeclarationStmt.rightBracket = SyntaxTokenCreateEmpty(parser->tree);

        if (parser->tokenCur.kind == SyntaxKind::EqualsToken && allowInitializer) {
            result->variableDeclarationStmt.equalsToken = _MatchAndAdvanceToken(parser, SyntaxKind::EqualsToken);
            result->variableDeclarationStmt.initializerExpression = _ParseExpression(parser);
        } else {
            result->variableDeclarationStmt.equalsToken = SyntaxTokenCreateEmpty(parser->tree);
            result->variableDeclarationStmt.initializerExpression = nullptr;
        }
    }

    if (terminator == SyntaxKind::BadToken) 
        result->variableDeclarationStmt.terminatorToken = SyntaxTokenCreateEmpty(parser->tree);
    else
        result->variableDeclarationStmt.terminatorToken = _MatchAndAdvanceToken(parser, terminator);

    return result;
}

fun SyntaxNode* _ParseIfStatement(Parser2* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::IfStatement, parser->tree);
    result->ifStmt.ifKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::IfKeyword);
    result->ifStmt.leftParen = _MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
    result->ifStmt.condition = _ParseExpression(parser);
    result->ifStmt.rightParen = _MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);

    result->ifStmt.thenBlock = _ParseStatement(parser);
    result->ifStmt.elseBlock = nullptr;
    result->ifStmt.elseKeyword = SyntaxTokenCreateEmpty(parser->tree);
    if (parser->tokenCur.kind == SyntaxKind::ElseKeyword) {
        result->ifStmt.elseKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::ElseKeyword);
        result->ifStmt.elseBlock = _ParseStatement(parser);
    }
    return result;
}

fun SyntaxNode* _ParseWhileStatement(Parser2* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::WhileStatement, parser->tree);
    result->whileStmt.whileKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::WhileKeyword);
    result->whileStmt.leftParen = _MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
    result->whileStmt.condition = _ParseExpression(parser);
    result->whileStmt.rightParen = _MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);

    parser->loopLevel += 1;
    result->whileStmt.body = _ParseStatement(parser);
    parser->loopLevel -= 1;
    return result;
}

fun SyntaxNode* _ParseDoWhileStatement(Parser2* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::DoWhileStatement, parser->tree);
    result->doWhileStmt.doKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::DoKeyword);
    parser->loopLevel += 1;
    result->doWhileStmt.body = _ParseStatement(parser);
    parser->loopLevel -= 1;

    result->doWhileStmt.whileKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::WhileKeyword);
    result->doWhileStmt.leftParen = _MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
    result->doWhileStmt.condition = _ParseExpression(parser);
    result->doWhileStmt.rightParen = _MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);
    result->doWhileStmt.semicolon = _MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
    return result;
}

fun SyntaxNode* _ParseForStatement(Parser2* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::ForStatement, parser->tree);
    result->forStmt.forKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::ForKeyword);

    result->forStmt.leftParen = _MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
    if (parser->tokenCur.kind == SyntaxKind::LetKeyword)
        result->forStmt.initializerStatement = _ParseVariableDefinitionStatement(parser, false, true, SyntaxKind::SemicolonToken);
    else 
        result->forStmt.initializerStatement = _ParseExpressionStatement(parser);
    result->forStmt.conditionStatement = _ParseExpressionStatement(parser);
    result->forStmt.incrementExpression = _ParseExpression(parser);
    result->forStmt.rightParen = _MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);

    parser->loopLevel += 1;
    result->forStmt.body = _ParseStatement(parser);
    parser->loopLevel -= 1;
    return result;
}

fun SyntaxNode* _ParseReturnStatement(Parser2* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::ReturnStatement, parser->tree);
    result->returnStmt.returnKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::ReturnKeyword);
    if (parser->functionLevel == 0) {
        ReportError(
            TokenGetLocation(result->returnStmt.returnKeyword),
            "Invalid 'return' keyword found outside of function definition"
        );
    }
    if (parser->tokenCur.kind != SyntaxKind::SemicolonToken)
        result->returnStmt.returnExpression = _ParseExpression(parser);
    else
        result->returnStmt.returnExpression = nullptr;
    result->returnStmt.semicolon = _MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
    return result;
}

fun SyntaxNode* _ParseBreakStatement(Parser2* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::BreakStatement, parser->tree);
    result->breakStmt.breakKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::BreakKeyword);
    if (parser->loopLevel == 0 && parser->switchCaseLevel == 0) {
        ReportError(
            TokenGetLocation(result->breakStmt.breakKeyword),
            "Invalid 'break' keyword found outside of loop or switch-case definition"
        );
    }
    result->returnStmt.semicolon = _MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
    return result;
}

fun SyntaxNode* _ParseContinueStatement(Parser2* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::ContinueStatement, parser->tree);
    result->continueStmt.continueKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::ContinueKeyword);
    if (parser->loopLevel == 0) {
        ReportError(
            TokenGetLocation(result->continueStmt.continueKeyword),
            "Invalid 'continue' keyword found outside of loop definition"
        );
    }
    result->returnStmt.semicolon = _MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
    return result;
}

fun SyntaxNode* _ParseBlockStatement(Parser2* parser, bool inSwitch) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::BlockStatement, parser->tree);
    if (parser->tokenCur.kind == SyntaxKind::LeftBraceToken || !inSwitch) {
        result->blockStmt.leftBrace = _MatchAndAdvanceToken(parser, SyntaxKind::LeftBraceToken);
    } else {
        // We allow omitting the braces in switch statements
        result->blockStmt.leftBrace = SyntaxTokenCreateEmpty(parser->tree);
    }

    result->blockStmt.statements = SyntaxNodeArrayCreate();
    while (parser->tokenCur.kind != SyntaxKind::RightBraceToken) {
        if (inSwitch && parser->tokenCur.kind == SyntaxKind::CaseKeyword)
            break;
        if (inSwitch && parser->tokenCur.kind == SyntaxKind::DefaultKeyword)
            break;
        let SyntaxNode* statement = _ParseStatement(parser);
        SyntaxNodeArrayPush(&result->blockStmt.statements, statement);
    }

    if (result->blockStmt.leftBrace.kind != SyntaxKind::BadToken ) {
        // If we start with a brace we also must end with a brace
        result->blockStmt.rightBrace = _MatchAndAdvanceToken(parser, SyntaxKind::RightBraceToken);
    }
    return result;
}

fun SyntaxNode* _ParseCaseStatement(Parser2* parser, SyntaxNode* switchExpression) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::CaseStatement, parser->tree);
    result->caseStmt.caseKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::CaseKeyword);

    if (parser->switchCaseLevel == 0) {
        ReportError(
            TokenGetLocation(result->caseStmt.caseKeyword), 
            "Unexpected 'case' label keyword outside of switch statement"
        );
    }
    result->caseStmt.literalExpression = _ParseExpression(parser);
    switch (result->caseStmt.literalExpression->kind)
    {
        case SyntaxKind::IntegerLiteralExpression:
        case SyntaxKind::StringLiteralExpression:
        case SyntaxKind::CharacterLiteralExpression:
        case SyntaxKind::EnumValueLiteralExpression:
            break;
        default:
            ReportError(
                // TODO we want the location of the failed expression here instead of the case token
                TokenGetLocation(result->caseStmt.caseKeyword), 
                "Expected literal token in case label but got '%s' instead",
                "TODO - we need to get the string of an expression"
                // TokenKindToString(result->caseStmt.literalExpression).cstr
            );
    }
    result->caseStmt.colon = _MatchAndAdvanceToken(parser, SyntaxKind::ColonToken);
    result->caseStmt.body = _ParseBlockStatement(parser, true);
    return result;
}

fun SyntaxNode* _ParseDefaultStatement(Parser2* parser, SyntaxNode* switchExpression) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::DefaultStatement, parser->tree);
    result->defaultStmt.defaultKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::DefaultKeyword);
    if (parser->switchCaseLevel == 0) {
        ReportError(
            TokenGetLocation(result->defaultStmt.defaultKeyword), 
            "Unexpected 'default' case label keyword outside of switch statement"
        );
    }
    result->defaultStmt.colon = _MatchAndAdvanceToken(parser, SyntaxKind::ColonToken);
    result->defaultStmt.body = _ParseBlockStatement(parser, true);
    return result;
}

fun SyntaxNode* _ParseSwitchStatement(Parser2* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::SwitchStatement, parser->tree);
    result->switchStmt.switchKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::SwitchKeyword);
    result->switchStmt.leftParen = _MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
    result->switchStmt.switchExpression = _ParseExpression(parser);
    result->switchStmt.rightParen = _MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);

    result->switchStmt.leftBrace = _MatchAndAdvanceToken(parser, SyntaxKind::LeftBraceToken);
    parser->switchCaseLevel += 1;

    result->switchStmt.caseStatements = SyntaxNodeArrayCreate();
    while (parser->tokenCur.kind != SyntaxKind::EndOfFileToken) {
        if (parser->tokenCur.kind == SyntaxKind::RightBraceToken)
            break;
        if (parser->tokenCur.kind == SyntaxKind::CaseKeyword) {
            let SyntaxNode* caseStatement = _ParseCaseStatement(parser, result);
            SyntaxNodeArrayPush(&result->switchStmt.caseStatements, caseStatement);
        } else {
            let SyntaxNode* defaultStatement = _ParseDefaultStatement(parser, result);
            SyntaxNodeArrayPush(&result->switchStmt.caseStatements, defaultStatement);
        }
    }

    parser->switchCaseLevel -= 1;
    result->switchStmt.rightBrace = _MatchAndAdvanceToken(parser, SyntaxKind::RightBraceToken);
    return result;
}

fun SyntaxNode* _ParseStatement(Parser2* parser) {
    assert(parser->functionLevel > 0);
    switch (parser->tokenCur.kind) {
        case SyntaxKind::LeftBraceToken:
            return _ParseBlockStatement(parser, false);
        case SyntaxKind::IfKeyword:
            return _ParseIfStatement(parser);
        case SyntaxKind::DoKeyword:
            return _ParseDoWhileStatement(parser);
        case SyntaxKind::WhileKeyword:
            return _ParseWhileStatement(parser);
        case SyntaxKind::ForKeyword:
            return _ParseForStatement(parser);
        case SyntaxKind::ReturnKeyword:
            return _ParseReturnStatement(parser);
        case SyntaxKind::BreakKeyword:
            return _ParseBreakStatement(parser);
        case SyntaxKind::ContinueKeyword:
            return _ParseContinueStatement(parser);
        case SyntaxKind::SwitchKeyword:
            return _ParseSwitchStatement(parser);
        case SyntaxKind::LetKeyword:
        case SyntaxKind::LetLocalPersistKeyword:
            return _ParseVariableDefinitionStatement(parser, false, true, SyntaxKind::SemicolonToken);
        default:
            return _ParseExpressionStatement(parser);
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Module

fun SyntaxNode* _ParseStructOrUnionDefinitionStatement(Parser2* parser, SyntaxToken externKeyword) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::StructOrUnionDeclarationStatement, parser->tree);
    result->structOrUnionDeclarationStmt.externKeyword = externKeyword;
    if (parser->tokenCur.kind == SyntaxKind::StructKeyword)
        result->structOrUnionDeclarationStmt.structOrUnionKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::StructKeyword);
    else 
        result->structOrUnionDeclarationStmt.structOrUnionKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::UnionKeyword);
    result->structOrUnionDeclarationStmt.identifier = _MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);

    if (parser->tokenCur.kind == SyntaxKind::SemicolonToken) {
        result->structOrUnionDeclarationStmt.semicolon = _MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
        return result;
    } else {
        result->kind = SyntaxKind::StructOrUniontDefinitionStatement;
        result->structOrUnionDefinitionStmt.memberDeclarationStatements = SyntaxNodeArrayCreate();

        result->structOrUnionDefinitionStmt.leftBrace = _MatchAndAdvanceToken(parser, SyntaxKind::LeftBraceToken);
        while (parser->tokenCur.kind != SyntaxKind::RightBraceToken) {
            let SyntaxNode* memberDeclaration = _ParseVariableDefinitionStatement(parser, true, false, SyntaxKind::SemicolonToken);
            SyntaxNodeArrayPush(&result->structOrUnionDefinitionStmt.memberDeclarationStatements, memberDeclaration);
        }
        result->structOrUnionDefinitionStmt.rightBrace = _MatchAndAdvanceToken(parser, SyntaxKind::RightBraceToken);
        result->structOrUnionDefinitionStmt.semicolon = _MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
        return result;
    }
}

fun SyntaxNode* _ParseEnumMemberClause(Parser2* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::EnumMemberClauseSyntax, parser->tree);

    result->enumMember.identifier = _MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
    if (parser->tokenCur.kind == SyntaxKind::EqualsToken) {
        result->enumMember.equals = _MatchAndAdvanceToken(parser, SyntaxKind::EqualsToken);
        result->enumMember.integerLiteral = _MatchAndAdvanceToken(parser, SyntaxKind::IntegerLiteralToken);
    } else {
        result->enumMember.equals = SyntaxTokenCreateEmpty(parser->tree);
        result->enumMember.integerLiteral = SyntaxTokenCreateEmpty(parser->tree);
    }

    if (parser->tokenCur.kind == SyntaxKind::CommaToken)
        result->enumMember.comma = _MatchAndAdvanceToken(parser, SyntaxKind::CommaToken);
    else
        result->enumMember.comma = SyntaxTokenCreateEmpty(parser->tree);

    return result;
}

fun SyntaxNode* _ParseEnumDefinitionStatement(Parser2* parser, SyntaxToken externKeyword) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::EnumDeclarationStatement, parser->tree);
    result->enumDeclarationStmt.externKeyword = externKeyword;
    result->enumDeclarationStmt.enumKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::EnumKeyword);
    result->enumDeclarationStmt.classKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::ClassKeyword);
    result->enumDeclarationStmt.identifier = _MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);

    if (parser->tokenCur.kind == SyntaxKind::SemicolonToken) {
        // Just a forward declaration
        result->enumDeclarationStmt.semicolon = _MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
        return result;
    } else {
        // Definition including body
        result->kind = SyntaxKind::EnumDefinitionStatement;
        result->enumDefinitionStmt.memberClauses = SyntaxNodeArrayCreate();

        result->enumDefinitionStmt.leftBrace = _MatchAndAdvanceToken(parser, SyntaxKind::LeftBraceToken);
        while (parser->tokenCur.kind != SyntaxKind::RightBraceToken) {
            let SyntaxNode* memberClause = _ParseEnumMemberClause(parser);
            SyntaxNodeArrayPush(&result->enumDefinitionStmt.memberClauses, memberClause);
            if (memberClause->enumMember.comma.kind != SyntaxKind::CommaToken)
                break;
        }
        result->enumDefinitionStmt.rightBrace = _MatchAndAdvanceToken(parser, SyntaxKind::RightBraceToken);
        result->enumDefinitionStmt.semicolon = _MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
        return result;
    }
}

fun SyntaxNode* _ParseFunctionDefinitionStatement(Parser2* parser, SyntaxToken externKeyword) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::FunctionDeclarationStatement, parser->tree);
    result->functionDeclarationStmt.externKeyword = externKeyword;
    result->functionDeclarationStmt.funKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::FunKeyword);
    result->functionDeclarationStmt.returnType = _ParseTypeExpression(parser);
    result->functionDeclarationStmt.identifier = _MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);

    result->functionDeclarationStmt.leftParen = _MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
    result->functionDeclarationStmt.params = SyntaxNodeArrayCreate();
    while (parser->tokenCur.kind != SyntaxKind::RightParenToken) {
        if (parser->tokenCur.kind == SyntaxKind::DotDotDotToken) {
            let SyntaxToken dotdot = _MatchAndAdvanceToken(parser, SyntaxKind::DotDotDotToken);
            let SyntaxNode* dotdotWrapper = WrapTokenInNode(parser, dotdot);
            SyntaxNodeArrayPush(&result->functionDeclarationStmt.params, dotdotWrapper);
            break;
        }

        let SyntaxNode* parameter = _ParseVariableDefinitionStatement(parser, true, false, SyntaxKind::BadToken);
        SyntaxNodeArrayPush(&result->functionDeclarationStmt.params, parameter);

        if (parser->tokenCur.kind == SyntaxKind::CommaToken) {
            parameter->variableDeclarationStmt.terminatorToken = _MatchAndAdvanceToken(parser, SyntaxKind::CommaToken);
        } else {
            break;
        }
    }
    result->functionDeclarationStmt.rightParen = _MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);

    if (parser->tokenCur.kind == SyntaxKind::SemicolonToken) {
        // Just a forward declaration
        result->functionDeclarationStmt.semicolon = _MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
        return result;
    } else {
        // Definition including body
        result->kind = SyntaxKind::FunctionDefinitionStatement;
        parser->functionLevel += 1;
        result->functionDefinitionStmt.body = _ParseBlockStatement(parser, false);
        parser->functionLevel -= 1;
        return result;
    }
}

fun SyntaxNode* _ParseGlobalVariableDefinitionStatement(Parser2* parser, SyntaxToken externKeyword) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::GlobalVariableDeclarationStatement, parser->tree);
    result->globalVariableStmt.externKeyword = externKeyword;
    result->globalVariableStmt.variableDeclarationStatement = _ParseVariableDefinitionStatement(parser, false, true, SyntaxKind::SemicolonToken);
    return result;
}

fun SyntaxNode* _ParseGlobalDefinitionStatement(Parser2* parser) {
    assert(parser->functionLevel == 0);

    let SyntaxToken externKeyword = SyntaxTokenCreateEmpty(parser->tree);
    if (parser->tokenCur.kind == SyntaxKind::ExternKeyword) 
        externKeyword = _AdvanceToken(parser);

    switch (parser->tokenCur.kind) {
        case SyntaxKind::EnumKeyword:
            return _ParseEnumDefinitionStatement(parser, externKeyword);
        case SyntaxKind::StructKeyword: 
        case SyntaxKind::UnionKeyword:
            return _ParseStructOrUnionDefinitionStatement(parser, externKeyword);
        case SyntaxKind::FunKeyword:
            return _ParseFunctionDefinitionStatement(parser, externKeyword);
        default:
            return _ParseGlobalVariableDefinitionStatement(parser, externKeyword);
    }
}

fun SyntaxNode* _ParseImportDeclarationStatement(Parser2* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::ImportDeclarationStatement, parser->tree);
    result->importStmt.importKeyword = _MatchAndAdvanceToken(parser, SyntaxKind::ImportKeyword);
    result->importStmt.modulenameLiteral = _MatchAndAdvanceToken(parser, SyntaxKind::StringLiteralToken);
    return result;
}

fun SyntaxNode* _ParseModuleStatement(Parser2* parser) {
    switch (parser->tokenCur.kind) {
        case SyntaxKind::ExternKeyword:
        case SyntaxKind::StructKeyword:
        case SyntaxKind::UnionKeyword:
        case SyntaxKind::EnumKeyword:
        case SyntaxKind::FunKeyword:
        case SyntaxKind::LetKeyword:
            return _ParseGlobalDefinitionStatement(parser);
        case SyntaxKind::IncludeDirectiveKeyword:
            return _ParseImportDeclarationStatement(parser);
        default:
            ReportError(
                TokenGetLocation(parser->tokenCur),
                "Expected global module definition got unexpected token '%s' instead",
                TokenGetText(parser->tokenCur).cstr
            );
            exit(1);
    }
}

fun SyntaxTree* _ParseModule(Parser2* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::Module, parser->tree);
    parser->tree->moduleRoot = (as ModuleStatementSyntax*)result;
    result->moduleStmt.globalStatements = SyntaxNodeArrayCreate();

    while (parser->tokenCur.kind != SyntaxKind::EndOfFileToken) {
        // TODO get rid of these hacks when we do our own language
        // Skip pragmas, include directives, define directives, typedefs
        let bool foundDirectives = false;
        do { 
            foundDirectives = false;
            if (parser->tokenCur.kind == SyntaxKind::TypedefKeyword) {
                _MatchAndAdvanceToken(parser, SyntaxKind::TypedefKeyword);
                while (parser->tokenCur.kind != SyntaxKind::SemicolonToken) {
                    _AdvanceToken(parser); // Ignore everything in the typedef
                }
                _MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
                foundDirectives = true;
            }
            if (parser->tokenCur.kind == SyntaxKind::IncludeDirectiveKeyword) {
                _MatchAndAdvanceToken(parser, SyntaxKind::IncludeDirectiveKeyword);
                if (parser->tokenCur.kind == SyntaxKind::LessToken) {
                    _MatchAndAdvanceToken(parser, SyntaxKind::LessToken);
                    _AdvanceToken(parser); // ex. stdio
                    _MatchAndAdvanceToken(parser, SyntaxKind::DotToken);
                    _AdvanceToken(parser); // ex. hpp
                    _MatchAndAdvanceToken(parser, SyntaxKind::GreaterToken);
                } else {
                    _MatchAndAdvanceToken(parser, SyntaxKind::StringLiteralToken);
                }
                foundDirectives = true;
            }
            if (parser->tokenCur.kind == SyntaxKind::PragmaDirectiveKeyword) {
                _MatchAndAdvanceToken(parser, SyntaxKind::PragmaDirectiveKeyword);
                _MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
                foundDirectives = true;
            }
            if (parser->tokenCur.kind == SyntaxKind::IfDirectiveKeyword) {
                _MatchAndAdvanceToken(parser, SyntaxKind::IfDirectiveKeyword);
                _AdvanceToken(parser); // Ignore 0
                foundDirectives = true;
            }
            if (parser->tokenCur.kind == SyntaxKind::EndIfDefinedDirectiveKeyword) {
                _MatchAndAdvanceToken(parser, SyntaxKind::EndIfDefinedDirectiveKeyword);
                foundDirectives = true;
            }
            if (parser->tokenCur.kind == SyntaxKind::DefineDirectiveKeyword) {
                _MatchAndAdvanceToken(parser, SyntaxKind::DefineDirectiveKeyword);
                if (parser->tokenCur.kind == SyntaxKind::LetKeyword) {
                    _MatchAndAdvanceToken(parser, SyntaxKind::LetKeyword);
                } else if (parser->tokenCur.kind == SyntaxKind::AsKeyword) {
                    _MatchAndAdvanceToken(parser, SyntaxKind::AsKeyword);
                } else if (parser->tokenCur.kind == SyntaxKind::ByteKeyword) {
                    _MatchAndAdvanceToken(parser, SyntaxKind::ByteKeyword);
                    _MatchAndAdvanceToken(parser, SyntaxKind::CharKeyword);
                } else if (parser->tokenCur.kind == SyntaxKind::LetLocalPersistKeyword) {
                    _MatchAndAdvanceToken(parser, SyntaxKind::LetLocalPersistKeyword);
                    _MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
                } else {
                    _MatchAndAdvanceToken(parser, SyntaxKind::FunKeyword);
                }
                foundDirectives = true;
            }
        } while (foundDirectives);
        let SyntaxNode* statement = _ParseModuleStatement(parser);
        SyntaxNodeArrayPush(&result->moduleStmt.globalStatements, statement);
    }

    if (parser->tokenCur.kind != SyntaxKind::EndOfFileToken)
    {
        ReportError(
            TokenGetLocation(parser->tokenCur),
            "Expected EOF token after parsing file, instead got '%s'", 
            TokenKindToString(parser->tokenCur.kind).cstr
        );
    }
    // TODO: figure out and assign all parents to all nodes in the tree here

    return parser->tree;
}