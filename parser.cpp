#include "definitions.hpp"
#include "scanner.cpp"
#if 0
import "definitions.hpp"
import "scanner.cpp"
#endif

struct Parser {
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

fun Parser ParserCreate(Source source) {
    let SyntaxTree* tree = (as SyntaxTree*)malloc(sizeof(SyntaxTree));
    tree->source = source;
    tree->moduleRoot = nullptr; // Will be set `_ParseModule(..)`

    let Parser result;
    result.tree = tree;
    result.scanner = ScannerCreate(tree);
    result.tokenPrev = SyntaxTokenCreateMissing();
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
        parser->tokenCur.location,
        "Expected token '%s' but got token '%s'", 
        TokenKindToString(kind).cstr, TokenKindToString(parser->tokenCur.kind).cstr
    );
    exit(1);
}

fun SyntaxNode* WrapTokenInNode(Parser* parser, SyntaxToken token) {
    let SyntaxNode* wrapper = SyntaxNodeCreate(token.kind, parser->tree);
    wrapper->token = token;
    return wrapper;
}

fun SyntaxNode* ParseStatement(Parser* parser);
fun SyntaxNode* ParseExpression(Parser* parser);
fun SyntaxNode* ParseBinaryExpression(Parser* parser, int parentPrecedence);
fun SyntaxNode* ParseUnaryExpression(Parser* parser, int parentPrecedence);

////////////////////////////////////////////////////////////////////////////////////////////////////
// Expressions

fun SyntaxNode* ParseTypeExpression(Parser* parser) {
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
                parser->tokenCur.location,
                "Expected primitive type or identifier token - got token '%s'", 
                TokenGetText(parser->tokenCur).cstr
            );
    }

    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::TypeExpression, parser->tree);
    result->typeExpr.typeTokens = SyntaxNodeArrayCreate();

    let SyntaxToken identifier = AdvanceToken(parser);
    let SyntaxNode* identifierWrapper = WrapTokenInNode(parser, identifier);
    SyntaxNodeArrayPush(&result->typeExpr.typeTokens, identifierWrapper);

    while (parser->tokenCur.kind == SyntaxKind::StarToken) {
        let SyntaxToken star = AdvanceToken(parser);
        let SyntaxNode* starWrapper = WrapTokenInNode(parser, star);
        SyntaxNodeArrayPush(&result->typeExpr.typeTokens, starWrapper);
    }

    return result;
}

fun SyntaxNode* ParseFunctionCallExpression(Parser* parser, SyntaxNode* func) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::FuncCallExpression, parser->tree);
    result->funcCallExpr.func = func;
    result->funcCallExpr.leftParen = MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);

    result->funcCallExpr.argumentsWithSeparators = SyntaxNodeArrayCreate();
    while (parser->tokenCur.kind != SyntaxKind::RightParenToken) {
        let SyntaxNode* argument = ParseExpression(parser);
        SyntaxNodeArrayPush(&result->funcCallExpr.argumentsWithSeparators, argument);
        
        if (parser->tokenCur.kind == SyntaxKind::CommaToken) {
            let SyntaxToken comma = MatchAndAdvanceToken(parser, SyntaxKind::CommaToken);
            let SyntaxNode* commaWrapper = WrapTokenInNode(parser, comma);
            SyntaxNodeArrayPush(&result->funcCallExpr.argumentsWithSeparators, commaWrapper);
        } else {
            break;
        }
    }
    result->funcCallExpr.rightParen = MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);
    return result;
}

fun SyntaxNode* ParseArrayIndexingExpression(Parser* parser, SyntaxNode* arr) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::ArrayIndexExpression, parser->tree);
    result->arrayIndexExpr.arr = arr;
    result->arrayIndexExpr.leftBracket = MatchAndAdvanceToken(parser, SyntaxKind::LeftBracketToken);
    result->arrayIndexExpr.indexExpression = ParseExpression(parser);
    result->arrayIndexExpr.rightBracket = MatchAndAdvanceToken(parser, SyntaxKind::RightBracketToken);
    return result;
}

fun SyntaxNode* ParseMemberAccess(Parser* parser, SyntaxNode* container) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::MemberAccessExpression, parser->tree);
    result->memberAccessExpr.container = container;
    if (parser->tokenCur.kind == SyntaxKind::ArrowToken) 
        result->memberAccessExpr.accessToken = MatchAndAdvanceToken(parser, SyntaxKind::ArrowToken);
    else
        result->memberAccessExpr.accessToken = MatchAndAdvanceToken(parser, SyntaxKind::DotToken);
    result->memberAccessExpr.memberIdentifier = MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
    return result;
}

fun SyntaxNode* ParseEnumLiteralExpression(Parser* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::EnumValueLiteralExpression, parser->tree);
    result->enumLiteralExpr.enumIdentifier = MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
    result->enumLiteralExpr.coloncolon = MatchAndAdvanceToken(parser, SyntaxKind::ColonColonToken);
    result->enumLiteralExpr.valueIdentifier = MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
    return result;
}

fun SyntaxNode* ParsePrimaryExpression(Parser* parser) {
    switch (parser->tokenCur.kind) {
        case SyntaxKind::LeftParenToken: {
            let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::ParenthesizedExpression, parser->tree);
            result->parenthesizedExpr.leftParen = MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
            result->parenthesizedExpr.expression = ParseExpression(parser);
            result->parenthesizedExpr.rightParen = MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);
            return result;
        }
        case SyntaxKind::SizeOfKeyword: {
            let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::SizeOfExpression, parser->tree);
            result->sizeofExpr.sizeofKeyword = MatchAndAdvanceToken(parser, SyntaxKind::SizeOfKeyword);
            result->sizeofExpr.leftParen = MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
            result->sizeofExpr.typeExpression = ParseTypeExpression(parser);
            result->sizeofExpr.rightParen = MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);
            return result;
        }
        case SyntaxKind::IdentifierToken: {
            if (parser->tokenNext.kind == SyntaxKind::ColonColonToken)
                return ParseEnumLiteralExpression(parser);

            let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::NameExpression, parser->tree);
            result->nameExpr.identifier = MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
            return result;
        }
        case SyntaxKind::StringLiteralToken: {
            let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::StringLiteralExpression, parser->tree);
            result->stringLiteralExpr.stringLiteralTokens = SyntaxNodeArrayCreate();
            while (parser->tokenCur.kind == SyntaxKind::StringLiteralToken) {
                let SyntaxToken literal = MatchAndAdvanceToken(parser, SyntaxKind::StringLiteralToken);
                let SyntaxNode* literalWrapper = WrapTokenInNode(parser, literal);
                SyntaxNodeArrayPush(&result->stringLiteralExpr.stringLiteralTokens, literalWrapper);
            }
            return result;
        }
        case SyntaxKind::NullKeyword: {
            let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::NullLiteralExpression, parser->tree);
            result->nullLiteralExpr.nullLiteral = MatchAndAdvanceToken(parser, SyntaxKind::NullKeyword);
            return result;
        }
        case SyntaxKind::CharacterLiteralToken: {
            let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::CharacterLiteralExpression, parser->tree);
            result->characterLiteralExpr.characterLiteral = MatchAndAdvanceToken(parser, SyntaxKind::CharacterLiteralToken);
            return result;
        }
        case SyntaxKind::FalseKeyword:
        case SyntaxKind::TrueKeyword: {
            let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::BoolLiteralExpression, parser->tree);
            result->boolLiteralExpr.boolLiteral = AdvanceToken(parser);
            return result;
        }
        case SyntaxKind::IntegerLiteralToken:
        default: {
            let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::IntegerLiteralExpression, parser->tree);
            result->integerLiteralExpr.integerLiteral = MatchAndAdvanceToken(parser, SyntaxKind::IntegerLiteralToken);
            return result;
        }
    }
}

fun SyntaxNode* ParseArrayLiteralExpression(Parser* parser) { 
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::ArrayLiteralExpression, parser->tree);
    result->arrayLiteralExpr.leftBrace = MatchAndAdvanceToken(parser, SyntaxKind::LeftBraceToken);

    result->arrayLiteralExpr.elemsWithSeparators = SyntaxNodeArrayCreate();
    while (parser->tokenCur.kind != SyntaxKind::RightBraceToken) {
        let SyntaxNode* expression = ParseExpression(parser);
        SyntaxNodeArrayPush(&result->arrayLiteralExpr.elemsWithSeparators, expression);
        if (parser->tokenCur.kind == SyntaxKind::CommaToken) {
            let SyntaxToken comma = MatchAndAdvanceToken(parser, SyntaxKind::CommaToken);
            let SyntaxNode* commaWrapper = WrapTokenInNode(parser, comma);
            SyntaxNodeArrayPush(&result->arrayLiteralExpr.elemsWithSeparators, commaWrapper);
        } else {
            break;
        }
    }
    result->arrayLiteralExpr.rightBrace = MatchAndAdvanceToken(parser, SyntaxKind::RightBraceToken);
    return result;
}

fun SyntaxNode* ParsePostFixExpression(Parser* parser) {
    let SyntaxNode* left = ParsePrimaryExpression(parser);

    let bool foundPostfix = false;
    do {
        foundPostfix = false;
        if (parser->tokenCur.kind == SyntaxKind::LeftParenToken) {
            foundPostfix = true;
            left =  ParseFunctionCallExpression(parser, left);
        }
        if (parser->tokenCur.kind == SyntaxKind::DotToken || parser->tokenCur.kind == SyntaxKind::ArrowToken) {
            foundPostfix = true;
            left = ParseMemberAccess(parser, left);
        }
        if (parser->tokenCur.kind == SyntaxKind::LeftBracketToken) {
            foundPostfix = true;
            left = ParseArrayIndexingExpression(parser, left);
        }
    } while(foundPostfix);

    return left;
}

fun SyntaxNode* ParseCastExpression(Parser* parser) {
    if (parser->tokenCur.kind != SyntaxKind::LeftParenToken || parser->tokenNext.kind != SyntaxKind::AsKeyword)
        return ParsePostFixExpression(parser);

    let SyntaxNode* result =  SyntaxNodeCreate(SyntaxKind::TypeCastExpression, parser->tree);
    result->typeCastExpr.leftParen = MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
    result->typeCastExpr.asKeyword = MatchAndAdvanceToken(parser, SyntaxKind::AsKeyword);
    result->typeCastExpr.targetTypeExpression = ParseTypeExpression(parser);
    result->typeCastExpr.rightParen = MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);
    result->typeCastExpr.expression = ParseUnaryExpression(parser, 0);
    return result;
}

fun SyntaxNode* ParseUnaryExpression(Parser* parser, int parentPrecedence) {
    let SyntaxNode* left = nullptr;
    let int unaryOperatorPrecedence = GetUnaryOperatorPrecedence(parser->tokenCur.kind);
    if ((unaryOperatorPrecedence != 0) && (unaryOperatorPrecedence >= parentPrecedence)) {
        let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::UnaryExpression, parser->tree);
        result->unaryExpr.operatorToken = AdvanceToken(parser);
        result->unaryExpr.operand = ParseBinaryExpression(parser, unaryOperatorPrecedence);
        left = result;
    } else {
      left = ParseCastExpression(parser);
    }
    return left;
}

fun SyntaxNode* ParseBinaryExpression(Parser* parser, int parentPrecedence) {
    let SyntaxNode* left = ParseUnaryExpression(parser, parentPrecedence);

    while (true) {
        let int precedence = GetBinaryOperatorPrecedence(parser->tokenCur.kind);
        if (precedence == 0
         || precedence < parentPrecedence 
         || precedence == parentPrecedence && !IsBinaryOperatorRightAssociative(parser->tokenCur.kind)) {
            break;
        }
        let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::BinaryExpression, parser->tree);
        result->binaryExpr.left = left;
        result->binaryExpr.operatorToken = AdvanceToken(parser);
        result->binaryExpr.right = ParseBinaryExpression(parser, precedence);
        left = result;
    }
    return left;
}

fun SyntaxNode* ParseTernaryConditionExpression(Parser* parser) {
    let SyntaxNode* left = ParseBinaryExpression(parser, 0);

    if (parser->tokenCur.kind == SyntaxKind::QuestionmarkToken) {
        let SyntaxNode* ternary = SyntaxNodeCreate(SyntaxKind::TernaryConditionalExpression, parser->tree);
        ternary->ternaryConditionalExpr.conditionExpression = left;
        ternary->ternaryConditionalExpr.questionmark = MatchAndAdvanceToken(parser, SyntaxKind::QuestionmarkToken);
        ternary->ternaryConditionalExpr.thenExpression = ParseTernaryConditionExpression(parser);
        ternary->ternaryConditionalExpr.colon = MatchAndAdvanceToken(parser, SyntaxKind::ColonToken);
        ternary->ternaryConditionalExpr.elseExpression = ParseTernaryConditionExpression(parser);
        return ternary;
    }
    return left;
}

fun SyntaxNode* ParseAssignmentExpression(Parser* parser) {
    let SyntaxNode* left = ParseTernaryConditionExpression(parser);

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
            assignment->binaryExpr.operatorToken = AdvanceToken(parser);
            assignment->binaryExpr.right = ParseAssignmentExpression(parser);
            return assignment;
        } 
    }
    return left;
}

fun SyntaxNode* ParseExpression(Parser* parser) {
    return ParseAssignmentExpression(parser);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Statements

fun SyntaxNode* ParseExpressionStatement(Parser* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::ExpressionStatement, parser->tree);
    result->expressionStmt.expression = ParseExpression(parser);
    result->expressionStmt.semicolon = MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
    return result;
}

fun SyntaxNode* ParseVariableDefinitionStatement(Parser* parser, bool skipLet, bool allowInitializer, SyntaxKind terminator) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::VariableDeclarationStatement, parser->tree);

    if (skipLet) {
        result->variableDeclarationStmt.letKeyword = SyntaxTokenCreateMissing();
    } else {
        if (parser->tokenCur.kind == SyntaxKind::LetLocalPersistKeyword) 
            result->variableDeclarationStmt.letKeyword = MatchAndAdvanceToken(parser, SyntaxKind::LetLocalPersistKeyword);
        else
            result->variableDeclarationStmt.letKeyword = MatchAndAdvanceToken(parser, SyntaxKind::LetKeyword);
    }

    result->variableDeclarationStmt.typeExpression = ParseTypeExpression(parser);
    result->variableDeclarationStmt.identifier = MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);

    if (parser->tokenCur.kind == SyntaxKind::LeftBracketToken) {
        // Array definition
        result->variableDeclarationStmt.leftBracket = MatchAndAdvanceToken(parser, SyntaxKind::LeftBracketToken);
        if (parser->tokenCur.kind == SyntaxKind::IntegerLiteralToken)
            result->variableDeclarationStmt.arraySizeLiteral = MatchAndAdvanceToken(parser, SyntaxKind::IntegerLiteralToken);
        result->variableDeclarationStmt.rightBracket = MatchAndAdvanceToken(parser, SyntaxKind::RightBracketToken);

        if (parser->tokenCur.kind == SyntaxKind::EqualsToken && allowInitializer) {
            result->variableDeclarationStmt.equalsToken = MatchAndAdvanceToken(parser, SyntaxKind::EqualsToken);
            result->variableDeclarationStmt.initializerExpression = ParseArrayLiteralExpression(parser);
        } else {
            result->variableDeclarationStmt.equalsToken = SyntaxTokenCreateMissing();
            result->variableDeclarationStmt.initializerExpression = nullptr;
        }
    } else {
        // Regular non-array variable
        result->variableDeclarationStmt.leftBracket = SyntaxTokenCreateMissing();
        result->variableDeclarationStmt.arraySizeLiteral = SyntaxTokenCreateMissing();
        result->variableDeclarationStmt.rightBracket = SyntaxTokenCreateMissing();

        if (parser->tokenCur.kind == SyntaxKind::EqualsToken && allowInitializer) {
            result->variableDeclarationStmt.equalsToken = MatchAndAdvanceToken(parser, SyntaxKind::EqualsToken);
            result->variableDeclarationStmt.initializerExpression = ParseExpression(parser);
        } else {
            result->variableDeclarationStmt.equalsToken = SyntaxTokenCreateMissing();
            result->variableDeclarationStmt.initializerExpression = nullptr;
        }
    }

    if (terminator == SyntaxKind::MissingToken) 
        result->variableDeclarationStmt.terminatorToken = SyntaxTokenCreateMissing();
    else
        result->variableDeclarationStmt.terminatorToken = MatchAndAdvanceToken(parser, terminator);

    return result;
}

fun SyntaxNode* ParseIfStatement(Parser* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::IfStatement, parser->tree);
    result->ifStmt.ifKeyword = MatchAndAdvanceToken(parser, SyntaxKind::IfKeyword);
    result->ifStmt.leftParen = MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
    result->ifStmt.condition = ParseExpression(parser);
    result->ifStmt.rightParen = MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);

    result->ifStmt.thenBlock = ParseStatement(parser);
    if (parser->tokenCur.kind == SyntaxKind::ElseKeyword) {
        result->ifStmt.elseKeyword = MatchAndAdvanceToken(parser, SyntaxKind::ElseKeyword);
        result->ifStmt.elseBlock = ParseStatement(parser);
    } else {
        result->ifStmt.elseBlock = nullptr;
        result->ifStmt.elseKeyword = SyntaxTokenCreateMissing();
    }
    return result;
}

fun SyntaxNode* ParseWhileStatement(Parser* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::WhileStatement, parser->tree);
    result->whileStmt.whileKeyword = MatchAndAdvanceToken(parser, SyntaxKind::WhileKeyword);
    result->whileStmt.leftParen = MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
    result->whileStmt.condition = ParseExpression(parser);
    result->whileStmt.rightParen = MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);

    parser->loopLevel += 1;
    result->whileStmt.body = ParseStatement(parser);
    parser->loopLevel -= 1;
    return result;
}

fun SyntaxNode* ParseDoWhileStatement(Parser* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::DoWhileStatement, parser->tree);
    result->doWhileStmt.doKeyword = MatchAndAdvanceToken(parser, SyntaxKind::DoKeyword);
    parser->loopLevel += 1;
    result->doWhileStmt.body = ParseStatement(parser);
    parser->loopLevel -= 1;

    result->doWhileStmt.whileKeyword = MatchAndAdvanceToken(parser, SyntaxKind::WhileKeyword);
    result->doWhileStmt.leftParen = MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
    result->doWhileStmt.condition = ParseExpression(parser);
    result->doWhileStmt.rightParen = MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);
    result->doWhileStmt.semicolon = MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
    return result;
}

fun SyntaxNode* ParseForStatement(Parser* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::ForStatement, parser->tree);
    result->forStmt.forKeyword = MatchAndAdvanceToken(parser, SyntaxKind::ForKeyword);

    result->forStmt.leftParen = MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
    if (parser->tokenCur.kind == SyntaxKind::LetKeyword)
        result->forStmt.initializerStatement = ParseVariableDefinitionStatement(parser, false, true, SyntaxKind::SemicolonToken);
    else 
        result->forStmt.initializerStatement = ParseExpressionStatement(parser);
    result->forStmt.conditionStatement = ParseExpressionStatement(parser);
    result->forStmt.incrementExpression = ParseExpression(parser);
    result->forStmt.rightParen = MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);

    parser->loopLevel += 1;
    result->forStmt.body = ParseStatement(parser);
    parser->loopLevel -= 1;
    return result;
}

fun SyntaxNode* ParseReturnStatement(Parser* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::ReturnStatement, parser->tree);
    result->returnStmt.returnKeyword = MatchAndAdvanceToken(parser, SyntaxKind::ReturnKeyword);
    if (parser->functionLevel == 0) {
        ReportError(
            result->returnStmt.returnKeyword.location,
            "Invalid 'return' keyword found outside of function definition"
        );
    }
    if (parser->tokenCur.kind != SyntaxKind::SemicolonToken)
        result->returnStmt.returnExpression = ParseExpression(parser);
    else
        result->returnStmt.returnExpression = nullptr;
    result->returnStmt.semicolon = MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
    return result;
}

fun SyntaxNode* ParseBreakStatement(Parser* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::BreakStatement, parser->tree);
    result->breakStmt.breakKeyword = MatchAndAdvanceToken(parser, SyntaxKind::BreakKeyword);
    if (parser->loopLevel == 0 && parser->switchCaseLevel == 0) {
        ReportError(
            result->breakStmt.breakKeyword.location,
            "Invalid 'break' keyword found outside of loop or switch-case definition"
        );
    }
    result->returnStmt.semicolon = MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
    return result;
}

fun SyntaxNode* ParseContinueStatement(Parser* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::ContinueStatement, parser->tree);
    result->continueStmt.continueKeyword = MatchAndAdvanceToken(parser, SyntaxKind::ContinueKeyword);
    if (parser->loopLevel == 0) {
        ReportError(
            result->continueStmt.continueKeyword.location,
            "Invalid 'continue' keyword found outside of loop definition"
        );
    }
    result->returnStmt.semicolon = MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
    return result;
}

fun SyntaxNode* ParseBlockStatement(Parser* parser, bool inSwitch) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::BlockStatement, parser->tree);
    if (parser->tokenCur.kind == SyntaxKind::LeftBraceToken || !inSwitch) {
        result->blockStmt.leftBrace = MatchAndAdvanceToken(parser, SyntaxKind::LeftBraceToken);
    } else {
        // We allow omitting the braces in switch statements
        result->blockStmt.leftBrace = SyntaxTokenCreateMissing();
    }

    result->blockStmt.statements = SyntaxNodeArrayCreate();
    while (parser->tokenCur.kind != SyntaxKind::RightBraceToken) {
        if (inSwitch && parser->tokenCur.kind == SyntaxKind::CaseKeyword)
            break;
        if (inSwitch && parser->tokenCur.kind == SyntaxKind::DefaultKeyword)
            break;
        let SyntaxNode* statement = ParseStatement(parser);
        SyntaxNodeArrayPush(&result->blockStmt.statements, statement);
    }

    if (result->blockStmt.leftBrace.kind != SyntaxKind::MissingToken ) {
        // If we start with a brace we also must end with a brace
        result->blockStmt.rightBrace = MatchAndAdvanceToken(parser, SyntaxKind::RightBraceToken);
    }
    return result;
}

fun SyntaxNode* ParseCaseStatement(Parser* parser, SyntaxNode* switchExpression) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::CaseStatement, parser->tree);
    result->caseStmt.caseKeyword = MatchAndAdvanceToken(parser, SyntaxKind::CaseKeyword);

    if (parser->switchCaseLevel == 0) {
        ReportError(
            result->caseStmt.caseKeyword.location, 
            "Unexpected 'case' label keyword outside of switch statement"
        );
    }
    result->caseStmt.literalExpression = ParseExpression(parser);
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
                result->caseStmt.caseKeyword.location, 
                "Expected literal token in case label but got '%s' instead",
                "TODO - we need to get the string of an expression"
                // TokenKindToString(result->caseStmt.literalExpression).cstr
            );
    }
    result->caseStmt.colon = MatchAndAdvanceToken(parser, SyntaxKind::ColonToken);
    result->caseStmt.body = ParseBlockStatement(parser, true);
    return result;
}

fun SyntaxNode* ParseDefaultStatement(Parser* parser, SyntaxNode* switchExpression) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::DefaultStatement, parser->tree);
    result->defaultStmt.defaultKeyword = MatchAndAdvanceToken(parser, SyntaxKind::DefaultKeyword);
    if (parser->switchCaseLevel == 0) {
        ReportError(
            result->defaultStmt.defaultKeyword.location, 
            "Unexpected 'default' case label keyword outside of switch statement"
        );
    }
    result->defaultStmt.colon = MatchAndAdvanceToken(parser, SyntaxKind::ColonToken);
    result->defaultStmt.body = ParseBlockStatement(parser, true);
    return result;
}

fun SyntaxNode* ParseSwitchStatement(Parser* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::SwitchStatement, parser->tree);
    result->switchStmt.switchKeyword = MatchAndAdvanceToken(parser, SyntaxKind::SwitchKeyword);
    result->switchStmt.leftParen = MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
    result->switchStmt.switchExpression = ParseExpression(parser);
    result->switchStmt.rightParen = MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);

    result->switchStmt.leftBrace = MatchAndAdvanceToken(parser, SyntaxKind::LeftBraceToken);
    parser->switchCaseLevel += 1;

    result->switchStmt.caseStatements = SyntaxNodeArrayCreate();
    while (parser->tokenCur.kind != SyntaxKind::EndOfFileToken) {
        if (parser->tokenCur.kind == SyntaxKind::RightBraceToken)
            break;
        if (parser->tokenCur.kind == SyntaxKind::CaseKeyword) {
            let SyntaxNode* caseStatement = ParseCaseStatement(parser, result);
            SyntaxNodeArrayPush(&result->switchStmt.caseStatements, caseStatement);
        } else {
            let SyntaxNode* defaultStatement = ParseDefaultStatement(parser, result);
            SyntaxNodeArrayPush(&result->switchStmt.caseStatements, defaultStatement);
        }
    }

    parser->switchCaseLevel -= 1;
    result->switchStmt.rightBrace = MatchAndAdvanceToken(parser, SyntaxKind::RightBraceToken);
    return result;
}

fun SyntaxNode* ParseStatement(Parser* parser) {
    assert(parser->functionLevel > 0);
    switch (parser->tokenCur.kind) {
        case SyntaxKind::LeftBraceToken:
            return ParseBlockStatement(parser, false);
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
        case SyntaxKind::LetKeyword:
        case SyntaxKind::LetLocalPersistKeyword:
            return ParseVariableDefinitionStatement(parser, false, true, SyntaxKind::SemicolonToken);
        default:
            return ParseExpressionStatement(parser);
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Module

fun SyntaxNode* ParseStructOrUnionDefinitionStatement(Parser* parser, SyntaxToken externKeyword) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::StructOrUnionDeclarationStatement, parser->tree);
    result->structOrUnionDeclarationStmt.externKeyword = externKeyword;
    if (parser->tokenCur.kind == SyntaxKind::StructKeyword)
        result->structOrUnionDeclarationStmt.structOrUnionKeyword = MatchAndAdvanceToken(parser, SyntaxKind::StructKeyword);
    else 
        result->structOrUnionDeclarationStmt.structOrUnionKeyword = MatchAndAdvanceToken(parser, SyntaxKind::UnionKeyword);
    result->structOrUnionDeclarationStmt.identifier = MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);

    if (parser->tokenCur.kind == SyntaxKind::SemicolonToken) {
        result->structOrUnionDeclarationStmt.semicolon = MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
        return result;
    } else {
        result->kind = SyntaxKind::StructOrUniontDefinitionStatement;
        result->structOrUnionDefinitionStmt.memberDeclarationStatements = SyntaxNodeArrayCreate();

        result->structOrUnionDefinitionStmt.leftBrace = MatchAndAdvanceToken(parser, SyntaxKind::LeftBraceToken);
        while (parser->tokenCur.kind != SyntaxKind::RightBraceToken) {
            let SyntaxNode* memberDeclaration = ParseVariableDefinitionStatement(parser, true, false, SyntaxKind::SemicolonToken);
            SyntaxNodeArrayPush(&result->structOrUnionDefinitionStmt.memberDeclarationStatements, memberDeclaration);
        }
        result->structOrUnionDefinitionStmt.rightBrace = MatchAndAdvanceToken(parser, SyntaxKind::RightBraceToken);
        result->structOrUnionDefinitionStmt.semicolon = MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
        return result;
    }
}

fun SyntaxNode* ParseEnumMemberClause(Parser* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::EnumMemberClauseSyntax, parser->tree);

    result->enumMember.identifier = MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
    if (parser->tokenCur.kind == SyntaxKind::EqualsToken) {
        result->enumMember.equals = MatchAndAdvanceToken(parser, SyntaxKind::EqualsToken);
        result->enumMember.integerLiteral = MatchAndAdvanceToken(parser, SyntaxKind::IntegerLiteralToken);
    } else {
        result->enumMember.equals = SyntaxTokenCreateMissing();
        result->enumMember.integerLiteral = SyntaxTokenCreateMissing();
    }

    if (parser->tokenCur.kind == SyntaxKind::CommaToken)
        result->enumMember.comma = MatchAndAdvanceToken(parser, SyntaxKind::CommaToken);
    else
        result->enumMember.comma = SyntaxTokenCreateMissing();

    return result;
}

fun SyntaxNode* ParseEnumDefinitionStatement(Parser* parser, SyntaxToken externKeyword) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::EnumDeclarationStatement, parser->tree);
    result->enumDeclarationStmt.externKeyword = externKeyword;
    result->enumDeclarationStmt.enumKeyword = MatchAndAdvanceToken(parser, SyntaxKind::EnumKeyword);
    result->enumDeclarationStmt.classKeyword = MatchAndAdvanceToken(parser, SyntaxKind::ClassKeyword);
    result->enumDeclarationStmt.identifier = MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);

    if (parser->tokenCur.kind == SyntaxKind::SemicolonToken) {
        // Just a forward declaration
        result->enumDeclarationStmt.semicolon = MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
        return result;
    } else {
        // Definition including body
        result->kind = SyntaxKind::EnumDefinitionStatement;
        result->enumDefinitionStmt.memberClauses = SyntaxNodeArrayCreate();

        result->enumDefinitionStmt.leftBrace = MatchAndAdvanceToken(parser, SyntaxKind::LeftBraceToken);
        while (parser->tokenCur.kind != SyntaxKind::RightBraceToken) {
            let SyntaxNode* memberClause = ParseEnumMemberClause(parser);
            SyntaxNodeArrayPush(&result->enumDefinitionStmt.memberClauses, memberClause);
            if (memberClause->enumMember.comma.kind != SyntaxKind::CommaToken)
                break;
        }
        result->enumDefinitionStmt.rightBrace = MatchAndAdvanceToken(parser, SyntaxKind::RightBraceToken);
        result->enumDefinitionStmt.semicolon = MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
        return result;
    }
}

fun SyntaxNode* ParseFunctionDefinitionStatement(Parser* parser, SyntaxToken externKeyword) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::FunctionDeclarationStatement, parser->tree);
    result->functionDeclarationStmt.externKeyword = externKeyword;
    result->functionDeclarationStmt.funKeyword = MatchAndAdvanceToken(parser, SyntaxKind::FunKeyword);
    result->functionDeclarationStmt.returnType = ParseTypeExpression(parser);
    result->functionDeclarationStmt.identifier = MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);

    result->functionDeclarationStmt.leftParen = MatchAndAdvanceToken(parser, SyntaxKind::LeftParenToken);
    result->functionDeclarationStmt.params = SyntaxNodeArrayCreate();
    while (parser->tokenCur.kind != SyntaxKind::RightParenToken) {
        if (parser->tokenCur.kind == SyntaxKind::DotDotDotToken) {
            let SyntaxToken dotdot = MatchAndAdvanceToken(parser, SyntaxKind::DotDotDotToken);
            let SyntaxNode* dotdotWrapper = WrapTokenInNode(parser, dotdot);
            SyntaxNodeArrayPush(&result->functionDeclarationStmt.params, dotdotWrapper);
            break;
        }

        let SyntaxNode* parameter = ParseVariableDefinitionStatement(parser, true, false, SyntaxKind::MissingToken);
        SyntaxNodeArrayPush(&result->functionDeclarationStmt.params, parameter);

        if (parser->tokenCur.kind == SyntaxKind::CommaToken) {
            parameter->variableDeclarationStmt.terminatorToken = MatchAndAdvanceToken(parser, SyntaxKind::CommaToken);
        } else {
            break;
        }
    }
    result->functionDeclarationStmt.rightParen = MatchAndAdvanceToken(parser, SyntaxKind::RightParenToken);

    if (parser->tokenCur.kind == SyntaxKind::SemicolonToken) {
        // Just a forward declaration
        result->functionDeclarationStmt.semicolon = MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
        return result;
    } else {
        // Definition including body
        result->kind = SyntaxKind::FunctionDefinitionStatement;
        parser->functionLevel += 1;
        result->functionDefinitionStmt.body = ParseBlockStatement(parser, false);
        parser->functionLevel -= 1;
        return result;
    }
}

fun SyntaxNode* ParseGlobalVariableDefinitionStatement(Parser* parser, SyntaxToken externKeyword) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::GlobalVariableDeclarationStatement, parser->tree);
    result->globalVariableStmt.externKeyword = externKeyword;
    result->globalVariableStmt.variableDeclarationStatement = ParseVariableDefinitionStatement(parser, false, true, SyntaxKind::SemicolonToken);
    return result;
}

fun SyntaxNode* ParseGlobalDefinitionStatement(Parser* parser) {
    assert(parser->functionLevel == 0);

    let SyntaxToken externKeyword = SyntaxTokenCreateMissing();
    if (parser->tokenCur.kind == SyntaxKind::ExternKeyword) 
        externKeyword = AdvanceToken(parser);

    switch (parser->tokenCur.kind) {
        case SyntaxKind::EnumKeyword:
            return ParseEnumDefinitionStatement(parser, externKeyword);
        case SyntaxKind::StructKeyword: 
        case SyntaxKind::UnionKeyword:
            return ParseStructOrUnionDefinitionStatement(parser, externKeyword);
        case SyntaxKind::FunKeyword:
            return ParseFunctionDefinitionStatement(parser, externKeyword);
        default:
            return ParseGlobalVariableDefinitionStatement(parser, externKeyword);
    }
}

fun SyntaxNode* ParseImportDeclarationStatement(Parser* parser) {
    let SyntaxNode* result = SyntaxNodeCreate(SyntaxKind::ImportDeclarationStatement, parser->tree);
    result->importStmt.importKeyword = MatchAndAdvanceToken(parser, SyntaxKind::ImportKeyword);
    result->importStmt.modulenameLiteral = MatchAndAdvanceToken(parser, SyntaxKind::StringLiteralToken);
    return result;
}

fun SyntaxNode* ParseModuleStatement(Parser* parser) {
    switch (parser->tokenCur.kind) {
        case SyntaxKind::ExternKeyword:
        case SyntaxKind::StructKeyword:
        case SyntaxKind::UnionKeyword:
        case SyntaxKind::EnumKeyword:
        case SyntaxKind::FunKeyword:
        case SyntaxKind::LetKeyword:
            return ParseGlobalDefinitionStatement(parser);
        case SyntaxKind::ImportKeyword:
            return ParseImportDeclarationStatement(parser);
        default:
            ReportError(
                parser->tokenCur.location,
                "Expected global module definition got unexpected token '%s' instead",
                TokenGetText(parser->tokenCur).cstr
            );
            exit(1);
    }
}

fun SyntaxTree* ParseModule(Parser* parser) {
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
                MatchAndAdvanceToken(parser, SyntaxKind::TypedefKeyword);
                while (parser->tokenCur.kind != SyntaxKind::SemicolonToken) {
                    AdvanceToken(parser); // Ignore everything in the typedef
                }
                MatchAndAdvanceToken(parser, SyntaxKind::SemicolonToken);
                foundDirectives = true;
            }
            if (parser->tokenCur.kind == SyntaxKind::IncludeDirectiveKeyword) {
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
            if (parser->tokenCur.kind == SyntaxKind::PragmaDirectiveKeyword) {
                MatchAndAdvanceToken(parser, SyntaxKind::PragmaDirectiveKeyword);
                MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
                foundDirectives = true;
            }
            if (parser->tokenCur.kind == SyntaxKind::IfDirectiveKeyword) {
                MatchAndAdvanceToken(parser, SyntaxKind::IfDirectiveKeyword);
                AdvanceToken(parser); // Ignore 0
                foundDirectives = true;
            }
            if (parser->tokenCur.kind == SyntaxKind::EndIfDefinedDirectiveKeyword) {
                MatchAndAdvanceToken(parser, SyntaxKind::EndIfDefinedDirectiveKeyword);
                foundDirectives = true;
            }
            if (parser->tokenCur.kind == SyntaxKind::DefineDirectiveKeyword) {
                MatchAndAdvanceToken(parser, SyntaxKind::DefineDirectiveKeyword);
                if (parser->tokenCur.kind == SyntaxKind::LetKeyword) {
                    MatchAndAdvanceToken(parser, SyntaxKind::LetKeyword);
                } else if (parser->tokenCur.kind == SyntaxKind::AsKeyword) {
                    MatchAndAdvanceToken(parser, SyntaxKind::AsKeyword);
                } else if (parser->tokenCur.kind == SyntaxKind::ByteKeyword) {
                    MatchAndAdvanceToken(parser, SyntaxKind::ByteKeyword);
                    MatchAndAdvanceToken(parser, SyntaxKind::CharKeyword);
                } else if (parser->tokenCur.kind == SyntaxKind::LetLocalPersistKeyword) {
                    MatchAndAdvanceToken(parser, SyntaxKind::LetLocalPersistKeyword);
                    MatchAndAdvanceToken(parser, SyntaxKind::IdentifierToken);
                } else {
                    MatchAndAdvanceToken(parser, SyntaxKind::FunKeyword);
                }
                foundDirectives = true;
            }
        } while (foundDirectives);
        if (parser->tokenCur.kind == SyntaxKind::EndOfFileToken)
            break;
        let SyntaxNode* statement = ParseModuleStatement(parser);
        SyntaxNodeArrayPush(&result->moduleStmt.globalStatements, statement);
    }

    if (parser->tokenCur.kind != SyntaxKind::EndOfFileToken)
    {
        ReportError(
            parser->tokenCur.location,
            "Expected EOF token after parsing file, instead got '%s'", 
            TokenKindToString(parser->tokenCur.kind).cstr
        );
    }
    // TODO: figure out and assign all parents to all nodes in the tree here

    return parser->tree;
}