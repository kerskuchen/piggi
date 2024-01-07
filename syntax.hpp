#pragma once

#include "definitions.hpp"

enum class SyntaxKind {
    BadToken,

    // ---------------------------------------------------------------------------------------------
    // Trivia

    SkippedTextTrivia,
    LineBreakTrivia,
    WhitespaceTrivia,
    SingleLineCommentTrivia,
    MultiLineCommentTrivia,

    // ---------------------------------------------------------------------------------------------
    // Tokens

    EndOfFileToken,

    /////// Unary operators

    BangToken,
    TildeToken,

    /////// Binary operators

    PlusToken,
    MinusToken,
    StarToken,
    SlashToken,
    PercentToken,

    // Assignments
    EqualsToken,
    PlusEqualsToken,
    MinusEqualsToken,
    StarEqualsToken,
    SlashEqualsToken,
    PercentEqualsToken,
    LessLessEqualsToken,
    GreaterGreaterEqualsToken,
    HatEqualsToken,
    PipeEqualsToken,
    AmpersandEqualsToken,

    // Logical
    PipePipeToken,
    AmpersandAmpersandToken,

    // Comparisons
    EqualsEqualsToken,
    BangEqualsToken,
    LessToken,
    LessEqualsToken,
    GreaterToken,
    GreaterEqualsToken,

    // Bits
    LessLessToken,
    GreaterGreaterToken,
    HatToken,
    PipeToken,
    AmpersandToken,

    /////// Punctuation
    QuestionmarkToken,
    ColonToken,
    ColonColonToken,
    ArrowToken,
    DotToken,
    DotDotDotToken,
    CommaToken,
    SemicolonToken,
    LeftBraceToken,
    RightBraceToken,
    LeftParenToken,
    RightParenToken,
    LeftBracketToken,
    RightBracketToken,

    /////// Literals
    IntegerLiteralToken,
    CharacterLiteralToken,
    StringLiteralToken,

    /////// Keywords and identifiers

    IdentifierToken,

    // Primitive types
    VoidKeyword,
    CharKeyword,
    ByteKeyword,
    ShortKeyword,
    IntKeyword,
    LongKeyword,
    NullKeyword,
    CStringKeyword,
    BoolKeyword,
    TrueKeyword,
    FalseKeyword,

    // Declarations
    FunKeyword,
    LetKeyword,
    StructKeyword,
    UnionKeyword,
    EnumKeyword,
    ClassKeyword,

    // Control flow
    IfKeyword,
    ElseKeyword,
    DoKeyword,
    WhileKeyword,
    ForKeyword,
    ReturnKeyword,
    BreakKeyword,
    ContinueKeyword,
    SwitchKeyword,
    CaseKeyword,
    DefaultKeyword,

    // Misc
    AsKeyword,
    SizeOfKeyword,

    // Storage location
    LocalPersistKeyword,
    ExternKeyword,

    // TODO get rid of these 
    IncludeDirectiveKeyword,
    DefineDirectiveKeyword,
    IfDirectiveKeyword,
    EndIfDefinedDirectiveKeyword,
    PragmaDirectiveKeyword,
    TypedefKeyword,

    // ---------------------------------------------------------------------------------------------
    // 

    // Expressions
    FunccallExpression,
    ArrayIndexExpression,
    MemberAccessExpression,
    TypeCastExpression,
    ParenthesizedExpression,
    TernaryConditionalExpression,
    AssignmentExpression,
    SizeOfExpression,
    TypeExpression, // TODO: currently used for sizeof expression. lets see if we need this in the long run

    // Literals
    NullLiteralExpression,
    IntegerLiteralExpression,
    CharacterLiteralExpression,
    BoolLiteralExpression,
    StringLiteralExpression,
    EnumValueLiteralExpression,
    ArrayLiteralExpression,

    // Misc
    NameExpression,

    // Statements
    BlockStatement,
    ExpressionStatement,

    IfStatement,
    DoWhileStatement,
    WhileStatement,
    ForStatement,
    ReturnStatement,
    BreakStatement,
    ContinueStatement,
    SwitchStatement,
    CaseStatement,
    DefaultStatement,

    VariableDeclarationStatement,
    ArrayDeclarationStatement,

    // Module
    Module,
    ImportDeclaration,
    GlobalVariableDeclaration,
    EnumDeclarationStatement,
    EnumDefinitionStatement,
    StructDeclarationStatement,
    StructDefinitionStatement,
    UnionDeclarationStatement,
    UnionDefinitionStatement,
    FunctionDeclarationStatement,
    FunctionDefinitionStatement,
};

union SyntaxNode;
struct SyntaxInfo {
    SyntaxKind kind;
    SyntaxNode* parent;
};

struct SyntaxNodeArray {
    SyntaxNode** nodes;
    int count;
    int capacity;
};

fun SyntaxNodeArray SyntaxNodeArrayCreate() {
    let SyntaxNodeArray result;
    result.nodes = nullptr;
    result.count = 0;
    result.capacity = 0;
    return result;
}

fun int SyntaxNodeArrayPush(SyntaxNodeArray* array, SyntaxNode* node) {
    if (array->count == array->capacity) {
        let int newCapacity = 2 * array->capacity;
        if (newCapacity == 0)
            newCapacity = 64;
        array->capacity = newCapacity;
        array->nodes = (as SyntaxNode**) realloc(array->nodes, newCapacity * sizeof(SyntaxNode*));
        assert (array->nodes != nullptr);
    }
    let int insertionIndex = array->count;
    array->nodes[insertionIndex] = node;
    array->count += 1;
    return insertionIndex;
}

// -------------------------------------------------------------------------------------------------
// Trivia 

struct SyntaxTrivia {
    SyntaxKind kind;
    SourceLocation location;
};

struct SyntaxTriviaArray {
    SyntaxTrivia* nodes;
    int count;
    int capacity;
};

fun SyntaxTriviaArray SyntaxTriviaArrayCreate() {
    let SyntaxTriviaArray result;
    result.nodes = nullptr;
    result.count = 0;
    result.capacity = 0;
    return result;
}

fun int SyntaxTriviaArrayPush(SyntaxTriviaArray* array, SyntaxTrivia node) {
    if (array->count == array->capacity) {
        let int newCapacity = 2 * array->capacity;
        if (newCapacity == 0)
            newCapacity = 64;
        array->capacity = newCapacity;
        array->nodes = (as SyntaxTrivia*) realloc(array->nodes, newCapacity * sizeof(SyntaxTrivia));
        assert (array->nodes != nullptr);
    }
    let int insertionIndex = array->count;
    array->nodes[insertionIndex] = node;
    array->count += 1;
    return insertionIndex;
}

// -------------------------------------------------------------------------------------------------
// Tokens 

struct SyntaxToken
{
    SyntaxInfo info;
    SourceLocation location;

    SyntaxTriviaArray leadingTrivia;
    SyntaxTriviaArray trailingTrivia;

    longint intvalue;   // for integer literals
    bool intvalueIsHex; // makes the emit more readable
    String stringValue; // for char and string literals

    String debugString;
};

// -------------------------------------------------------------------------------------------------
// Expressions 

struct UnaryExpressionSyntax {
    SyntaxInfo info;

    SyntaxToken operatorToken;
    SyntaxNode* operand;
};

struct BinaryExpressionSyntax {
    SyntaxInfo info;

    SyntaxNode* left;
    SyntaxToken operatorToken;
    SyntaxNode* right;
};

struct ParenthesizedExpressionSyntax {
    SyntaxInfo info;

    SyntaxToken leftParen;
    SyntaxNode* expression;
    SyntaxToken rightParen;
};

struct PrimitiveLiteralExpressionSyntax {
    SyntaxInfo info;

    SyntaxToken literalToken;
};

struct ArrayLiteralExpressionSyntax {
    SyntaxInfo info;

    SyntaxToken leftBrace;
    // TODO
    SyntaxToken rightBrace;
};


// -------------------------------------------------------------------------------------------------
// Statements 

union SyntaxNode {
    SyntaxInfo info;
    SyntaxToken token;
    UnaryExpressionSyntax unaryExpression;
    BinaryExpressionSyntax binaryExpression;
    ParenthesizedExpressionSyntax parenthesizedExpression;
    PrimitiveLiteralExpressionSyntax primitiveLiteralExpression;
    ArrayLiteralExpressionSyntax arrayLiteralExpression;
};

fun SyntaxNode* SyntaxNodeCreate(SyntaxKind kind, SyntaxNode* parent) {
    let SyntaxNode* node = (as SyntaxNode*) malloc(sizeof(SyntaxNode));
    assert(node != nullptr);
    node->info.kind = kind;
    node->info.parent = parent;
    return node;
}
