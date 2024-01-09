#pragma once

#include "prelude.hpp"
#if 0
import "prelude.hpp"
#endif

////////////////////////////////////////////////////////////////////////////////////////////////////
// Common stuff to keep our sanity

struct String {
    cstring cstr; // Always zero terminated
    int length;       // string length NOT buffer length
};

fun String StringCreateEmpty() {
    let String result;
    result.cstr = nullptr;
    result.length = 0;
    return result;
}

fun String StringCreateFromCStr(cstring str) {
    let int length = 0;
    if (str != nullptr) {
        let cstring pos = str;
        while (*pos != '\0') {
            length += 1;
            pos += 1;
        }
    }
    let String result;
    result.cstr = str;
    result.length = length;
    return result;
}

fun bool StringEquals(String a, String b) {
    if (a.length != b.length)
        return false;

    for (let int index = 0; index < a.length; index += 1) {
        if (a.cstr[index] != b.cstr[index])
            return false;
    }
    return true;
}

struct LineAndColumnNumber {
    int lineNumber;
    int columnNumber;
};

fun LineAndColumnNumber StringGetLineAndColumnNumberAtPos(String str, int pos) {
    let int curPos = 0;
    let int curLine = 1;
    let int curColumn = 1;
    while (curPos < pos && curPos < str.length) {
        if (str.cstr[curPos] == '\n') {
            curColumn = 1;
            curLine += 1;
        }
        curPos +=1;
    }
    let LineAndColumnNumber result;
    result.lineNumber = curLine;
    result.columnNumber = curColumn;
    return result;
}

fun String StringGetSubstring(String str, int start, int end) {
    assert(start <= end);
    let int length = end - start;
    if (length == 0)
        return StringCreateEmpty();
    
    let char* buffer = (as char*) malloc(length + 1);
    for (let int index = 0; index < length; index += 1) {
        buffer[index] = str.cstr[start + index];
    }
    buffer[length] = '\0';

    let String result;
    result.cstr = buffer;
    result.length = length;
    return result;
}

fun String StringAppend(String base, String suffix) {
    let int length = base.length + suffix.length;
    let char* buffer = (as char*) malloc(length + 1);

    let int pos = 0;
    for (let int sourceIndex = 0; sourceIndex < base.length; sourceIndex += 1) {
        buffer[pos] = base.cstr[sourceIndex];
        pos +=1;
    }
    for (let int sourceIndex = 0; sourceIndex < suffix.length; sourceIndex += 1) {
        buffer[pos] = suffix.cstr[sourceIndex];
        pos +=1;
    }
    buffer[pos] = '\0';

    let String result;
    result.length = length;
    result.cstr = buffer;
    return result;
}

fun int FindCharPosInString(String str, char ch) {
    for (let int pos = 0; pos < str.length; pos += 1) {
        if (str.cstr[pos] == ch)
            return pos;
    }
    return -1;
}

fun bool StringStartsWith(String str, String prefix) {
    if (str.length < prefix.length)
        return false;

    for (let int pos = 0; pos < prefix.length; pos += 1) {
        if (str.cstr[pos] != prefix.cstr[pos])
            return false;
    }
    return true;
}

fun bool StringEndsWith(String str, String suffix) {
    if (str.length < suffix.length)
        return false;

    for (let int pos = 0; pos < suffix.length; pos += 1) {
        let int strIndex = str.length - suffix.length + pos;
        let int suffixIndex = pos;
        if (str.cstr[strIndex] != suffix.cstr[suffixIndex])
            return false;
    }
    return true;
}


struct StringArray {
    String* strings;
    int count;
    int capacity;
};

fun StringArray StringArrayCreate() {
    let StringArray result;
    result.strings = nullptr;
    result.count = 0;
    result.capacity = 0;
    return result;
}

fun void StringArrayGrow(StringArray* array, int newCapacity) {
    array->capacity = newCapacity;
    array->strings = (as String*) realloc(array->strings, newCapacity * sizeof(String));
    assert(array->strings != nullptr);
}

fun int StringArrayPush(StringArray* array, String string) {
    if (array->count == array->capacity) {
        let int newCapacity = 2 * array->capacity;
        if (newCapacity == 0)
            newCapacity = 64;
        StringArrayGrow(array, newCapacity);
    }
    let int insertionIndex = array->count;
    array->strings[insertionIndex] = string;
    array->count += 1;
    return insertionIndex;
}

fun bool StringArrayContains(StringArray* array, String value) {
    for (let int index = 0; index < array->count; index += 1) {
        if (StringEquals(array->strings[index], value))
            return true;
    }

    return false;
}

fun String StringArrayPop(StringArray* array) {
    if (array->count > 0) {
        array->count -= 1;
        return array->strings[array->count];
    }
    assert(array->count > 0);
    exit(1);
}


////////////////////////////////////////////////////////////////////////////////////////////////////
// Source 

struct Source {
    String modulename;
    String filepath;
    String content;
};

fun String SourceGetSubstring(Source source, int start, int end) {
    return StringGetSubstring(source.content, start, end);
}

fun char SourceGetCharAtIndex(Source source, int charPos) {
    return source.content.cstr[charPos];
}

struct SourceLocation {
    Source source;
    int start;
    int end;
};

fun SourceLocation SourceLocationCreate(Source source, int start, int end) {
    let SourceLocation result;
    result.source = source;
    result.start = start;
    result.end = end;
    return result;
}

fun LineAndColumnNumber LocationGetLineColumn(SourceLocation location) {
    return StringGetLineAndColumnNumberAtPos(location.source.content, location.start);
}

struct SourceArray {
    Source* files;
    int count;
    int capacity;
};

fun SourceArray SourceArrayCreate() {
    let SourceArray result;
    result.files = nullptr;
    result.count = 0;
    result.capacity = 0;
    return result;
}

fun void SourceArrayGrow(SourceArray* array, int newCapacity) {
    array->capacity = newCapacity;
    array->files = (as Source*) realloc(array->files, newCapacity * sizeof(Source));
    assert(array->files != nullptr);
}

fun int SourceArrayPush(SourceArray* array, Source file) {
    if (array->count == array->capacity) {
        let int newCapacity = 2 * array->capacity;
        if (newCapacity == 0)
            newCapacity = 64;
        SourceArrayGrow(array, newCapacity);
    }
    let int insertionIndex = array->count;
    array->files[insertionIndex] = file;
    array->count += 1;
    return insertionIndex;
}


////////////////////////////////////////////////////////////////////////////////////////////////////
// Errors 

fun void ReportLocation(SourceLocation location) {
    let LineAndColumnNumber fileLineColumn = LocationGetLineColumn(location);
    fprintf(stderr, "%s(%d)", location.source.filepath.cstr, fileLineColumn.lineNumber);
}

fun void ReportError(SourceLocation location, char* format, ...) {
    ReportLocation(location);
    fprintf(stderr, ": Error - ");
    let va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fprintf(stderr, "\n");
    exit(1);
}


////////////////////////////////////////////////////////////////////////////////////////////////////
// Syntax 

enum class SyntaxKind {
    MissingToken,
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
    LetKeyword,
    LetLocalPersistKeyword,
    FunKeyword,
    StructKeyword,
    UnionKeyword,
    EnumKeyword,
    ClassKeyword,
    ImportKeyword,

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
    UnaryExpression,
    BinaryExpression,
    FuncCallExpression,
    ArrayIndexExpression,
    MemberAccessExpression,
    TypeCastExpression,
    ParenthesizedExpression,
    TernaryConditionalExpression,
    SizeOfExpression,
    NameExpression,
    TypeExpression,

    // Literals
    NullLiteralExpression,
    IntegerLiteralExpression,
    CharacterLiteralExpression,
    BoolLiteralExpression,
    StringLiteralExpression,
    EnumValueLiteralExpression,
    ArrayLiteralExpression,

    // Misc
    EnumMemberClauseSyntax,

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

    // Module
    Module,
    ImportDeclarationStatement,
    GlobalVariableDeclarationStatement,
    EnumDeclarationStatement,
    EnumDefinitionStatement,
    StructOrUnionDeclarationStatement,
    StructOrUniontDefinitionStatement,
    FunctionDeclarationStatement,
    FunctionDefinitionStatement,
};

union SyntaxNode;
struct ModuleStatementSyntax;

struct SyntaxTree {
    Source source;
    ModuleStatementSyntax* moduleRoot;
};

struct SyntaxTreeArray {
    SyntaxTree** trees;
    int count;
    int capacity;
};

fun SyntaxTreeArray SyntaxTreeArrayCreate() {
    let SyntaxTreeArray result;
    result.trees = nullptr;
    result.count = 0;
    result.capacity = 0;
    return result;
}

fun void SyntaxTreeArrayGrow(SyntaxTreeArray* array, int newCapacity) {
    array->capacity = newCapacity;
    array->trees = (as SyntaxTree**) realloc(array->trees, newCapacity * sizeof(SyntaxTree*));
    assert(array->trees != nullptr);
}

fun int SyntaxTreeArrayPush(SyntaxTreeArray* array, SyntaxTree* tree) {
    if (array->count == array->capacity) {
        let int newCapacity = 2 * array->capacity;
        if (newCapacity == 0)
            newCapacity = 64;
        SyntaxTreeArrayGrow(array, newCapacity);
    }
    let int insertionIndex = array->count;
    array->trees[insertionIndex] = tree;
    array->count += 1;
    return insertionIndex;
}

struct SyntaxInfo {
    SyntaxKind kind;
    SyntaxTree* tree;
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

fun SyntaxTrivia SyntaxTriviaCreate(SyntaxKind kind, SourceLocation location) {
    let SyntaxTrivia result;
    result.kind = kind;
    result.location = location;
    return result;
}

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
    SyntaxKind kind;
    SyntaxTree* tree;
    SourceLocation location;

    SyntaxTriviaArray leadingTrivia;
    SyntaxTriviaArray trailingTrivia;

    longint intvalue;   // for integer literals
    bool intvalueIsHex; // makes the emit more readable
    String stringValueWithoutQuotes; // for char and string literals

    String debugString;
};

fun SyntaxToken SyntaxTokenCreateMissing() {
    let SyntaxToken result;
    result.kind = SyntaxKind::MissingToken;
    result.tree = nullptr;

    result.location.source.content = StringCreateEmpty();
    result.location.source.filepath = StringCreateEmpty();
    result.location.source.modulename = StringCreateEmpty();
    result.location.start = 0;
    result.location.end = 0;

    result.leadingTrivia = SyntaxTriviaArrayCreate();
    result.trailingTrivia = SyntaxTriviaArrayCreate();

    result.intvalue = 0;
    result.intvalueIsHex = false;
    result.stringValueWithoutQuotes = StringCreateEmpty();

    result.debugString = StringCreateEmpty();
    return result;
}

fun SyntaxToken SyntaxTokenCreateEmpty(SyntaxTree* tree) {
    let SyntaxToken result;
    result.kind = SyntaxKind::BadToken;
    result.tree = tree;

    result.location.source = tree->source;
    result.location.start = 0;
    result.location.end = 0;

    result.leadingTrivia = SyntaxTriviaArrayCreate();
    result.trailingTrivia = SyntaxTriviaArrayCreate();

    result.intvalue = 0;
    result.intvalueIsHex = false;
    result.stringValueWithoutQuotes = StringCreateEmpty();

    result.debugString = StringCreateEmpty();
    return result;
}

fun String TokenKindToString(SyntaxKind kind) {
    switch (kind) {
        case SyntaxKind::EndOfFileToken:
            return StringCreateFromCStr("eof");

        case SyntaxKind::PlusToken:
            return StringCreateFromCStr("+");
        case SyntaxKind::PlusEqualsToken:
            return StringCreateFromCStr("+=");
        case SyntaxKind::MinusToken:
            return StringCreateFromCStr("-");
        case SyntaxKind::MinusEqualsToken:
            return StringCreateFromCStr("-=");
        case SyntaxKind::StarToken:
            return StringCreateFromCStr("*");
        case SyntaxKind::StarEqualsToken:
            return StringCreateFromCStr("*=");
        case SyntaxKind::SlashToken:
            return StringCreateFromCStr("/");
        case SyntaxKind::SlashEqualsToken:
            return StringCreateFromCStr("/=");
        case SyntaxKind::PercentToken:
            return StringCreateFromCStr("%");
        case SyntaxKind::PercentEqualsToken:
            return StringCreateFromCStr("%=");

        case SyntaxKind::EqualsToken:
            return StringCreateFromCStr("=");
        case SyntaxKind::EqualsEqualsToken:
            return StringCreateFromCStr("==");
        case SyntaxKind::BangToken:
            return StringCreateFromCStr("!");
        case SyntaxKind::BangEqualsToken:
            return StringCreateFromCStr("!=");
        case SyntaxKind::LessToken:
            return StringCreateFromCStr("<");
        case SyntaxKind::LessEqualsToken:
            return StringCreateFromCStr("<=");
        case SyntaxKind::GreaterToken:
            return StringCreateFromCStr(">");
        case SyntaxKind::GreaterEqualsToken:
            return StringCreateFromCStr(">=");

        case SyntaxKind::LessLessToken:
            return StringCreateFromCStr("<<");
        case SyntaxKind::LessLessEqualsToken:
            return StringCreateFromCStr("<<=");
        case SyntaxKind::GreaterGreaterToken:
            return StringCreateFromCStr(">>");
        case SyntaxKind::GreaterGreaterEqualsToken:
            return StringCreateFromCStr(">>=");
        case SyntaxKind::TildeToken:
            return StringCreateFromCStr("~");
        case SyntaxKind::HatToken:
            return StringCreateFromCStr("^");
        case SyntaxKind::HatEqualsToken:
            return StringCreateFromCStr("^=");
        case SyntaxKind::PipeToken:
            return StringCreateFromCStr("|");
        case SyntaxKind::PipeEqualsToken:
            return StringCreateFromCStr("|=");
        case SyntaxKind::PipePipeToken:
            return StringCreateFromCStr("||");
        case SyntaxKind::AmpersandToken:
            return StringCreateFromCStr("&");
        case SyntaxKind::AmpersandEqualsToken:
            return StringCreateFromCStr("&=");
        case SyntaxKind::AmpersandAmpersandToken:
            return StringCreateFromCStr("&&");

        case SyntaxKind::QuestionmarkToken:
            return StringCreateFromCStr("?");
        case SyntaxKind::ColonToken:
            return StringCreateFromCStr(":");
        case SyntaxKind::ColonColonToken:
            return StringCreateFromCStr("::");
        case SyntaxKind::ArrowToken:
            return StringCreateFromCStr("->");
        case SyntaxKind::DotToken:
            return StringCreateFromCStr(".");
        case SyntaxKind::DotDotDotToken:
            return StringCreateFromCStr("...");
        case SyntaxKind::CommaToken:
            return StringCreateFromCStr(",");
        case SyntaxKind::SemicolonToken:
            return StringCreateFromCStr(";");
        case SyntaxKind::LeftBraceToken:
            return StringCreateFromCStr("{");
        case SyntaxKind::RightBraceToken:
            return StringCreateFromCStr("}");
        case SyntaxKind::LeftParenToken:
            return StringCreateFromCStr("(");
        case SyntaxKind::RightParenToken:
            return StringCreateFromCStr(")");
        case SyntaxKind::LeftBracketToken:
            return StringCreateFromCStr("[");
        case SyntaxKind::RightBracketToken:
            return StringCreateFromCStr("]");

        case SyntaxKind::IntegerLiteralToken:
            return StringCreateFromCStr("int-lit");
        case SyntaxKind::CharacterLiteralToken:
            return StringCreateFromCStr("chr-lit");
        case SyntaxKind::StringLiteralToken:
            return StringCreateFromCStr("str-lit");

        case SyntaxKind::CharKeyword:
            return StringCreateFromCStr("char");
        case SyntaxKind::ByteKeyword:
            return StringCreateFromCStr("byte");
        case SyntaxKind::ShortKeyword:
            return StringCreateFromCStr("short");
        case SyntaxKind::IntKeyword:
            return StringCreateFromCStr("int");
        case SyntaxKind::LongKeyword:
            return StringCreateFromCStr("longint");
        case SyntaxKind::VoidKeyword:
            return StringCreateFromCStr("void");
        case SyntaxKind::NullKeyword:
            return StringCreateFromCStr("nullptr");
        case SyntaxKind::CStringKeyword:
            return StringCreateFromCStr("cstring");
        case SyntaxKind::BoolKeyword:
            return StringCreateFromCStr("bool");
        case SyntaxKind::TrueKeyword:
            return StringCreateFromCStr("true");
        case SyntaxKind::FalseKeyword:
            return StringCreateFromCStr("false");

        case SyntaxKind::IfKeyword:
            return StringCreateFromCStr("if");
        case SyntaxKind::ElseKeyword:
            return StringCreateFromCStr("else");
        case SyntaxKind::DoKeyword:
            return StringCreateFromCStr("do");
        case SyntaxKind::WhileKeyword:
            return StringCreateFromCStr("while");
        case SyntaxKind::ForKeyword:
            return StringCreateFromCStr("for");
        case SyntaxKind::ReturnKeyword:
            return StringCreateFromCStr("return");
        case SyntaxKind::BreakKeyword:
            return StringCreateFromCStr("break");
        case SyntaxKind::ContinueKeyword:
            return StringCreateFromCStr("continue");
        case SyntaxKind::SwitchKeyword:
            return StringCreateFromCStr("switch");
        case SyntaxKind::CaseKeyword:
            return StringCreateFromCStr("case");
        case SyntaxKind::DefaultKeyword:
            return StringCreateFromCStr("default");
        case SyntaxKind::AsKeyword:
            return StringCreateFromCStr("as");
        case SyntaxKind::SizeOfKeyword:
            return StringCreateFromCStr("sizeof");

        case SyntaxKind::LetKeyword:
            return StringCreateFromCStr("let");
        case SyntaxKind::LetLocalPersistKeyword:
            return StringCreateFromCStr("letpersist");
        case SyntaxKind::FunKeyword:
            return StringCreateFromCStr("fun");
        case SyntaxKind::StructKeyword:
            return StringCreateFromCStr("struct");
        case SyntaxKind::UnionKeyword:
            return StringCreateFromCStr("union");
        case SyntaxKind::EnumKeyword:
            return StringCreateFromCStr("enum");
        case SyntaxKind::ClassKeyword:
            return StringCreateFromCStr("class");
        case SyntaxKind::ImportKeyword:
            return StringCreateFromCStr("import");

        case SyntaxKind::ExternKeyword:
            return StringCreateFromCStr("extern");
        case SyntaxKind::IncludeDirectiveKeyword:
            return StringCreateFromCStr("#include");
        case SyntaxKind::DefineDirectiveKeyword:
            return StringCreateFromCStr("#define");
        case SyntaxKind::IfDirectiveKeyword:
            return StringCreateFromCStr("#if");
        case SyntaxKind::EndIfDefinedDirectiveKeyword:
            return StringCreateFromCStr("#endif");
        case SyntaxKind::PragmaDirectiveKeyword:
            return StringCreateFromCStr("#pragma");
        case SyntaxKind::TypedefKeyword:
            return StringCreateFromCStr("typedef");

        case SyntaxKind::IdentifierToken:
            return StringCreateFromCStr("identifier");
        
        default:
            fprintf(stderr, "Unrecognized token in `TokenKindToString`: %d", kind);
            exit(1);
    }
}

fun SyntaxKind GetKeywordForIdentifier(String identifier) {
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::CharKeyword)))
        return SyntaxKind::CharKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::ByteKeyword)))
        return SyntaxKind::ByteKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::ShortKeyword)))
        return SyntaxKind::ShortKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::IntKeyword)))
        return SyntaxKind::IntKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::LongKeyword)))
        return SyntaxKind::LongKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::VoidKeyword)))
        return SyntaxKind::VoidKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::NullKeyword)))
        return SyntaxKind::NullKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::CStringKeyword)))
        return SyntaxKind::CStringKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::BoolKeyword)))
        return SyntaxKind::BoolKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::TrueKeyword)))
        return SyntaxKind::TrueKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::FalseKeyword)))
        return SyntaxKind::FalseKeyword;

    if (StringEquals(identifier, TokenKindToString(SyntaxKind::IfKeyword)))
        return SyntaxKind::IfKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::ElseKeyword)))
        return SyntaxKind::ElseKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::DoKeyword)))
        return SyntaxKind::DoKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::WhileKeyword)))
        return SyntaxKind::WhileKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::ForKeyword)))
        return SyntaxKind::ForKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::ReturnKeyword)))
        return SyntaxKind::ReturnKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::BreakKeyword)))
        return SyntaxKind::BreakKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::ContinueKeyword)))
        return SyntaxKind::ContinueKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::SwitchKeyword)))
        return SyntaxKind::SwitchKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::CaseKeyword)))
        return SyntaxKind::CaseKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::DefaultKeyword)))
        return SyntaxKind::DefaultKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::AsKeyword)))
        return SyntaxKind::AsKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::SizeOfKeyword)))
        return SyntaxKind::SizeOfKeyword;

    if (StringEquals(identifier, TokenKindToString(SyntaxKind::FunKeyword)))
        return SyntaxKind::FunKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::LetKeyword)))
        return SyntaxKind::LetKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::LetLocalPersistKeyword)))
        return SyntaxKind::LetLocalPersistKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::StructKeyword)))
        return SyntaxKind::StructKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::UnionKeyword)))
        return SyntaxKind::UnionKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::EnumKeyword)))
        return SyntaxKind::EnumKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::ClassKeyword)))
        return SyntaxKind::ClassKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::ImportKeyword)))
        return SyntaxKind::ImportKeyword;

    if (StringEquals(identifier, TokenKindToString(SyntaxKind::ExternKeyword)))
        return SyntaxKind::ExternKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::IncludeDirectiveKeyword)))
        return SyntaxKind::IncludeDirectiveKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::DefineDirectiveKeyword)))
        return SyntaxKind::DefineDirectiveKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::IfDirectiveKeyword)))
        return SyntaxKind::IfDirectiveKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::EndIfDefinedDirectiveKeyword)))
        return SyntaxKind::EndIfDefinedDirectiveKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::PragmaDirectiveKeyword)))
        return SyntaxKind::PragmaDirectiveKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::TypedefKeyword)))
        return SyntaxKind::TypedefKeyword;

    return SyntaxKind::EndOfFileToken;
}

fun String TokenGetText(SyntaxToken token) {
    return SourceGetSubstring(token.location.source, token.location.start, token.location.end);
}

// -------------------------------------------------------------------------------------------------
// Expressions 

// Literals
struct StringLiteralExpressionSyntax {
    SyntaxInfo info;

    SyntaxNodeArray stringLiteralTokens;
};

struct NullLiteralExpressionSyntax {
    SyntaxInfo info;

    SyntaxToken nullLiteral;
};

struct IntegerLiteralExpressionSyntax {
    SyntaxInfo info;

    SyntaxToken integerLiteral;
};

struct CharacterLiteralExpressionSyntax {
    SyntaxInfo info;

    SyntaxToken characterLiteral;
};

struct BoolLiteralExpressionSyntax {
    SyntaxInfo info;

    SyntaxToken boolLiteral;
};

struct EnumLiteralExpressionSyntax {
    SyntaxInfo info;

    SyntaxToken enumIdentifier;
    SyntaxToken coloncolon;
    SyntaxToken valueIdentifier;
};

struct ArrayLiteralExpressionSyntax {
    SyntaxInfo info;

    SyntaxToken leftBrace;
    SyntaxNodeArray elemsWithSeparators;
    SyntaxToken rightBrace;
};

// Expressions

struct NameExpressionSyntax {
    SyntaxInfo info;

    SyntaxToken identifier;
};

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

struct FuncCallExpressionSyntax {
    SyntaxInfo info;

    SyntaxNode* func;
    SyntaxToken leftParen;
    SyntaxNodeArray argumentsWithSeparators;
    SyntaxToken rightParen;
};

struct ArrayIndexExpressionSyntax {
    SyntaxInfo info;

    SyntaxNode* arr;
    SyntaxToken leftBracket;
    SyntaxNode* indexExpression;
    SyntaxToken rightBracket;
};

struct MemberAccessExpressionSyntax {
    SyntaxInfo info;

    SyntaxNode* container;
    SyntaxToken accessToken;
    SyntaxToken memberIdentifier;
};

struct TypeCastExpressionSyntax {
    SyntaxInfo info;

    SyntaxToken leftParen;
    SyntaxToken asKeyword;
    SyntaxNode* targetTypeExpression;
    SyntaxToken rightParen;
    SyntaxNode* expression;
};

struct TernaryConditionalExpressionSyntax {
    SyntaxInfo info;

    SyntaxNode* conditionExpression;
    SyntaxToken questionmark;
    SyntaxNode* thenExpression;
    SyntaxToken colon;
    SyntaxNode* elseExpression;
};

struct SizeofExpressionSyntax {
    SyntaxInfo info;

    SyntaxToken sizeofKeyword;
    SyntaxToken leftParen;
    SyntaxNode* typeExpression;
    SyntaxToken rightParen;
};

struct TypeExpressionSyntax {
    SyntaxInfo info;

    SyntaxNodeArray typeTokens;
};

// -------------------------------------------------------------------------------------------------
// Statements 

struct BlockStatementSyntax {
    SyntaxInfo info;

    SyntaxToken leftBrace;
    SyntaxNodeArray statements;
    SyntaxToken rightBrace;
};

struct ExpressionStatementSyntax {
    SyntaxInfo info;

    SyntaxNode* expression;
    SyntaxToken semicolon;
};

struct IfStatementSyntax {
    SyntaxInfo info;

    SyntaxToken ifKeyword;
    SyntaxToken leftParen;
    SyntaxNode* condition;
    SyntaxToken rightParen;
    SyntaxNode* thenBlock;
    SyntaxToken elseKeyword;
    SyntaxNode* elseBlock;
};

struct DoWhileStatementSyntax {
    SyntaxInfo info;

    SyntaxToken doKeyword;
    SyntaxNode* body;
    SyntaxToken whileKeyword;
    SyntaxToken leftParen;
    SyntaxNode* condition;
    SyntaxToken rightParen;
    SyntaxToken semicolon;
};

struct WhileStatementSyntax {
    SyntaxInfo info;

    SyntaxToken whileKeyword;
    SyntaxToken leftParen;
    SyntaxNode* condition;
    SyntaxToken rightParen;
    SyntaxNode* body;
};

struct ForStatementSyntax {
    SyntaxInfo info;

    SyntaxToken forKeyword;
    SyntaxToken leftParen;
    SyntaxNode* initializerStatement;
    SyntaxNode* conditionStatement;
    SyntaxNode* incrementExpression;
    SyntaxToken rightParen;
    SyntaxNode* body;
};

struct ReturnStatementSyntax {
    SyntaxInfo info;

    SyntaxToken returnKeyword;
    SyntaxNode* returnExpression;
    SyntaxToken semicolon;
};

struct BreakStatementSyntax {
    SyntaxInfo info;

    SyntaxToken breakKeyword;
    SyntaxToken semicolon;
};

struct ContinueStatementSyntax {
    SyntaxInfo info;

    SyntaxToken continueKeyword;
    SyntaxToken semicolon;
};

struct SwitchStatementSyntax {
    SyntaxInfo info;

    SyntaxToken switchKeyword;
    SyntaxToken leftParen;
    SyntaxNode* switchExpression;
    SyntaxToken rightParen;

    SyntaxToken leftBrace;
    SyntaxNodeArray caseStatements;
    SyntaxToken rightBrace;
};

struct CaseStatementSyntax {
    SyntaxInfo info;

    SyntaxToken caseKeyword;
    SyntaxNode* literalExpression;
    SyntaxToken colon;

    SyntaxNode* body;
};

struct DefaultStatementSyntax {
    SyntaxInfo info;

    SyntaxToken defaultKeyword;
    SyntaxToken colon;

    SyntaxNode* body;
};

struct VariableDeclarationStatementSyntax {
    SyntaxInfo info;

    SyntaxToken letKeyword;
    SyntaxNode* typeExpression;
    SyntaxToken identifier;

    // NOTE: These three are optional
    SyntaxToken leftBracket;
    SyntaxToken arraySizeLiteral;
    SyntaxToken rightBracket;
    
    SyntaxToken equalsToken;
    SyntaxNode* initializerExpression;
    SyntaxToken terminatorToken;
};

// -------------------------------------------------------------------------------------------------
// Module 

struct ModuleStatementSyntax {
    SyntaxInfo info;

    SyntaxNodeArray globalStatements;
};

struct ImportDeclarationStatementSyntax {
    SyntaxInfo info;

    SyntaxToken importKeyword;
    SyntaxToken modulenameLiteral;
};

struct GlobalVariableStatementSyntax {
    SyntaxInfo info;

    SyntaxToken externKeyword;
    SyntaxNode* variableDeclarationStatement;
};

struct EnumMemberClauseSyntax {
    SyntaxInfo info;

    SyntaxToken identifier;
    SyntaxToken equals;
    SyntaxToken integerLiteral;
    SyntaxToken comma;
};

struct EnumDeclarationStatementSyntax {
    SyntaxInfo info;

    SyntaxToken externKeyword;
    SyntaxToken enumKeyword;
    SyntaxToken classKeyword;
    SyntaxToken identifier;

    // NOTE: Until here this matches up with `EnumDefinitionStatementSyntax`
    SyntaxToken semicolon;
};

struct EnumDefinitionStatementSyntax {
    SyntaxInfo info;

    SyntaxToken externKeyword;
    SyntaxToken enumKeyword;
    SyntaxToken classKeyword;
    SyntaxToken identifier;

    // NOTE: Until here this matches up with `EnumDeclarationStatementSyntax`
    SyntaxToken leftBrace;
    SyntaxNodeArray memberClauses;
    SyntaxToken rightBrace;
    SyntaxToken semicolon;
};

struct StructOrUnionDeclarationStatementSyntax {
    SyntaxInfo info;

    SyntaxToken externKeyword;
    SyntaxToken structOrUnionKeyword;
    SyntaxToken identifier;

    // NOTE: Until here this matches up with `StructOrUnionDefinitionStatementSyntax`
    SyntaxToken semicolon;
};

struct StructOrUnionDefinitionStatementSyntax {
    SyntaxInfo info;

    SyntaxToken externKeyword;
    SyntaxToken structOrUnionKeyword;
    SyntaxToken identifier;

    // NOTE: Until here this matches up with `StructOrUnionDeclarationStatementSyntax`
    SyntaxToken leftBrace;
    SyntaxNodeArray memberDeclarationStatements;
    SyntaxToken rightBrace;
    SyntaxToken semicolon;
};

struct FunctionParameterClauseSyntax {
    SyntaxInfo info;

    SyntaxNode* typeExpression;
    SyntaxToken identifier;
    SyntaxToken comma;
    SyntaxToken dotdot;
};

struct FunctionDeclarationStatementSyntax {
    SyntaxInfo info;

    SyntaxToken externKeyword;
    SyntaxToken funKeyword;
    SyntaxNode* returnType;
    SyntaxToken identifier;

    SyntaxToken leftParen;
    SyntaxNodeArray params;
    SyntaxToken rightParen;

    // NOTE: Until here this matches up with `FunctionDefinitionStatementSyntax`
    SyntaxToken semicolon;
};

struct FunctionDefinitionStatementSyntax {
    SyntaxInfo info;

    SyntaxToken externKeyword;
    SyntaxToken funKeyword;
    SyntaxNode* returnType;
    SyntaxToken identifier;

    SyntaxToken leftParen;
    SyntaxNodeArray params;
    SyntaxToken rightParen;

    // NOTE: Until here this matches up with `FunctionDeclarationStatementSyntax`
    SyntaxNode* body;
};

// -------------------------------------------------------------------------------------------------
// SyntaxNode 

union SyntaxNode {
    SyntaxKind kind;
    SyntaxInfo info;

    SyntaxToken token;

    // Literals
    StringLiteralExpressionSyntax stringLiteralExpr;
    NullLiteralExpressionSyntax nullLiteralExpr;
    IntegerLiteralExpressionSyntax integerLiteralExpr;
    CharacterLiteralExpressionSyntax characterLiteralExpr;
    BoolLiteralExpressionSyntax boolLiteralExpr;
    EnumLiteralExpressionSyntax enumLiteralExpr;
    ArrayLiteralExpressionSyntax arrayLiteralExpr;

    // Expressions
    NameExpressionSyntax nameExpr; 
    UnaryExpressionSyntax unaryExpr; 
    BinaryExpressionSyntax binaryExpr; 
    ParenthesizedExpressionSyntax parenthesizedExpr; 
    FuncCallExpressionSyntax funcCallExpr; 
    ArrayIndexExpressionSyntax arrayIndexExpr; 
    MemberAccessExpressionSyntax memberAccessExpr; 
    TypeCastExpressionSyntax typeCastExpr; 
    TernaryConditionalExpressionSyntax ternaryConditionalExpr; 
    SizeofExpressionSyntax sizeofExpr; 
    TypeExpressionSyntax typeExpr; 

    // Statements 
    BlockStatementSyntax blockStmt; 
    ExpressionStatementSyntax expressionStmt; 
    IfStatementSyntax ifStmt; 
    DoWhileStatementSyntax doWhileStmt; 
    WhileStatementSyntax whileStmt; 
    ForStatementSyntax forStmt; 
    ReturnStatementSyntax returnStmt; 
    BreakStatementSyntax breakStmt; 
    ContinueStatementSyntax continueStmt; 
    SwitchStatementSyntax switchStmt; 
    CaseStatementSyntax caseStmt; 
    DefaultStatementSyntax defaultStmt; 
    VariableDeclarationStatementSyntax variableDeclarationStmt; 

    // Helper clauses
    EnumMemberClauseSyntax enumMember;

    // Module 
    ModuleStatementSyntax moduleStmt; 
    ImportDeclarationStatementSyntax importStmt;
    GlobalVariableStatementSyntax globalVariableStmt; 
    EnumDeclarationStatementSyntax enumDeclarationStmt; 
    EnumDefinitionStatementSyntax enumDefinitionStmt; 
    StructOrUnionDeclarationStatementSyntax structOrUnionDeclarationStmt; 
    StructOrUnionDefinitionStatementSyntax structOrUnionDefinitionStmt; 
    FunctionDeclarationStatementSyntax functionDeclarationStmt; 
    FunctionDefinitionStatementSyntax functionDefinitionStmt; 
};

fun SyntaxNode* SyntaxNodeCreate(SyntaxKind kind, SyntaxTree* tree) {
    let SyntaxNode* node = (as SyntaxNode*) malloc(sizeof(SyntaxNode));
    assert(node != nullptr);
    node->info.kind = kind;
    node->info.tree = tree;
    return node;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Types 

enum class TypeKind {
    PrimitiveNull,
    PrimitiveVoid,
    PrimitiveBool,
    PrimitiveChar,
    PrimitiveByte,
    PrimitiveShort,
    PrimitiveInt,
    PrimitiveLong,
    PrimitiveCString,
    Struct,
    Union,
    Enum,
};

struct Type {
    TypeKind kind;
    int baseIndirectionLevel;   // How many * come after type: int**** -> indirectionLevel == 4
    String name;       // For structs, unions, enums
    bool isArray; 
    longint arrayElementCount; 
};

fun Type TypeCreate(TypeKind kind, int baseIndirectionLevel, String name) {
    let Type result;
    result.kind = kind;
    result.baseIndirectionLevel = baseIndirectionLevel;
    result.name = name;
    result.isArray = false;
    result.arrayElementCount = -1;
    return result;
}

fun int TypeGetIndirectionLevel(Type type) {
    return type.baseIndirectionLevel + (as int)type.isArray + (as int)(type.kind == TypeKind::PrimitiveCString);
}

fun Type GetElementTypeForArrayType(Type type) {
    assert(type.isArray);
    type.isArray = false;
    return type;
}

fun String TypeGetText(Type type) {
    let char* buffer = (as char*) malloc(128);
    let int bufferpos = 0;
    switch (type.kind) {
        case TypeKind::PrimitiveNull:
            bufferpos = snprintf(buffer, 128, "void");
            assert(!type.isArray);
            assert(type.baseIndirectionLevel == 1);
            break;
        case TypeKind::PrimitiveVoid:
            bufferpos = snprintf(buffer, 128, "void");
            break;
        case TypeKind::PrimitiveBool:
            bufferpos = snprintf(buffer, 128, "bool");
            break;
        case TypeKind::PrimitiveChar:
            bufferpos = snprintf(buffer, 128, "char");
            break;
        case TypeKind::PrimitiveByte:
            bufferpos = snprintf(buffer, 128, "byte");
            break;
        case TypeKind::PrimitiveShort:
            bufferpos = snprintf(buffer, 128, "short");
            break;
        case TypeKind::PrimitiveInt:
            bufferpos = snprintf(buffer, 128, "int");
            break;
        case TypeKind::PrimitiveLong:
            bufferpos = snprintf(buffer, 128, "long");
            break;
        case TypeKind::PrimitiveCString:
            bufferpos = snprintf(buffer, 128, "cstring");
            break;
        case TypeKind::Struct:
        case TypeKind::Union:
        case TypeKind::Enum:
            bufferpos = snprintf(buffer, 128, "%s", type.name.cstr);
            break;
        default:
            assert(false && "unknown kind");
    }

    for (let int index = 0; index < type.baseIndirectionLevel; index += 1) {
        buffer[bufferpos] = '*';
        bufferpos +=1;
    }
    if (type.isArray) {
        buffer[bufferpos] = '[';
        bufferpos +=1;
        buffer[bufferpos] = ']';
        bufferpos +=1;
    }
    buffer[bufferpos] = '\0';

    let String result;
    result.cstr = buffer;
    result.length = bufferpos;
    return result;
}

fun Type TypeCreatePrimitive(TypeKind kind) {
    assert(kind != TypeKind::Struct);
    return TypeCreate(kind, 0, StringCreateEmpty());
}

fun Type TypeCreateVoid() {
    return TypeCreatePrimitive(TypeKind::PrimitiveVoid);
}

fun bool IsNull(Type type) {
    return type.kind == TypeKind::PrimitiveNull;
}

fun bool IsVoidType(Type type) {
    return type.kind == TypeKind::PrimitiveVoid && TypeGetIndirectionLevel(type) == 0;
}

fun bool IsVoidPointer(Type type) {
    return type.kind == TypeKind::PrimitiveVoid && type.baseIndirectionLevel == 1 && !type.isArray;
}

fun bool IsCharType(Type type) {
    if (TypeGetIndirectionLevel(type) > 0)
        return false;
    return type.kind == TypeKind::PrimitiveChar;
}

fun bool IsNumberType(Type type) {
    if (TypeGetIndirectionLevel(type) > 0)
        return false;

    switch (type.kind) {
        case TypeKind::PrimitiveBool:
        case TypeKind::PrimitiveByte:
        case TypeKind::PrimitiveShort:
        case TypeKind::PrimitiveInt:
        case TypeKind::PrimitiveLong:
            return true;

        default:
            return false;
    }
}

fun bool IsPointerType(Type type) {
    return type.kind == TypeKind::PrimitiveCString || (type.baseIndirectionLevel > 0 && !type.isArray);
}

fun bool TypesIdentical(Type a, Type b) {
    if (a.kind != b.kind )
        return false;

    if (TypeGetIndirectionLevel(a) != TypeGetIndirectionLevel(b))
        return false;

    return StringEquals(a.name, b.name);
}

enum class TypeConversionResult {
    NonConvertible,
    Identical,
    ImplictlyConvertible,
    ExplicitlyConvertible,
};

fun TypeConversionResult CanConvertTypeFromTo(Type from, Type to) {
    if (IsVoidType(from) || IsVoidType(to)) 
        return TypeConversionResult::NonConvertible;

    if (TypesIdentical(from, to))
        return TypeConversionResult::Identical;

    if (TypeGetIndirectionLevel(from) == 0 && TypeGetIndirectionLevel(to) == 0) {
        if (IsNumberType(from) && IsNumberType(to)) {
            if (from.kind <= to.kind)
                return TypeConversionResult::ImplictlyConvertible;
            else 
                return TypeConversionResult::ExplicitlyConvertible;
        }

        if (from.kind == TypeKind::PrimitiveChar && IsNumberType(to)) {
            return TypeConversionResult::ExplicitlyConvertible;
        }
        if (IsNumberType(from) && to.kind == TypeKind::PrimitiveChar) {
            return TypeConversionResult::ExplicitlyConvertible;
        }

        if (from.kind == TypeKind::Enum && IsNumberType(to)) {
            return TypeConversionResult::ExplicitlyConvertible;
        }
        if (IsNumberType(from) && to.kind == TypeKind::Enum) {
            return TypeConversionResult::ExplicitlyConvertible;
        }
    }

    if (IsPointerType(from) && IsVoidPointer(to))
        return TypeConversionResult::ImplictlyConvertible;
    if (IsVoidPointer(from) && IsPointerType(to))
        return TypeConversionResult::ImplictlyConvertible;

    if (from.baseIndirectionLevel == 1 && to.baseIndirectionLevel == 1) {
        if (from.kind == TypeKind::PrimitiveVoid || to.kind == TypeKind::PrimitiveVoid)
            return TypeConversionResult::ImplictlyConvertible;
        else if (IsNull(from) || IsNull(to))
            return TypeConversionResult::ImplictlyConvertible;
        else
            return TypeConversionResult::ExplicitlyConvertible;
    }

    if (IsPointerType(from) && IsNull(to))
        return TypeConversionResult::ImplictlyConvertible;

    if (IsNull(from) && IsPointerType(to))
        return TypeConversionResult::ImplictlyConvertible;

    if (IsNull(from) && to.kind == TypeKind::PrimitiveCString)
        return TypeConversionResult::ImplictlyConvertible;

    if (from.kind == TypeKind::PrimitiveCString && IsNull(to))
        return TypeConversionResult::ImplictlyConvertible;

    if (from.kind == to.kind && from.isArray && !to.isArray && TypeGetIndirectionLevel(from) == TypeGetIndirectionLevel(to))
        return TypeConversionResult::ImplictlyConvertible;

    if (from.kind == TypeKind::PrimitiveChar 
    && to.kind == TypeKind::PrimitiveCString 
    && TypeGetIndirectionLevel(from) == TypeGetIndirectionLevel(to)) {
        return TypeConversionResult::ImplictlyConvertible;
    }
    
    return TypeConversionResult::NonConvertible;
}

fun Type GetPointerTypeForBaseType(Type type) {
    assert(type.kind != TypeKind::PrimitiveNull);
    let Type result = type;
    result.baseIndirectionLevel += 1;
    return result;
}

fun Type GetBaseTypeForPointerType(Type type) {
    assert(type.baseIndirectionLevel > 0 || type.kind == TypeKind::PrimitiveCString);
    let Type result = type;
    if (result.baseIndirectionLevel > 0) {
        result.baseIndirectionLevel -= 1;
    } else if (type.kind == TypeKind::PrimitiveCString) {
        result.kind = TypeKind::PrimitiveChar;
    }
    return result;
}

// TODO: make sure that this somewhat matches up with the `CanConvertTypeFromTo(..)` function
fun Type GetTypeThatFitsBothTypes(Type a, Type b) { 
    let TypeConversionResult conversion = CanConvertTypeFromTo(a, b);

    if (TypesIdentical(a, b))
        return a;
    
    if (a.baseIndirectionLevel == 0 && b.baseIndirectionLevel == 0) {
        if (IsNumberType(a) && IsNumberType(b)) {
            if (a.kind <= b.kind)
                return b;
            else 
                return a;
        }
    }

    if (IsPointerType(a) && IsNull(b))
        return a;
    if (IsPointerType(b) && IsNull(a))
        return b;

    return TypeCreateVoid();
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// AST 

enum class ASTNodeKind {
    // Unary operations
    Identity,
    Negation,
    LogicalNegation,
    BitwiseNegation,
    Dereference,
    Address,

    // Binary operations
    Assignment,
    Add,
    AddAssignment,
    Subtract,
    SubtractAssignment,
    Multiply,
    MultiplyAssignment,
    Divide,
    DivideAssignment,
    Remainder,
    RemainderAssignment,
    // Bits
    BitwiseXor,
    BitwiseXorAssignment,
    BitwiseAnd,
    BitwiseAndAssignment,
    BitwiseOr,
    BitwiseOrAssignment,
    BitshiftLeft,
    BitshiftLeftAssignment,
    BitshiftRight,
    BitshiftRightAssignment,
    // Logical
    LogicalAnd,
    LogicalOr,
    // Pointer math
    AddToPointer,
    AddToPointerAssignment,
    SubtractFromPointer,
    SubtractFromPointerAssignment,
    DistanceBetweenPointers,
    // Comparisons
    Equals,
    NotEquals,
    Less,
    LessEquals,
    Greater,
    GreaterEquals,

    // Expressions
    NameExpression,
    FunccallExpression,
    Arrayindexing,
    Memberaccess,
    CastExpression,
    ParenthesizedExpression,
    TernaryConditionalExpression,
    SizeOfExpression,
    TypeExpression, // TODO: currently used for sizeof expression. lets see if we need this in the long run

    // Literals
    NullLiteral,
    IntegerLiteral,
    CharacterLiteral,
    BoolLiteral,
    StringLiteral,
    EnumValueLiteral,
    ArrayLiteral,

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

    EnumDeclarationStatement,
    EnumDefinitionStatement,
    StructDeclarationStatement,
    StructDefinitionStatement,
    UnionDeclarationStatement,
    UnionDefinitionStatement,
    FunctionDeclarationStatement,
    FunctionDefinitionStatement,
    VariableDeclarationStatement,
    ArrayDeclarationStatement,

    // Contains global function- and variable declarations
    Module,
};

struct ASTNode;

struct ASTNodeArray {
    ASTNode** nodes;
    int count;
    int capacity;
};

fun ASTNodeArray ASTNodeArrayCreate() {
    let ASTNodeArray result;
    result.nodes = nullptr;
    result.count = 0;
    result.capacity = 0;
    return result;
}

fun void ASTNodeArrayGrow(ASTNodeArray* array, int newCapacity) {
    array->capacity = newCapacity;
    array->nodes = (as ASTNode**) realloc(array->nodes, newCapacity * sizeof(ASTNode*));
    if (array->nodes == nullptr) {
        fprintf(stderr, "Unable to malloc new AST array buffer");
        exit(1);
    }
}

fun int ASTNodeArrayPush(ASTNodeArray* array, ASTNode* node) {
    if (array->count == array->capacity) {
        let int newCapacity = 2 * array->capacity;
        if (newCapacity == 0)
            newCapacity = 64;
        ASTNodeArrayGrow(array, newCapacity);
    }
    let int insertionIndex = array->count;
    array->nodes[insertionIndex] = node;
    array->count += 1;
    return insertionIndex;
}

struct Symbol;
struct SymbolTable;

struct ASTNode {
    ASTNodeKind kind;
    Type type;                // Mainly for expressions
    SymbolTable* symbolTable; // Can be the global table but can also be a local scope

    // These are used in a variety of ways by our nodes. For now we can look at what the parser
    // and emitter does to understand how each of these are filled/used until we have something 
    // more useful.
    SyntaxNode* syntax;
    SyntaxToken token;
    ASTNode* left;
    ASTNode* right;
    ASTNode* extra1; // Used by 'while' statements
    ASTNode* extra2; // Used by 'for' statements

    bool isRValue;
    longint intvalue;     // For ASTNodeKind::Integerliteral and ASTNodeKind::IntegerliteralHex
    String stringvalue;      // For ASTNodeKind::Stringliteral
    Symbol* symbol;         // For variable/function declarations
    // For ASTNodeKind::BlockStatement, ASTNodeKind::FunccallExpression arguments, 
    // and ASTNodeKind::SwitchStatement
    ASTNodeArray children;
};

fun ASTNode* ASTNodeCreate(ASTNodeKind kind, SymbolTable* symbolTable, SyntaxToken token) {
    let ASTNode* node = (as ASTNode*) malloc(sizeof(ASTNode));
    if (node == nullptr) {
        fprintf(stderr, "Unable to malloc new AST Node");
        exit(1);
    }
    node->kind = kind;
    node->type = TypeCreateVoid();
    node->symbolTable = symbolTable;

    node->token = token;
    node->syntax = nullptr;
    node->left = nullptr;
    node->right = nullptr;
    node->extra1 = nullptr;
    node->extra2 = nullptr;

    node->isRValue = false;
    node->intvalue = 0;
    node->symbol = nullptr;
    node->children = ASTNodeArrayCreate();
    return node;
}

fun ASTNode* ASTNodeCreate2(ASTNodeKind kind, SymbolTable* symbolTable, SyntaxNode* syntax) {
    let ASTNode* node = (as ASTNode*) malloc(sizeof(ASTNode));
    if (node == nullptr) {
        fprintf(stderr, "Unable to malloc new AST Node");
        exit(1);
    }
    node->kind = kind;
    node->type = TypeCreateVoid();
    node->symbolTable = symbolTable;

    // node->token = token;
    node->syntax = syntax;
    node->left = nullptr;
    node->right = nullptr;
    node->extra1 = nullptr;
    node->extra2 = nullptr;

    node->isRValue = false;
    node->intvalue = 0;
    node->symbol = nullptr;
    node->children = ASTNodeArrayCreate();
    return node;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Symbol Table

enum class SymbolKind {
    Function,
    Struct,
    Union,
    Enum,
    Variable,
    Parameter,  // Valid in functions only
    Member,     // Valid in struct/union members only
    Enumvalue,  // For enums only
};

enum class SymbolScopeKind {
    Extern,
    Global,
    Local,
    LocalPersist,
};

struct SymbolTable;

struct Symbol {
    String name;
    SymbolKind kind;
    SymbolScopeKind scopeKind;
    Type type;

    // For structs/unions/functions
    bool isVariadric;
    SymbolTable* membersSymbolTable; // Holds function parameters, enum/union/struct members
    bool alreadyDefined; // For functions/structs to indicate if they have been defined yet
    longint enumValue; // For enum values
};

fun Symbol* SymbolCreate(String name, SymbolKind kind, SymbolScopeKind scopeKind, Type type) {
    let Symbol* newSymbol = (as Symbol*)malloc(sizeof(Symbol));
    newSymbol->name = name;
    newSymbol->kind = kind;
    newSymbol->scopeKind = scopeKind;
    newSymbol->type = type;
    newSymbol->isVariadric = false;
    newSymbol->membersSymbolTable = nullptr;
    newSymbol->alreadyDefined = false;
    newSymbol->enumValue = 0;
    return newSymbol;
}

struct SymbolTable {
    SymbolTable* parent;

    Symbol** symbols;
    int count;
    int capacity;
};

fun SymbolTable* SymbolTableCreate(SymbolTable* parent) {
    let SymbolTable* result = (as SymbolTable*)malloc(sizeof(SymbolTable));
    result->parent = parent;
    result->symbols = nullptr;
    result->count = 0;
    result->capacity = 0;
    return result;
}

fun void SymbolTableGrow(SymbolTable* table, int newCapacity) {
    table->capacity = newCapacity;
    table->symbols = (as Symbol**) realloc(table->symbols, newCapacity * sizeof(Symbol*));
    if (table->symbols == nullptr) {
        fprintf(stderr, "Unable to malloc new AST array buffer");
        exit(1);
    }
}

fun int SymbolTablePush(SymbolTable* table, Symbol* symbol) {
    if (table->count == table->capacity) {
        let int newCapacity = 2 * table->capacity;
        if (newCapacity == 0)
            newCapacity = 64;
        SymbolTableGrow(table, newCapacity);
    }
    let int insertionIndex = table->count;
    table->symbols[insertionIndex] = symbol;
    table->count += 1;
    return insertionIndex;
}

fun Symbol* GetSymbolFromLocalScope(SymbolTable* table, String name) {
    for (let int index = 0; index < table->count; index += 1) {
        if (StringEquals(name, table->symbols[index]->name))
            return table->symbols[index];
    }
    return nullptr;
}

fun Symbol* GetSymbol(SymbolTable* table, String name) {
    let Symbol* result = GetSymbolFromLocalScope(table, name);
    if (result != nullptr)
        return result;
    if (table->parent != nullptr)
        return GetSymbol(table->parent, name);

    return nullptr;
}

fun Symbol* AddSymbol(SymbolTable* table, String name, SymbolKind kind, SymbolScopeKind scopeKind, Type type) {
    let Symbol* existingLocal = GetSymbolFromLocalScope(table, name);
    if (existingLocal != nullptr) {
        // We don't allow overwriting symbols from our local scope
        return nullptr;
    }

    let Symbol* existing = GetSymbol(table, name);
    if (existing != nullptr) {
        // We don't allow shadowing parameter type symbols from our surrounding scope
        if (existing->kind == SymbolKind::Parameter)
            return nullptr;
    }

    let Symbol* newSymbol = SymbolCreate(name, kind, scopeKind, type);
    SymbolTablePush(table, newSymbol);
    return newSymbol;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Operators

fun int GetUnaryOperatorPrecedence(SyntaxKind kind)
{
    switch (kind) {
        case SyntaxKind::PlusToken:      // Identity
        case SyntaxKind::MinusToken:     // Negation
        case SyntaxKind::BangToken:      // Logical negation
        case SyntaxKind::TildeToken:     // Bitwise negation

        case SyntaxKind::StarToken:      // Dereference
        case SyntaxKind::AmpersandToken: // Addressof
            return 13;
        default:
            return 0;
    }
}

fun bool IsBinaryOperatorRightAssociative(SyntaxKind kind) {
    switch (kind) {
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
        return true;

        default:
        return false;
    }
}

fun int GetBinaryOperatorPrecedence(SyntaxKind kind)
{
    switch (kind) {
        case SyntaxKind::StarToken:
        case SyntaxKind::SlashToken:
        case SyntaxKind::PercentToken:
            return 12;

        case SyntaxKind::PlusToken:
        case SyntaxKind::MinusToken:
            return 11;

        // BitshiftIng
        case SyntaxKind::LessLessToken:
        case SyntaxKind::GreaterGreaterToken:
            return 10;

        case SyntaxKind::LessToken:
        case SyntaxKind::LessEqualsToken:
        case SyntaxKind::GreaterToken:
        case SyntaxKind::GreaterEqualsToken:
            return 9;

        case SyntaxKind::EqualsEqualsToken:
        case SyntaxKind::BangEqualsToken:
            return 8;

        case SyntaxKind::AmpersandToken: // Bitwise AND
            return 7;

        case SyntaxKind::HatToken:       // Bitwise XOR
            return 6;

        case SyntaxKind::PipeToken:      // Bitwise OR
            return 5;

        case SyntaxKind::AmpersandAmpersandToken: // Logical AND
            return 4;

        case SyntaxKind::PipePipeToken: // Logical OR
            return 3;
        
        // Ternary conditional (is handled in in the parser as special node)
        // case SyntaxKind::QuestionMarkToken:
        //  return 2

        // Assignment (is handled in in the parser as special node)
        // case SyntaxKind::EqualsToken: 
        // case SyntaxKind::PlusEqualsToken: 
        // case SyntaxKind::MinusEqualsToken: 
        // case SyntaxKind::StarEqualsToken: 
        // case SyntaxKind::SlashEqualsToken:
        // case SyntaxKind::PercentEqualsToken:
        // case SyntaxKind::HatEqualsToken:
        // case SyntaxKind::AmpersandEqualsToken:
        // case SyntaxKind::PipeEqualsToken:
        // case SyntaxKind::LessLessEqualsToken:
        // case SyntaxKind::GreaterGreaterEqualsToken:
        //      return 1;

        default:
            return 0;
    }
}

struct UnaryOperator {
    SyntaxKind tokenKind;
    ASTNodeKind operatorKind;
    Type operandType;
    Type resultType;
    bool resultIsRValue;
    bool operandMustBeLValue;
};

fun UnaryOperator GetUnaryOperationForToken(SyntaxToken token, Type operandType) {
    let UnaryOperator result;
    result.tokenKind = token.kind;
    result.operandType = operandType;
    result.resultType = operandType;
    result.resultIsRValue = true;
    result.operandMustBeLValue = false;

    if (IsPointerType(operandType) || operandType.kind == TypeKind::PrimitiveCString) {
        if (token.kind == SyntaxKind::StarToken) {
            result.operatorKind = ASTNodeKind::Dereference;
            result.resultType = GetBaseTypeForPointerType(operandType);
            result.resultIsRValue = false;
            result.operandMustBeLValue = true;
            return result;
        }
    } else if (IsNumberType(operandType)) {
        if (token.kind == SyntaxKind::PlusToken) {
            result.operatorKind = ASTNodeKind::Identity;
            return result;
        }
        if (token.kind == SyntaxKind::MinusToken)
        {
            result.operatorKind = ASTNodeKind::Negation;
            return result;
        }
        if (token.kind == SyntaxKind::BangToken) 
        {
            result.operatorKind = ASTNodeKind::LogicalNegation;
            return result;
        }
        if (token.kind == SyntaxKind::TildeToken) {
            result.operatorKind = ASTNodeKind::BitwiseNegation;
            return result;
        }
    }
    if (token.kind == SyntaxKind::AmpersandToken) {
        result.operatorKind = ASTNodeKind::Address;
        result.resultType = GetPointerTypeForBaseType(operandType);
        result.resultIsRValue = false;
        result.operandMustBeLValue = true;
        return result;
    }

    ReportError(
        token.location, 
        "No applicable unary operation for combination token '%s', type '%s'", 
        TokenKindToString(token.kind).cstr, TypeGetText(operandType).cstr
    );
    exit(1);
}

struct BinaryOperator {
    SyntaxKind tokenKind;
    ASTNodeKind operatorKind;
    Type leftType;
    Type rightType;
    Type resultType;
    bool resultIsRValue;
    bool leftMustBeLValue;
    bool rightMustBeLValue;
};

fun BinaryOperator GetBinaryOperationForToken(SyntaxToken token, Type leftType, Type rightType) {
    let BinaryOperator result;
    result.tokenKind = token.kind;
    result.leftType = leftType;
    result.rightType = rightType;
    result.resultType = leftType;
    result.resultIsRValue = true;
    result.leftMustBeLValue = false;
    result.rightMustBeLValue = false;

    if (token.kind == SyntaxKind::EqualsToken) { 
        let TypeConversionResult conversion = CanConvertTypeFromTo(rightType, leftType);
        if (conversion == TypeConversionResult::NonConvertible) {
            ReportError(
                token.location,
                "Incompatible types for assignment '%s' = '%s'", 
                TypeGetText(leftType).cstr, TypeGetText(rightType).cstr
            );
        }
        if (conversion == TypeConversionResult::ExplicitlyConvertible) {
            ReportError(
                token.location,
                "Cannot implicitly convert types for assignment '%s' = '%s'", 
                TypeGetText(leftType).cstr, TypeGetText(rightType).cstr
            );
        }
        result.operatorKind = ASTNodeKind::Assignment;
        result.leftMustBeLValue = true;
        return result;
    }

    if ((token.kind == SyntaxKind::PlusToken) && IsPointerType(leftType) && IsNumberType(rightType)) { 
        result.operatorKind = ASTNodeKind::AddToPointer;
        result.resultIsRValue = false;
        result.leftMustBeLValue = true;
        return result;
    }
    if ((token.kind == SyntaxKind::PlusEqualsToken) && IsPointerType(leftType) && IsNumberType(rightType)) { 
        result.operatorKind = ASTNodeKind::AddToPointerAssignment;
        result.resultIsRValue = false;
        result.leftMustBeLValue = true;
        return result;
    }
    if ((token.kind == SyntaxKind::MinusToken) && IsPointerType(leftType) && IsNumberType(rightType)) {
        result.operatorKind = ASTNodeKind::SubtractFromPointer;
        result.resultIsRValue = false;
        result.leftMustBeLValue = true;
        return result;
    }
    if ((token.kind == SyntaxKind::MinusEqualsToken) && IsPointerType(leftType) && IsNumberType(rightType)) {
        result.operatorKind = ASTNodeKind::SubtractFromPointerAssignment;
        result.resultIsRValue = false;
        result.leftMustBeLValue = true;
        return result;
    }
    if ((token.kind == SyntaxKind::MinusToken) 
        && IsPointerType(leftType) && IsPointerType(rightType)
        && leftType.baseIndirectionLevel == rightType.baseIndirectionLevel 
        && (leftType.kind == rightType.kind)) {
        result.operatorKind = ASTNodeKind::DistanceBetweenPointers;
        result.leftMustBeLValue = true;
        result.rightMustBeLValue = true;
        result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveLong);
        return result;
    }

    if (token.kind == SyntaxKind::EqualsEqualsToken) {
        let TypeConversionResult conversion = CanConvertTypeFromTo(leftType, rightType);
        if (conversion == TypeConversionResult::ImplictlyConvertible 
        || conversion == TypeConversionResult::Identical) {
            result.operatorKind = ASTNodeKind::Equals;
            result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
            return result;
        }
    }
    if (token.kind == SyntaxKind::BangEqualsToken) {
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
            case SyntaxKind::EqualsEqualsToken: {
                result.operatorKind = ASTNodeKind::Equals;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
            case SyntaxKind::BangEqualsToken: {
                result.operatorKind = ASTNodeKind::NotEquals;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
            case SyntaxKind::LessToken: {
                result.operatorKind = ASTNodeKind::Less;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
            case SyntaxKind::LessEqualsToken: {
                result.operatorKind = ASTNodeKind::LessEquals;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
            case SyntaxKind::GreaterToken: {
                result.operatorKind = ASTNodeKind::Greater;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
            case SyntaxKind::GreaterEqualsToken: {
                result.operatorKind = ASTNodeKind::GreaterEquals;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
        }
    }

    // TODO: introduce concept of 'truthyness' 
    // We basically allow almost any types for the logical operators
    if (token.kind == SyntaxKind::AmpersandAmpersandToken && !IsVoidType(leftType) && !IsVoidType(rightType)) {
        result.operatorKind = ASTNodeKind::LogicalAnd;
        return result;
    }
    if (token.kind == SyntaxKind::PipePipeToken && !IsVoidType(leftType) && !IsVoidType(rightType)) {
        result.operatorKind = ASTNodeKind::LogicalOr;
        return result;
    }

    if (IsNumberType(leftType) && IsNumberType(rightType) || IsCharType(leftType) && IsCharType(rightType)) {
        switch (token.kind) {
            case SyntaxKind::PlusToken: {
                result.operatorKind = ASTNodeKind::Add;
                return result;
            }
            case SyntaxKind::PlusEqualsToken: {
                result.operatorKind = ASTNodeKind::AddAssignment;
                result.resultIsRValue = false;
                result.leftMustBeLValue = true;
                return result;
            }
            case SyntaxKind::MinusToken: {
                result.operatorKind = ASTNodeKind::Subtract;
                return result;
            }
            case SyntaxKind::MinusEqualsToken: {
                result.operatorKind = ASTNodeKind::SubtractAssignment;
                result.resultIsRValue = false;
                result.leftMustBeLValue = true;
                return result;
            }
            case SyntaxKind::StarToken: {
                result.operatorKind = ASTNodeKind::Multiply;
                return result;
            }
            case SyntaxKind::StarEqualsToken: {
                result.operatorKind = ASTNodeKind::MultiplyAssignment;
                result.resultIsRValue = false;
                result.leftMustBeLValue = true;
                return result;
            }
            case SyntaxKind::SlashToken: {
                result.operatorKind = ASTNodeKind::Divide;
                return result;
            }
            case SyntaxKind::SlashEqualsToken: {
                result.operatorKind = ASTNodeKind::DivideAssignment;
                result.resultIsRValue = false;
                result.leftMustBeLValue = true;
                return result;
            }
            case SyntaxKind::PercentToken: {
                result.operatorKind = ASTNodeKind::Remainder;
                return result;
            }
            case SyntaxKind::PercentEqualsToken: {
                result.operatorKind = ASTNodeKind::RemainderAssignment;
                result.resultIsRValue = false;
                result.leftMustBeLValue = true;
                return result;
            }

            case SyntaxKind::LessLessToken: {
                result.operatorKind = ASTNodeKind::BitshiftLeft;
                return result;
            }
            case SyntaxKind::LessLessEqualsToken: {
                result.operatorKind = ASTNodeKind::BitshiftLeftAssignment;
                result.resultIsRValue = false;
                result.leftMustBeLValue = true;
                return result;
            }
            case SyntaxKind::GreaterGreaterToken: {
                result.operatorKind = ASTNodeKind::BitshiftRight;
                return result;
            }
            case SyntaxKind::GreaterGreaterEqualsToken: {
                result.operatorKind = ASTNodeKind::BitshiftRightAssignment;
                result.resultIsRValue = false;
                result.leftMustBeLValue = true;
                return result;
            }
            case SyntaxKind::HatToken: {
                result.operatorKind = ASTNodeKind::BitwiseXor;
                return result;
            }
            case SyntaxKind::HatEqualsToken: {
                result.operatorKind = ASTNodeKind::BitwiseXorAssignment;
                result.resultIsRValue = false;
                result.leftMustBeLValue = true;
                return result;
            }
            case SyntaxKind::AmpersandToken: {
                result.operatorKind = ASTNodeKind::BitwiseAnd;
                return result;
            }
            case SyntaxKind::AmpersandEqualsToken: {
                result.operatorKind = ASTNodeKind::BitwiseAndAssignment;
                result.resultIsRValue = false;
                result.leftMustBeLValue = true;
                return result;
            }
            case SyntaxKind::PipeToken: {
                result.operatorKind = ASTNodeKind::BitwiseOr;
                return result;
            }
            case SyntaxKind::PipeEqualsToken: {
                result.operatorKind = ASTNodeKind::BitwiseOrAssignment;
                result.resultIsRValue = false;
                result.leftMustBeLValue = true;
                return result;
            }

            case SyntaxKind::EqualsEqualsToken: {
                result.operatorKind = ASTNodeKind::Equals;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
            case SyntaxKind::BangEqualsToken: {
                result.operatorKind = ASTNodeKind::NotEquals;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
            case SyntaxKind::LessToken: {
                result.operatorKind = ASTNodeKind::Less;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
            case SyntaxKind::LessEqualsToken: {
                result.operatorKind = ASTNodeKind::LessEquals;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
            case SyntaxKind::GreaterToken: {
                result.operatorKind = ASTNodeKind::Greater;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
            case SyntaxKind::GreaterEqualsToken: {
                result.operatorKind = ASTNodeKind::GreaterEquals;
                result.resultType = TypeCreatePrimitive(TypeKind::PrimitiveBool);
                return result;
            }
        }
    }

    ReportError(
        token.location,
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