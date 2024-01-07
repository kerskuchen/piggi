#pragma once

#include "prelude.hpp"

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


////////////////////////////////////////////////////////////////////////////////////////////////////
// Source 

struct SourceFile {
    String filepath;
    String content;
};

struct SourceFileArray {
    SourceFile* files;
    int count;
    int capacity;
};

fun SourceFileArray SourceFileArrayCreate() {
    let SourceFileArray result;
    result.files = nullptr;
    result.count = 0;
    result.capacity = 0;
    return result;
}

fun void SourceFileArrayGrow(SourceFileArray* array, int newCapacity) {
    array->capacity = newCapacity;
    array->files = (as SourceFile*) realloc(array->files, newCapacity * sizeof(SourceFile));
    assert(array->files != nullptr);
}

fun int SourceFileArrayPush(SourceFileArray* array, SourceFile file) {
    if (array->count == array->capacity) {
        let int newCapacity = 2 * array->capacity;
        if (newCapacity == 0)
            newCapacity = 64;
        SourceFileArrayGrow(array, newCapacity);
    }
    let int insertionIndex = array->count;
    array->files[insertionIndex] = file;
    array->count += 1;
    return insertionIndex;
}

// TODO: Later we want to get rid of this source concept and parse each file separately like we did
// in our TS compiler. Currently this is hard to wrap our head around and pretty slow to index.
struct Source {
    SourceFileArray files;
    String content;
};

fun Source SourceCreateEmpty() {
    let Source result;
    result.files = SourceFileArrayCreate();
    result.content = StringCreateEmpty();
    return result;
}

fun bool SourceContainsFile(Source source, String filepath) {
    for (let int index = 0; index < source.files.count; index += 1) {
        if (StringEquals(source.files.files[index].filepath, filepath))
            return true;
    }
    return false;
}

fun String SourceGetSubstring(Source source, int start, int end) {
    return StringGetSubstring(source.content, start, end);
}

fun char SourceGetCharAtIndex(Source source, int charPos) {
    return source.content.cstr[charPos];
}

struct SourceLocation {
    Source source;
    int charPos;
    char ch;
    String filepath;
    LineAndColumnNumber fileLineColumn;
};


fun SourceLocation SourceGetLocationForCharPos(Source source, int charPos) {
    let SourceLocation result;
    result.source = source;
    result.charPos = charPos;
    result.ch = SourceGetCharAtIndex(source, charPos);
    result.filepath = StringCreateEmpty();
    result.fileLineColumn.lineNumber = -1;
    result.fileLineColumn.columnNumber = -1;

    let int remainder = charPos;
    for (let int fileIndex = 0; fileIndex < source.files.count; fileIndex += 1) {
        let String filepath = source.files.files[fileIndex].filepath;
        let String fileContent = source.files.files[fileIndex].content;
        let int fileLength = fileContent.length;
        if (remainder < fileLength) {
            result.filepath = filepath;
            result.fileLineColumn = StringGetLineAndColumnNumberAtPos(fileContent, remainder);
            result.ch = fileContent.cstr[remainder];
            break;
        }
        remainder -= fileLength;
    }

    return result;
}


struct SourceLocation2 {
    Source source;
    int start;
    int end;
};

fun SourceLocation2 SourceLocation2Create(Source source, int start, int end) {
    let SourceLocation2 result;
    result.source = source;
    result.start = start;
    result.end = end;
    return result;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Errors 

fun void ReportLocation(SourceLocation location) {
    fprintf(stderr, "%s(%d)", location.filepath.cstr, location.fileLineColumn.lineNumber);
}

fun void ReportError(SourceLocation location, char* format, ...) {
    ReportLocation(location);
    fprintf(stderr, ": Error - ", location.filepath.cstr, location.fileLineColumn.lineNumber);
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
    SourceLocation2 location;
};

fun SyntaxTrivia SyntaxTriviaCreate(SyntaxKind kind, SourceLocation2 location) {
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
    SyntaxNode* parent;
    SourceLocation2 location;

    SyntaxTriviaArray leadingTrivia;
    SyntaxTriviaArray trailingTrivia;

    longint intvalue;   // for integer literals
    bool intvalueIsHex; // makes the emit more readable
    String stringValueWithoutQuotes; // for char and string literals

    String debugString;
};

fun SyntaxToken SyntaxTokenCreateEmpty(Source source) {
    let SyntaxToken result;
    result.kind = SyntaxKind::BadToken;
    result.parent = nullptr;

    result.location.source = source;
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

        case SyntaxKind::LocalPersistKeyword:
            return StringCreateFromCStr("localpersist");
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

    if (StringEquals(identifier, TokenKindToString(SyntaxKind::FunKeyword)))
        return SyntaxKind::FunKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::LetKeyword)))
        return SyntaxKind::LetKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::StructKeyword)))
        return SyntaxKind::StructKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::UnionKeyword)))
        return SyntaxKind::UnionKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::EnumKeyword)))
        return SyntaxKind::EnumKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::ClassKeyword)))
        return SyntaxKind::ClassKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::AsKeyword)))
        return SyntaxKind::AsKeyword;
    if (StringEquals(identifier, TokenKindToString(SyntaxKind::SizeOfKeyword)))
        return SyntaxKind::SizeOfKeyword;

    if (StringEquals(identifier, TokenKindToString(SyntaxKind::LocalPersistKeyword)))
        return SyntaxKind::LocalPersistKeyword;
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

fun SourceLocation TokenGetLocation(SyntaxToken token) {
    return SourceGetLocationForCharPos(token.location.source, token.location.start);
}

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
    SyntaxKind kind;
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

    // Misc
    Identifier,

    // Statements
    CompoundStatement,
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
    Root,
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
    SyntaxToken token;
    ASTNode* left;
    ASTNode* right;
    ASTNode* extra1; // Used by 'while' statements
    ASTNode* extra2; // Used by 'for' statements

    bool isRValue;
    longint intvalue;     // For ASTNodeKind::Integerliteral and ASTNodeKind::IntegerliteralHex
    String stringvalue;      // For ASTNodeKind::Stringliteral
    Symbol* symbol;         // For variable/function declarations
    // For ASTNodeKind::CompoundStatement, ASTNodeKind::FunccallExpression arguments, 
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
