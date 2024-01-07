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
// Tokens 

enum class TokenKind {
    EndOfFile,

    // Basic Tokens
    Plus,
    PlusEquals,
    Minus,
    MinusEquals,
    Star,
    StarEquals,
    Slash,
    SlashEquals,
    Percent,
    PercentEquals,

    Equals,
    EqualsEquals,
    Bang,
    BangEquals,
    Less,
    LessEquals,
    Greater,
    GreaterEquals,

    LessLess,
    LessLessEquals,
    GreaterGreater,
    GreaterGreaterEquals,
    Tilde,
    Hat,
    HatEquals,
    Pipe,
    PipeEquals,
    PipePipe,
    Ampersand,
    AmpersandEquals,
    AmpersandAmpersand,

    Questionmark,
    Colon,
    ColonColon,
    Arrow,
    Dot,
    DotDotDot,
    Comma,
    Semicolon,
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,

    // Literals
    IntegerLiteral,
    CharacterLiteral,
    StringLiteral,

    // Keywords and identifiers
    Void,
    Char,
    Byte,
    Short,
    Int,
    Long,
    Null,
    CString,
    Bool,
    True,
    False,

    If,
    Else,
    Do,
    While,
    For,
    Return,
    Break,
    Continue,
    Switch,
    Case,
    Default,
    As,
    SizeOf,

    Fun,
    Let,
    Struct,
    Union,
    Enum,
    Class,

    LocalPersist,
    Extern,
    IncludeDirective,
    DefineDirective,
    IfDirective,
    EndIfDefinedDirective,
    PragmaDirective,
    Typedef,

    Identifier,
};

fun String TokenKindToString(TokenKind kind) {
    switch (kind) {
        case TokenKind::EndOfFile:
            return StringCreateFromCStr("eof");

        case TokenKind::Plus:
            return StringCreateFromCStr("+");
        case TokenKind::PlusEquals:
            return StringCreateFromCStr("+=");
        case TokenKind::Minus:
            return StringCreateFromCStr("-");
        case TokenKind::MinusEquals:
            return StringCreateFromCStr("-=");
        case TokenKind::Star:
            return StringCreateFromCStr("*");
        case TokenKind::StarEquals:
            return StringCreateFromCStr("*=");
        case TokenKind::Slash:
            return StringCreateFromCStr("/");
        case TokenKind::SlashEquals:
            return StringCreateFromCStr("/=");
        case TokenKind::Percent:
            return StringCreateFromCStr("%");
        case TokenKind::PercentEquals:
            return StringCreateFromCStr("%=");

        case TokenKind::Equals:
            return StringCreateFromCStr("=");
        case TokenKind::EqualsEquals:
            return StringCreateFromCStr("==");
        case TokenKind::Bang:
            return StringCreateFromCStr("!");
        case TokenKind::BangEquals:
            return StringCreateFromCStr("!=");
        case TokenKind::Less:
            return StringCreateFromCStr("<");
        case TokenKind::LessEquals:
            return StringCreateFromCStr("<=");
        case TokenKind::Greater:
            return StringCreateFromCStr(">");
        case TokenKind::GreaterEquals:
            return StringCreateFromCStr(">=");

        case TokenKind::LessLess:
            return StringCreateFromCStr("<<");
        case TokenKind::LessLessEquals:
            return StringCreateFromCStr("<<=");
        case TokenKind::GreaterGreater:
            return StringCreateFromCStr(">>");
        case TokenKind::GreaterGreaterEquals:
            return StringCreateFromCStr(">>=");
        case TokenKind::Tilde:
            return StringCreateFromCStr("~");
        case TokenKind::Hat:
            return StringCreateFromCStr("^");
        case TokenKind::HatEquals:
            return StringCreateFromCStr("^=");
        case TokenKind::Pipe:
            return StringCreateFromCStr("|");
        case TokenKind::PipeEquals:
            return StringCreateFromCStr("|=");
        case TokenKind::PipePipe:
            return StringCreateFromCStr("||");
        case TokenKind::Ampersand:
            return StringCreateFromCStr("&");
        case TokenKind::AmpersandEquals:
            return StringCreateFromCStr("&=");
        case TokenKind::AmpersandAmpersand:
            return StringCreateFromCStr("&&");

        case TokenKind::Questionmark:
            return StringCreateFromCStr("?");
        case TokenKind::Colon:
            return StringCreateFromCStr(":");
        case TokenKind::ColonColon:
            return StringCreateFromCStr("::");
        case TokenKind::Arrow:
            return StringCreateFromCStr("->");
        case TokenKind::Dot:
            return StringCreateFromCStr(".");
        case TokenKind::DotDotDot:
            return StringCreateFromCStr("...");
        case TokenKind::Comma:
            return StringCreateFromCStr(",");
        case TokenKind::Semicolon:
            return StringCreateFromCStr(";");
        case TokenKind::LeftBrace:
            return StringCreateFromCStr("{");
        case TokenKind::RightBrace:
            return StringCreateFromCStr("}");
        case TokenKind::LeftParen:
            return StringCreateFromCStr("(");
        case TokenKind::RightParen:
            return StringCreateFromCStr(")");
        case TokenKind::LeftBracket:
            return StringCreateFromCStr("[");
        case TokenKind::RightBracket:
            return StringCreateFromCStr("]");

        case TokenKind::IntegerLiteral:
            return StringCreateFromCStr("int-lit");
        case TokenKind::CharacterLiteral:
            return StringCreateFromCStr("chr-lit");
        case TokenKind::StringLiteral:
            return StringCreateFromCStr("str-lit");

        case TokenKind::Char:
            return StringCreateFromCStr("char");
        case TokenKind::Byte:
            return StringCreateFromCStr("byte");
        case TokenKind::Short:
            return StringCreateFromCStr("short");
        case TokenKind::Int:
            return StringCreateFromCStr("int");
        case TokenKind::Long:
            return StringCreateFromCStr("longint");
        case TokenKind::Void:
            return StringCreateFromCStr("void");
        case TokenKind::Null:
            return StringCreateFromCStr("nullptr");
        case TokenKind::CString:
            return StringCreateFromCStr("cstring");
        case TokenKind::Bool:
            return StringCreateFromCStr("bool");
        case TokenKind::True:
            return StringCreateFromCStr("true");
        case TokenKind::False:
            return StringCreateFromCStr("false");

        case TokenKind::If:
            return StringCreateFromCStr("if");
        case TokenKind::Else:
            return StringCreateFromCStr("else");
        case TokenKind::Do:
            return StringCreateFromCStr("do");
        case TokenKind::While:
            return StringCreateFromCStr("while");
        case TokenKind::For:
            return StringCreateFromCStr("for");
        case TokenKind::Return:
            return StringCreateFromCStr("return");
        case TokenKind::Break:
            return StringCreateFromCStr("break");
        case TokenKind::Continue:
            return StringCreateFromCStr("continue");
        case TokenKind::Switch:
            return StringCreateFromCStr("switch");
        case TokenKind::Case:
            return StringCreateFromCStr("case");
        case TokenKind::Default:
            return StringCreateFromCStr("default");
        case TokenKind::As:
            return StringCreateFromCStr("as");
        case TokenKind::SizeOf:
            return StringCreateFromCStr("sizeof");

        case TokenKind::Let:
            return StringCreateFromCStr("let");
        case TokenKind::Fun:
            return StringCreateFromCStr("fun");
        case TokenKind::Struct:
            return StringCreateFromCStr("struct");
        case TokenKind::Union:
            return StringCreateFromCStr("union");
        case TokenKind::Enum:
            return StringCreateFromCStr("enum");
        case TokenKind::Class:
            return StringCreateFromCStr("class");

        case TokenKind::LocalPersist:
            return StringCreateFromCStr("localpersist");
        case TokenKind::Extern:
            return StringCreateFromCStr("extern");
        case TokenKind::IncludeDirective:
            return StringCreateFromCStr("#include");
        case TokenKind::DefineDirective:
            return StringCreateFromCStr("#define");
        case TokenKind::IfDirective:
            return StringCreateFromCStr("#if");
        case TokenKind::EndIfDefinedDirective:
            return StringCreateFromCStr("#endif");
        case TokenKind::PragmaDirective:
            return StringCreateFromCStr("#pragma");
        case TokenKind::Typedef:
            return StringCreateFromCStr("typedef");

        case TokenKind::Identifier:
            return StringCreateFromCStr("identifier");
        
        default:
            fprintf(stderr, "Unrecognized token in `TokenKindToString`: %d", kind);
            exit(1);
    }
}

fun TokenKind GetKeywordForIdentifier(String identifier) {
    if (StringEquals(identifier, TokenKindToString(TokenKind::Char)))
        return TokenKind::Char;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Byte)))
        return TokenKind::Byte;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Short)))
        return TokenKind::Short;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Int)))
        return TokenKind::Int;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Long)))
        return TokenKind::Long;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Void)))
        return TokenKind::Void;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Null)))
        return TokenKind::Null;
    if (StringEquals(identifier, TokenKindToString(TokenKind::CString)))
        return TokenKind::CString;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Bool)))
        return TokenKind::Bool;
    if (StringEquals(identifier, TokenKindToString(TokenKind::True)))
        return TokenKind::True;
    if (StringEquals(identifier, TokenKindToString(TokenKind::False)))
        return TokenKind::False;

    if (StringEquals(identifier, TokenKindToString(TokenKind::If)))
        return TokenKind::If;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Else)))
        return TokenKind::Else;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Do)))
        return TokenKind::Do;
    if (StringEquals(identifier, TokenKindToString(TokenKind::While)))
        return TokenKind::While;
    if (StringEquals(identifier, TokenKindToString(TokenKind::For)))
        return TokenKind::For;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Return)))
        return TokenKind::Return;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Break)))
        return TokenKind::Break;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Continue)))
        return TokenKind::Continue;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Switch)))
        return TokenKind::Switch;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Case)))
        return TokenKind::Case;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Default)))
        return TokenKind::Default;

    if (StringEquals(identifier, TokenKindToString(TokenKind::Fun)))
        return TokenKind::Fun;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Let)))
        return TokenKind::Let;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Struct)))
        return TokenKind::Struct;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Union)))
        return TokenKind::Union;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Enum)))
        return TokenKind::Enum;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Class)))
        return TokenKind::Class;
    if (StringEquals(identifier, TokenKindToString(TokenKind::As)))
        return TokenKind::As;
    if (StringEquals(identifier, TokenKindToString(TokenKind::SizeOf)))
        return TokenKind::SizeOf;

    if (StringEquals(identifier, TokenKindToString(TokenKind::LocalPersist)))
        return TokenKind::LocalPersist;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Extern)))
        return TokenKind::Extern;
    if (StringEquals(identifier, TokenKindToString(TokenKind::IncludeDirective)))
        return TokenKind::IncludeDirective;
    if (StringEquals(identifier, TokenKindToString(TokenKind::DefineDirective)))
        return TokenKind::DefineDirective;
    if (StringEquals(identifier, TokenKindToString(TokenKind::IfDirective)))
        return TokenKind::IfDirective;
    if (StringEquals(identifier, TokenKindToString(TokenKind::EndIfDefinedDirective)))
        return TokenKind::EndIfDefinedDirective;
    if (StringEquals(identifier, TokenKindToString(TokenKind::PragmaDirective)))
        return TokenKind::PragmaDirective;
    if (StringEquals(identifier, TokenKindToString(TokenKind::Typedef)))
        return TokenKind::Typedef;

    return TokenKind::EndOfFile;
}

struct Token {
    TokenKind kind;
    longint intvalue;      // for integer literals
    bool intvalueIsHex; // makes the emit more readable
    String stringValue; // for char and string literals
    Source source;
    int sourceStart;
    int sourceEnd;
    String sourceString; // for debug purposes
};

fun Token TokenCreateEmpty(Source source) {
    let Token result;
    result.kind = TokenKind::EndOfFile;
    result.intvalue = 0;
    result.intvalueIsHex = false;
    result.stringValue = StringCreateEmpty();
    result.source = source;
    result.sourceStart = 0;
    result.sourceEnd = 0;
    result.sourceString = StringCreateEmpty();
    return result;
}

fun String TokenGetText(Token token) {
    return SourceGetSubstring(token.source, token.sourceStart, token.sourceEnd);
}

fun SourceLocation TokenGetLocation(Token token) {
    return SourceGetLocationForCharPos(token.source, token.sourceStart);
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
    Token token;
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

fun ASTNode* ASTNodeCreate(ASTNodeKind kind, SymbolTable* symbolTable, Token token) {
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
