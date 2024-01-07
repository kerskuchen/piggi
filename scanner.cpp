#include "definitions.hpp"
#include "syntax.hpp"

////////////////////////////////////////////////////////////////////////////////////////////////////
// Helpers

fun bool IsWhiteSpace(char ch) {
    return (ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n' || ch == '\f');
}

fun bool IsDigit(char ch) {
    return '0' <= ch && ch <= '9';
}

fun char ToLower(char ch) {
    if ('A' <= ch && ch <= 'Z')
        return ch + ('a' - 'A');
    else
        return ch;
}

fun char ToUpper(char ch) {
    if ('a' <= ch && ch <= 'z')
        return ch - ('a' - 'A');
    else
        return ch;
}

fun bool IsAlpha(char ch) {
    return ToLower(ch) != ToUpper(ch);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Scanner

struct Scanner {
    Source source;
    int start;
    int pos;
    Token token;

    char debugCurChar;
    char debugNextChar;
};

fun Scanner ScannerCreate(Source source) {
    let Scanner result;
    result.source = source;
    result.start = 0;
    result.pos = 0;
    result.token = TokenCreateEmpty(source);
    result.debugCurChar = '\0';
    result.debugNextChar = '\0';
    return result;
}

fun char PeekChar(Scanner* scanner, int offset) 
{
    let int index = scanner->pos + offset;
    return SourceGetCharAtIndex(scanner->source, index);
}
fun char CurrentChar(Scanner* scanner) { return PeekChar(scanner, 0); }
fun char Lookahead(Scanner* scanner) { return PeekChar(scanner, 1); }

fun char AdvanceChar(Scanner* scanner) {
    let char result = CurrentChar(scanner);
    scanner->pos += 1;
    
    scanner->debugCurChar = CurrentChar(scanner);
    scanner->debugNextChar = Lookahead(scanner);

    return result;
}

fun void SkipWhitespaceTrivia(Scanner* scanner) {
    while (IsWhiteSpace(CurrentChar(scanner))) {
        AdvanceChar(scanner);
    }
}

fun void SkipSingleLineComment(Scanner* scanner) {
    while (CurrentChar(scanner) != '\0') {
        let char ch = AdvanceChar(scanner);
        if (ch == '\n')
            break;
    }
}

fun void SkipMultiLineComment(Scanner* scanner)
{
    let SourceLocation location = SourceGetLocationForCharPos(scanner->source, scanner->pos);
    while (true) {
        if (CurrentChar(scanner) == '\0') {
            ReportError(location, "Unterminated multiline comment");
        }
        if ((CurrentChar(scanner) == '*') && (Lookahead(scanner) == '/')) {
            AdvanceChar(scanner);
            AdvanceChar(scanner);
            return;
        }
        AdvanceChar(scanner);
    }
}

fun void SkipTrivia(Scanner* scanner) {
    while (true) {
        if (IsWhiteSpace(CurrentChar(scanner)))
            SkipWhitespaceTrivia(scanner);
        else if ((CurrentChar(scanner) == '/') && (Lookahead(scanner) == '/'))
            SkipSingleLineComment(scanner);
        else if ((CurrentChar(scanner) == '/') && (Lookahead(scanner) == '*'))
            SkipMultiLineComment(scanner);
        else
            break;
    }
}

fun longint ReadPositiveIntegerLiteralWithRadix(Scanner* scanner, int radix) {
    if (FindCharPosInString(StringCreateFromCStr("0123456789ABCDEF"), ToUpper(CurrentChar(scanner))) == -1) {
        let SourceLocation location = SourceGetLocationForCharPos(scanner->source, scanner->pos);
        ReportError(location, "Unexpected character in integer literal '%c'", CurrentChar(scanner));
    }

    let longint result = 0;
    let int digit;
    while (true) {
        digit = FindCharPosInString(StringCreateFromCStr("0123456789ABCDEF"), ToUpper(CurrentChar(scanner)));
        if (digit == -1)
            break;
        if (digit >= radix) {
            let SourceLocation location = SourceGetLocationForCharPos(scanner->source, scanner->pos);
            ReportError(location, "Invalid digit in integer literal '%c'", CurrentChar(scanner));
        }

        result = result * radix + digit;
        AdvanceChar(scanner);
    }

    return result;
}

fun Token ReadIntegerLiteral(Scanner* scanner) {
    let bool isNegative = false;
    if (CurrentChar(scanner) == '-') {
        isNegative = true;
        AdvanceChar(scanner);
    }

    let int radix = 10;
    if (CurrentChar(scanner) == '0' && Lookahead(scanner) == 'x'){
        radix = 16;
        AdvanceChar(scanner);
        AdvanceChar(scanner);
    }

    let longint intvalue = ReadPositiveIntegerLiteralWithRadix(scanner, radix);

    let Token result = TokenCreateEmpty(scanner->source);
    result.kind = TokenKind::IntegerLiteral;
    result.intvalueIsHex = radix == 16;
    result.intvalue = isNegative ? -intvalue : intvalue;
    return result;
}

fun char ReadCharWithEscapeSequence(Scanner* scanner) {
    let char ch = AdvanceChar(scanner);
    if (ch == '\\') {
        let char escape = AdvanceChar(scanner);
        switch (escape) {
            case 'a': 
                return '\a';
            case 'b': 
                return '\b';
            case 'f': 
                return '\f';
            case 'n': 
                return '\n';
            case 'r': 
                return '\r';
            case 't': 
                return '\t';
            case 'v': 
                return '\v';
            case '\\': 
                return '\\';
            case '"': 
                return '"';
            case '\'': 
                return '\'';
            case '0': 
                return '0';
            case 'x': {
                let longint number = ReadPositiveIntegerLiteralWithRadix(scanner, 16);
                if (number > UCHAR_MAX) {
                    let SourceLocation location = SourceGetLocationForCharPos(scanner->source, scanner->pos);
                    ReportError(location, "Hexadecimal character literal cannot be bigger than '\\xFF'");
                }
                return (as char)number;
            }
            default:
                let SourceLocation location = SourceGetLocationForCharPos(scanner->source, scanner->pos);
                ReportError(location, "Unknown escape sequence '\\%c'", escape);
        }
    }
    return ch;
}

fun Token ReadCharacterLiteral(Scanner* scanner) {
    let char openingQuote = AdvanceChar(scanner);
    let int start = scanner->pos;
    let char ch = ReadCharWithEscapeSequence(scanner);
    let int end = scanner->pos;
    let char closingQuote = AdvanceChar(scanner);
    if (closingQuote != '\'') {
        let SourceLocation location = SourceGetLocationForCharPos(scanner->source, scanner->start);
        ReportError(location, "Expected closing quote \"'\" character after character literal but got '%c'", closingQuote);
    }

    let Token result = TokenCreateEmpty(scanner->source);
    result.kind = TokenKind::CharacterLiteral;
    result.stringValue = SourceGetSubstring(scanner->source, start, end);
    result.intvalue = (as longint)ch;
    return result;
}

fun Token ReadStringLiteral(Scanner* scanner) {
    let char openingQuote = AdvanceChar(scanner);
    let int start = scanner->pos;
    while (CurrentChar(scanner) != '\0' && CurrentChar(scanner) != '\n') {
        let char ch = CurrentChar(scanner);
        if (ch == '\\' && Lookahead(scanner) == '"') {
            AdvanceChar(scanner);
            AdvanceChar(scanner);
            continue;
        }
        if (ch == '"')
            break;
        AdvanceChar(scanner);
    }
    let int end = scanner->pos;
    let char closingQuote = AdvanceChar(scanner);
    if (closingQuote != '\"') {
        let SourceLocation location = SourceGetLocationForCharPos(scanner->source, scanner->start);
        ReportError(location, "Unterminated string literal");
    }

    let Token result = TokenCreateEmpty(scanner->source);
    result.kind = TokenKind::StringLiteral;
    result.stringValue = SourceGetSubstring(scanner->source, start, end);
    return result;
}

fun void ReadPreprocessorDirective(Scanner* scanner) {
    assert(CurrentChar(scanner) == '#');
    AdvanceChar(scanner);
    while (IsAlpha(CurrentChar(scanner)) || (CurrentChar(scanner) == '_')) {
        AdvanceChar(scanner);
    }
}

fun void ReadIdentifier(Scanner* scanner) {
    while (IsAlpha(CurrentChar(scanner)) || IsDigit(CurrentChar(scanner)) || (CurrentChar(scanner) == '_')) {
        AdvanceChar(scanner);
    }
}

fun void ReadToken(Scanner* scanner) {
    scanner->start = scanner->pos;
    scanner->token = TokenCreateEmpty(scanner->source);

    let char ch = CurrentChar(scanner);
    switch (ch) {
        case '\0':
            scanner->token.kind = TokenKind::EndOfFile;
            break;
        case '+':
            if (Lookahead(scanner) == '=') {
                scanner->token.kind = TokenKind::PlusEquals;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                scanner->token.kind = TokenKind::Plus;
                AdvanceChar(scanner);
            }
            break;
        case '-':
            if (Lookahead(scanner) == '=') {
                scanner->token.kind = TokenKind::MinusEquals;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else if (Lookahead(scanner) == '>') {
                scanner->token.kind = TokenKind::Arrow;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else if (IsDigit(Lookahead(scanner))) {
                scanner->token = ReadIntegerLiteral(scanner);
            } else {
                scanner->token.kind = TokenKind::Minus;
                AdvanceChar(scanner);
            }
            break;
        case '*':
            if (Lookahead(scanner) == '=') {
                scanner->token.kind = TokenKind::StarEquals;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                scanner->token.kind = TokenKind::Star;
                AdvanceChar(scanner);
            }
            break;
        case '/':
            if (Lookahead(scanner) == '=') {
                scanner->token.kind = TokenKind::SlashEquals;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                scanner->token.kind = TokenKind::Slash;
                AdvanceChar(scanner);
            }
            break;
        case '%':
            if (Lookahead(scanner) == '=') {
                scanner->token.kind = TokenKind::PercentEquals;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                scanner->token.kind = TokenKind::Percent;
                AdvanceChar(scanner);
            }
            break;
        case '.':
            if (PeekChar(scanner, 1) == '.' && PeekChar(scanner, 2)) {
                scanner->token.kind = TokenKind::DotDotDot;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                scanner->token.kind = TokenKind::Dot;
                AdvanceChar(scanner);
            }
            break;
        case ',':
            scanner->token.kind = TokenKind::Comma;
            AdvanceChar(scanner);
            break;
        case '?':
            scanner->token.kind = TokenKind::Questionmark;
            AdvanceChar(scanner);
            break;
        case ':':
            if (Lookahead(scanner) == ':') {
                scanner->token.kind = TokenKind::ColonColon;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                scanner->token.kind = TokenKind::Colon;
                AdvanceChar(scanner);
            }
            break;
        case ';':
            scanner->token.kind = TokenKind::Semicolon;
            AdvanceChar(scanner);
            break;
        case '~':
            scanner->token.kind = TokenKind::Tilde;
            AdvanceChar(scanner);
            break;
        case '^':
            if (Lookahead(scanner) == '=') {
                scanner->token.kind = TokenKind::HatEquals;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                scanner->token.kind = TokenKind::Hat;
                AdvanceChar(scanner);
            }
            break;
        case '&':
            if (Lookahead(scanner) == '&') {
                scanner->token.kind = TokenKind::AmpersandAmpersand;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else if (Lookahead(scanner) == '=') {
                scanner->token.kind = TokenKind::AmpersandEquals;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                scanner->token.kind = TokenKind::Ampersand;
                AdvanceChar(scanner);
            }
            break;
        case '|':
            if (Lookahead(scanner) == '|') {
                scanner->token.kind = TokenKind::PipePipe;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else if (Lookahead(scanner) == '=') {
                scanner->token.kind = TokenKind::PipeEquals;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                scanner->token.kind = TokenKind::Pipe;
                AdvanceChar(scanner);
            }
            break;
        case '{':
            scanner->token.kind = TokenKind::LeftBrace;
            AdvanceChar(scanner);
            break;
        case '}':
            scanner->token.kind = TokenKind::RightBrace;
            AdvanceChar(scanner);
            break;
        case '(':
            scanner->token.kind = TokenKind::LeftParen;
            AdvanceChar(scanner);
            break;
        case ')':
            scanner->token.kind = TokenKind::RightParen;
            AdvanceChar(scanner);
            break;
        case '[':
            scanner->token.kind = TokenKind::LeftBracket;
            AdvanceChar(scanner);
            break;
        case ']':
            scanner->token.kind = TokenKind::RightBracket;
            AdvanceChar(scanner);
            break;
        case '=':
            if (Lookahead(scanner) == '=') {
                scanner->token.kind = TokenKind::EqualsEquals;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                scanner->token.kind = TokenKind::Equals;
                AdvanceChar(scanner);
            }
            break;
        case '!':
            if (Lookahead(scanner) == '=') {
                scanner->token.kind = TokenKind::BangEquals;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                AdvanceChar(scanner);
                scanner->token.kind = TokenKind::Bang;
            }
            break;
        case '<':
            if (Lookahead(scanner) == '=') {
                scanner->token.kind = TokenKind::LessEquals;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else if (Lookahead(scanner) == '<') {
                scanner->token.kind = TokenKind::LessLess;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
                if (CurrentChar(scanner) == '=') {
                    scanner->token.kind = TokenKind::LessLessEquals;
                    AdvanceChar(scanner);
                }
            } else {
                scanner->token.kind = TokenKind::Less;
                AdvanceChar(scanner);
            }
            break;
        case '>':
            if (Lookahead(scanner) == '=') {
                scanner->token.kind = TokenKind::GreaterEquals;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else if (Lookahead(scanner) == '>') {
                scanner->token.kind = TokenKind::GreaterGreater;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
                if (CurrentChar(scanner) == '=') {
                    scanner->token.kind = TokenKind::GreaterGreaterEquals;
                    AdvanceChar(scanner);
                }
            } else {
                scanner->token.kind = TokenKind::Greater;
                AdvanceChar(scanner);
            }
            break;
        default:
            if (IsDigit(ch)) {
                scanner->token = ReadIntegerLiteral(scanner);
                break;
            } else if (IsAlpha(ch) || (ch == '_')) {
                ReadIdentifier(scanner);
                let String identifier = SourceGetSubstring(scanner->source, scanner->start, scanner->pos); 
                let TokenKind keywordKind = GetKeywordForIdentifier(identifier);
                if (keywordKind != TokenKind::EndOfFile) {
                    scanner->token.kind = keywordKind;
                } else {
                    scanner->token.kind = TokenKind::Identifier;
                }
                break;
            } else if (ch == '#') {
                ReadPreprocessorDirective(scanner);
                let String identifier = SourceGetSubstring(scanner->source, scanner->start, scanner->pos); 
                let TokenKind keywordKind = GetKeywordForIdentifier(identifier);
                if (keywordKind != TokenKind::EndOfFile) {
                    scanner->token.kind = keywordKind;
                    break;
                } 
                // Fallthrough to error
            } else if (ch == '\'') {
                scanner->token = ReadCharacterLiteral(scanner);
                break;
            } else if (ch == '"') {
                scanner->token = ReadStringLiteral(scanner);
                break;
            }
            let SourceLocation location = SourceGetLocationForCharPos(scanner->source, scanner->start);
            ReportError(location, "Unexpected character '%c'", ch);
    }

    scanner->token.sourceStart = scanner->start;
    scanner->token.sourceEnd = scanner->pos;
    scanner->token.sourceString = SourceGetSubstring(scanner->source, scanner->start, scanner->pos);

    // debugstuff
    if (0) {
        fprintf(stdout, "Token %s", TokenKindToString(scanner->token.kind).cstr);
        if (scanner->token.kind == TokenKind::IntegerLiteral)
            fprintf(stdout, ", value '%lld'", scanner->token.intvalue);
        if (scanner->token.kind == TokenKind::CharacterLiteral)
            fprintf(stdout, ", value '%s'", scanner->token.stringValue.cstr);
        if (scanner->token.kind == TokenKind::StringLiteral)
            fprintf(stdout, ", value '%s'", scanner->token.stringValue.cstr);
        if (scanner->token.kind == TokenKind::Identifier)
            fprintf(stdout, ", name '%s'", TokenGetText(scanner->token).cstr);
        fprintf(stdout, "\n");
    }
}

fun Token NextToken(Scanner* scanner) {
    SkipTrivia(scanner);
    ReadToken(scanner);
    return scanner->token;
}
