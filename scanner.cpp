#pragma once

#include "definitions.hpp"
// #if 0
// import "definitions.hpp"
// #endif

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
    SyntaxTree* tree;
    Source source;
    int start;
    int pos;
    SyntaxToken token;

    char debugCharPrev;
    char debugCharCur;
    char debugCharNext;
    char debugCharNextNext;
};

fun Scanner ScannerCreate(SyntaxTree* tree) {
    let Scanner result;
    result.tree = tree;
    result.source = tree->source;
    result.start = 0;
    result.pos = 0;
    result.token = SyntaxTokenCreateEmpty(tree);

    result.debugCharPrev = '\0';
    result.debugCharCur = '\0';
    result.debugCharNext = '\0';
    result.debugCharNextNext = '\0';

    return result;
}

//--------------------------------------------------------------------------------------------------
// Basics

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
    
    scanner->debugCharPrev = scanner->debugCharCur;
    scanner->debugCharCur = result;
    scanner->debugCharNext = PeekChar(scanner, 1);
    scanner->debugCharNextNext = PeekChar(scanner, 2);

    return result;
}

//--------------------------------------------------------------------------------------------------
// Trivia

fun void ReadLineBreak(Scanner* scanner)
{
    scanner->token.kind = SyntaxKind::LineBreakTrivia;
    if (CurrentChar(scanner) == '\r' && Lookahead(scanner) == '\n') {
        AdvanceChar(scanner);
        AdvanceChar(scanner);
    } else {
        AdvanceChar(scanner);
    }
}

fun void ReadWhiteSpace(Scanner* scanner)
{
    scanner->token.kind = SyntaxKind::WhitespaceTrivia;
    while (true) {
        switch (CurrentChar(scanner)) {
            case '\0':
            case '\r':
            case '\n':
                return;
            default:
                if (!IsWhiteSpace(CurrentChar(scanner)))
                    return;
                else
                    AdvanceChar(scanner);
        }
    }
}


fun void ReadSingleLineComment(Scanner* scanner)
{
    scanner->token.kind = SyntaxKind::SingleLineCommentTrivia;
    AdvanceChar(scanner);
    AdvanceChar(scanner);

    while (true) {
        switch (CurrentChar(scanner)) {
            case '\0':
            case '\r':
            case '\n':
                return;
            default:
                AdvanceChar(scanner);
        }
    }

}

fun void ReadMultiLineComment(Scanner* scanner)
{
    scanner->token.kind = SyntaxKind::MultiLineCommentTrivia;
    AdvanceChar(scanner);
    AdvanceChar(scanner);

    while (true) {
        switch (CurrentChar(scanner)) {
            case '\0':
            {
                let SourceLocation location = SourceGetLocationForCharPos(scanner->source, scanner->pos);
                // let SourceLocation2 location = SourceLocation2Create(scanner->source, scanner->start, 2);
                ReportError(location, "Unterminated multiline comment");
                return;
            }
            case '*':
            {
                AdvanceChar(scanner);
                if (CurrentChar(scanner) == '/') {
                    AdvanceChar(scanner);
                    return;
                }
                break;
            }
            default: 
            {
                AdvanceChar(scanner);
                break;
            }
        }
    }
}

fun SyntaxTriviaArray ReadTrivia(Scanner* scanner, bool isLeading)
{
    let SyntaxTriviaArray result = SyntaxTriviaArrayCreate();

    let bool done = false;
    while (!done) {
        scanner->start = scanner->pos;
        scanner->token = SyntaxTokenCreateEmpty(scanner->tree);

        switch (CurrentChar(scanner)) {
            case '\0':
                done = true;
                break;
            case '/':
                if (Lookahead(scanner) == '/')
                    ReadSingleLineComment(scanner);
                else if (Lookahead(scanner) == '*')
                    ReadMultiLineComment(scanner);
                else
                    done = true;
                break;
            case '\n':
            case '\r':
                if (!isLeading)
                    done = true;
                ReadLineBreak(scanner);
                break;
            case ' ':
            case '\t':
                ReadWhiteSpace(scanner);
                break;
            default:
                if (IsWhiteSpace(CurrentChar(scanner)))
                    ReadWhiteSpace(scanner);
                else
                    done = true;
                break;
        }

        if (scanner->pos > scanner->start) {
            let SourceLocation2 location = SourceLocation2Create(scanner->source, scanner->start, scanner->pos);
            let SyntaxTrivia trivia = SyntaxTriviaCreate(scanner->token.kind, location);
            SyntaxTriviaArrayPush(&result, trivia);
        }
    }
    return result;
}

//--------------------------------------------------------------------------------------------------
// Literals

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

fun void ReadIntegerLiteral(Scanner* scanner) {
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
    scanner->token.kind = SyntaxKind::IntegerLiteralToken;
    scanner->token.intvalueIsHex = radix == 16;
    scanner->token.intvalue = isNegative ? -intvalue : intvalue;
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

fun void ReadCharacterLiteral(Scanner* scanner) {
    let char openingQuote = AdvanceChar(scanner);
    let int start = scanner->pos;
    let char ch = ReadCharWithEscapeSequence(scanner);
    let int end = scanner->pos;
    let char closingQuote = AdvanceChar(scanner);
    if (closingQuote != '\'') {
        let SourceLocation location = SourceGetLocationForCharPos(scanner->source, scanner->start);
        ReportError(location, "Expected closing quote \"'\" character after character literal but got '%c'", closingQuote);
    }

    scanner->token.kind = SyntaxKind::CharacterLiteralToken;
    scanner->token.stringValueWithoutQuotes = SourceGetSubstring(scanner->source, start, end);
    scanner->token.intvalue = (as longint)ch;
}

fun void ReadStringLiteral(Scanner* scanner) {
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

    scanner->token.kind = SyntaxKind::StringLiteralToken;
    scanner->token.stringValueWithoutQuotes = SourceGetSubstring(scanner->source, start, end);
}

fun void ReadPreprocessorDirective(Scanner* scanner) {
    assert(CurrentChar(scanner) == '#');
    AdvanceChar(scanner);
    while (IsAlpha(CurrentChar(scanner)) || (CurrentChar(scanner) == '_')) {
        AdvanceChar(scanner);
    }

    let String identifier = SourceGetSubstring(scanner->source, scanner->start, scanner->pos); 
    let SyntaxKind keywordKind = GetKeywordForIdentifier(identifier);
    if (keywordKind != SyntaxKind::EndOfFileToken) {
        scanner->token.kind = keywordKind;
    } else {
        let SourceLocation location = SourceGetLocationForCharPos(scanner->source, scanner->start);
        ReportError(location, "Unknown preprocesser directive '%s'", identifier.cstr);
    }
}

fun void ReadIdentifierOrKeyword(Scanner* scanner) {
    while (IsAlpha(CurrentChar(scanner)) || IsDigit(CurrentChar(scanner)) || (CurrentChar(scanner) == '_')) {
        AdvanceChar(scanner);
    }
    let String identifier = SourceGetSubstring(scanner->source, scanner->start, scanner->pos); 
    let SyntaxKind keywordKind = GetKeywordForIdentifier(identifier);
    if (keywordKind != SyntaxKind::EndOfFileToken) {
        scanner->token.kind = keywordKind;
    } else {
        scanner->token.kind = SyntaxKind::IdentifierToken;
    }
}

//--------------------------------------------------------------------------------------------------
// Tokens

fun SyntaxToken ReadToken(Scanner* scanner) {
    scanner->start = scanner->pos;
    scanner->token = SyntaxTokenCreateEmpty(scanner->tree);

    let char ch = CurrentChar(scanner);
    switch (ch) {
        case '\0':
            scanner->token.kind = SyntaxKind::EndOfFileToken;
            break;
        case '+':
            if (Lookahead(scanner) == '=') {
                scanner->token.kind = SyntaxKind::PlusEqualsToken;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                scanner->token.kind = SyntaxKind::PlusToken;
                AdvanceChar(scanner);
            }
            break;
        case '-':
            if (Lookahead(scanner) == '=') {
                scanner->token.kind = SyntaxKind::MinusEqualsToken;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else if (Lookahead(scanner) == '>') {
                scanner->token.kind = SyntaxKind::ArrowToken;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else if (IsDigit(Lookahead(scanner))) {
                ReadIntegerLiteral(scanner);
            } else {
                scanner->token.kind = SyntaxKind::MinusToken;
                AdvanceChar(scanner);
            }
            break;
        case '*':
            if (Lookahead(scanner) == '=') {
                scanner->token.kind = SyntaxKind::StarEqualsToken;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                scanner->token.kind = SyntaxKind::StarToken;
                AdvanceChar(scanner);
            }
            break;
        case '/':
            if (Lookahead(scanner) == '=') {
                scanner->token.kind = SyntaxKind::SlashEqualsToken;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                scanner->token.kind = SyntaxKind::SlashToken;
                AdvanceChar(scanner);
            }
            break;
        case '%':
            if (Lookahead(scanner) == '=') {
                scanner->token.kind = SyntaxKind::PercentEqualsToken;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                scanner->token.kind = SyntaxKind::PercentToken;
                AdvanceChar(scanner);
            }
            break;
        case '.':
            if (PeekChar(scanner, 1) == '.' && PeekChar(scanner, 2)) {
                scanner->token.kind = SyntaxKind::DotDotDotToken;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                scanner->token.kind = SyntaxKind::DotToken;
                AdvanceChar(scanner);
            }
            break;
        case ',':
            scanner->token.kind = SyntaxKind::CommaToken;
            AdvanceChar(scanner);
            break;
        case '?':
            scanner->token.kind = SyntaxKind::QuestionmarkToken;
            AdvanceChar(scanner);
            break;
        case ':':
            if (Lookahead(scanner) == ':') {
                scanner->token.kind = SyntaxKind::ColonColonToken;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                scanner->token.kind = SyntaxKind::ColonToken;
                AdvanceChar(scanner);
            }
            break;
        case ';':
            scanner->token.kind = SyntaxKind::SemicolonToken;
            AdvanceChar(scanner);
            break;
        case '~':
            scanner->token.kind = SyntaxKind::TildeToken;
            AdvanceChar(scanner);
            break;
        case '^':
            if (Lookahead(scanner) == '=') {
                scanner->token.kind = SyntaxKind::HatEqualsToken;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                scanner->token.kind = SyntaxKind::HatToken;
                AdvanceChar(scanner);
            }
            break;
        case '&':
            if (Lookahead(scanner) == '&') {
                scanner->token.kind = SyntaxKind::AmpersandAmpersandToken;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else if (Lookahead(scanner) == '=') {
                scanner->token.kind = SyntaxKind::AmpersandEqualsToken;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                scanner->token.kind = SyntaxKind::AmpersandToken;
                AdvanceChar(scanner);
            }
            break;
        case '|':
            if (Lookahead(scanner) == '|') {
                scanner->token.kind = SyntaxKind::PipePipeToken;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else if (Lookahead(scanner) == '=') {
                scanner->token.kind = SyntaxKind::PipeEqualsToken;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                scanner->token.kind = SyntaxKind::PipeToken;
                AdvanceChar(scanner);
            }
            break;
        case '{':
            scanner->token.kind = SyntaxKind::LeftBraceToken;
            AdvanceChar(scanner);
            break;
        case '}':
            scanner->token.kind = SyntaxKind::RightBraceToken;
            AdvanceChar(scanner);
            break;
        case '(':
            scanner->token.kind = SyntaxKind::LeftParenToken;
            AdvanceChar(scanner);
            break;
        case ')':
            scanner->token.kind = SyntaxKind::RightParenToken;
            AdvanceChar(scanner);
            break;
        case '[':
            scanner->token.kind = SyntaxKind::LeftBracketToken;
            AdvanceChar(scanner);
            break;
        case ']':
            scanner->token.kind = SyntaxKind::RightBracketToken;
            AdvanceChar(scanner);
            break;
        case '=':
            if (Lookahead(scanner) == '=') {
                scanner->token.kind = SyntaxKind::EqualsEqualsToken;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                scanner->token.kind = SyntaxKind::EqualsToken;
                AdvanceChar(scanner);
            }
            break;
        case '!':
            if (Lookahead(scanner) == '=') {
                scanner->token.kind = SyntaxKind::BangEqualsToken;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else {
                AdvanceChar(scanner);
                scanner->token.kind = SyntaxKind::BangToken;
            }
            break;
        case '<':
            if (Lookahead(scanner) == '=') {
                scanner->token.kind = SyntaxKind::LessEqualsToken;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else if (Lookahead(scanner) == '<') {
                scanner->token.kind = SyntaxKind::LessLessToken;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
                if (CurrentChar(scanner) == '=') {
                    scanner->token.kind = SyntaxKind::LessLessEqualsToken;
                    AdvanceChar(scanner);
                }
            } else {
                scanner->token.kind = SyntaxKind::LessToken;
                AdvanceChar(scanner);
            }
            break;
        case '>':
            if (Lookahead(scanner) == '=') {
                scanner->token.kind = SyntaxKind::GreaterEqualsToken;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
            } else if (Lookahead(scanner) == '>') {
                scanner->token.kind = SyntaxKind::GreaterGreaterToken;
                AdvanceChar(scanner);
                AdvanceChar(scanner);
                if (CurrentChar(scanner) == '=') {
                    scanner->token.kind = SyntaxKind::GreaterGreaterEqualsToken;
                    AdvanceChar(scanner);
                }
            } else {
                scanner->token.kind = SyntaxKind::GreaterToken;
                AdvanceChar(scanner);
            }
            break;
        default:
            if (IsDigit(ch)) {
                ReadIntegerLiteral(scanner);
                break;
            } else if (IsAlpha(ch) || (ch == '_')) {
                ReadIdentifierOrKeyword(scanner);
                break;
            } else if (ch == '#') {
                ReadPreprocessorDirective(scanner);
                break;
            } else if (ch == '\'') {
                ReadCharacterLiteral(scanner);
                break;
            } else if (ch == '"') {
                ReadStringLiteral(scanner);
                break;
            }
            let SourceLocation location = SourceGetLocationForCharPos(scanner->source, scanner->start);
            ReportError(location, "Unexpected character '%c'", ch);
    }

    scanner->token.location.start = scanner->start;
    scanner->token.location.end = scanner->pos;
    scanner->token.debugString = SourceGetSubstring(scanner->source, scanner->start, scanner->pos);

    // debugstuff
    if (0) {
        fprintf(stdout, "Token %s", TokenKindToString(scanner->token.kind).cstr);
        if (scanner->token.kind == SyntaxKind::IntegerLiteralToken)
            fprintf(stdout, ", value '%lld'", scanner->token.intvalue);
        if (scanner->token.kind == SyntaxKind::CharacterLiteralToken)
            fprintf(stdout, ", value '%s'", scanner->token.stringValueWithoutQuotes.cstr);
        if (scanner->token.kind == SyntaxKind::StringLiteralToken)
            fprintf(stdout, ", value '%s'", scanner->token.stringValueWithoutQuotes.cstr);
        if (scanner->token.kind == SyntaxKind::IdentifierToken)
            fprintf(stdout, ", name '%s'", TokenGetText(scanner->token).cstr);
        fprintf(stdout, "\n");
    }

    return scanner->token;
}

fun SyntaxToken NextToken(Scanner* scanner) {
    let SyntaxTriviaArray leadingTrivia = ReadTrivia(scanner, true);
    let SyntaxToken result = ReadToken(scanner);
    let SyntaxTriviaArray trailingTrivia = ReadTrivia(scanner, false);

    result.leadingTrivia = leadingTrivia;
    result.trailingTrivia = trailingTrivia;

    // TODO: sum up all token lengths when reaching EOF and make sure that it matches exactly the file length
    return result;
}
