// deno-lint-ignore-file prefer-const

import { DiagnosticBag, Source, SourceLocation } from "./common.ts"
import { SyntaxKind, SyntaxTree, SyntaxToken, SyntaxTrivia, SyntaxFacts } from "./syntax.ts"

function IsDigit(c: string)
{
    if (c.length > 1)
        throw Error(`String '${c}' is not a single character`)
    return c.length == 1 && c >= '0' && c <= '9'
}
function IsWhiteSpace(c: string)
{
    if (c.length > 1)
        throw Error(`String '${c}' is not a single character`)
    return (c === ' ' || c === '\t')
}
function IsLetter(c: string)
{
    if (c.length > 1)
        throw Error(`String '${c}' is not a single character`)
    return (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))
}

export class Scanner
{
    source: Source
    pos = 0
    start = 0
    token: SyntaxToken

    debugToScan = ""
    debugAlreadyScanned = ""

    constructor(public tree: SyntaxTree)
    {
        this.source = tree.source
        this.token = SyntaxToken.CreateEmpty(tree)
    }

    NextToken(): SyntaxToken 
    {
        let leadingTrivia = this.ReadTrivia(true)

        this.ReadToken()
        let result = this.token

        let trailingTrivia = this.ReadTrivia(false)

        result.leadingTrivia = leadingTrivia
        result.trailingTrivia = trailingTrivia
        return result
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // Trivia

    private ReadTrivia(leading: boolean): SyntaxTrivia[]
    {
        let result = []

        let done = false
        while (!done) {
            this.start = this.pos
            this.token = SyntaxToken.CreateEmpty(this.tree)

            switch (this.Current()) {
                case '\0':
                    done = true
                    break
                case '/':
                    if (this.Lookahead() == '/')
                        this.ReadSingleLineComment()
                    else if (this.Lookahead() == '*')
                        this.ReadMultiLineComment()
                    else
                        done = true
                    break
                case '\n':
                case '\r':
                    if (!leading)
                        done = true
                    this.ReadLineBreak()
                    break
                case ' ':
                case '\t':
                    this.ReadWhiteSpace()
                    break
                default:
                    if (IsWhiteSpace(this.Current()))
                        this.ReadWhiteSpace()
                    else
                        done = true
                    break
            }

            if (this.pos > this.start) {
                let trivia = new SyntaxTrivia(this.token.kind, this.tree, this.GetLocation())
                result.push(trivia)
            }
        }
        return result
    }

    private ReadLineBreak()
    {
        this.token.kind = SyntaxKind.LineBreakTrivia
        if (this.Current() == '\r' && this.Lookahead() == '\n') {
            this.Advance()
            this.Advance()
        } else {
            this.Advance()
        }
    }

    private ReadWhiteSpace()
    {
        this.token.kind = SyntaxKind.WhitespaceTrivia
        while (true) {
            switch (this.Current()) {
                case '\0':
                case '\r':
                case '\n':
                    return
                default:
                    if (!IsWhiteSpace(this.Current()))
                        return
                    else
                        this.Advance()
                    break
            }
        }
    }


    private ReadSingleLineComment()
    {
        this.token.kind = SyntaxKind.SingleLineCommentTrivia
        this.Advance()
        this.Advance()

        while (true) {
            switch (this.Current()) {
                case '\0':
                case '\r':
                case '\n':
                    return
                default:
                    this.Advance()
            }
        }

    }

    private ReadMultiLineComment()
    {
        this.token.kind = SyntaxKind.MultiLineCommentTrivia
        this.Advance()
        this.Advance()

        while (true) {
            switch (this.Current()) {
                case '\0':
                    {
                        this.tree.diagnostics.ReportError(
                            this.GetLocation(),
                            "Unterminated multiline comment"
                        )
                        return
                    }
                case '*':
                    this.Advance()
                    if (this.Current() == '/') {
                        this.Advance()
                        return
                    }
                    break
                default:
                    this.Advance()
                    break
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // Tokens

    private ReadToken()
    {
        this.start = this.pos
        this.token = SyntaxToken.CreateEmpty(this.tree)

        switch (this.Current()) {
            case '\0':
                this.token.kind = SyntaxKind.EndOfFileToken
                break
            case '+':
                if (this.Lookahead() == '=') {
                    this.token.kind = SyntaxKind.PlusEqualsToken
                    this.Advance()
                    this.Advance()
                } else {
                    this.token.kind = SyntaxKind.PlusToken
                    this.Advance()
                }
                break
            case '-':
                if (this.Lookahead() == '=') {
                    this.token.kind = SyntaxKind.MinusEqualsToken
                    this.Advance()
                    this.Advance()
                } else if (this.Lookahead() == '>') {
                    this.token.kind = SyntaxKind.ArrowToken
                    this.Advance()
                    this.Advance()
                } else if (IsDigit(this.Lookahead())) {
                    this.ReadIntegerLiteral()
                } else {
                    this.token.kind = SyntaxKind.MinusToken
                    this.Advance()
                }
                break
            case '*':
                if (this.Lookahead() == '=') {
                    this.token.kind = SyntaxKind.StarEqualsToken
                    this.Advance()
                    this.Advance()
                } else {
                    this.token.kind = SyntaxKind.StarToken
                    this.Advance()
                }
                break
            case '/':
                if (this.Lookahead() == '=') {
                    this.token.kind = SyntaxKind.SlashEqualsToken
                    this.Advance()
                    this.Advance()
                } else {
                    this.token.kind = SyntaxKind.SlashToken
                    this.Advance()
                }
                break
            case '%':
                if (this.Lookahead() == '=') {
                    this.token.kind = SyntaxKind.PercentEqualsToken
                    this.Advance()
                    this.Advance()
                } else {
                    this.token.kind = SyntaxKind.PercentToken
                    this.Advance()
                }
                break
            case '.':
                if (this.Lookahead() == '.') {
                    this.token.kind = SyntaxKind.DotDotToken
                    this.Advance()
                    this.Advance()
                } else {
                    this.token.kind = SyntaxKind.DotToken
                    this.Advance()
                }
                break
            case ',':
                this.token.kind = SyntaxKind.CommaToken
                this.Advance()
                break
            case '?':
                this.token.kind = SyntaxKind.QuestionmarkToken
                this.Advance()
                break
            case ':':
                if (this.Lookahead() == ':') {
                    this.token.kind = SyntaxKind.ColonColonToken
                    this.Advance()
                    this.Advance()
                } else {
                    this.token.kind = SyntaxKind.ColonToken
                    this.Advance()
                }
                break
            case ';':
                this.token.kind = SyntaxKind.SemicolonToken
                this.Advance()
                break
            case '~':
                this.token.kind = SyntaxKind.TildeToken
                this.Advance()
                break
            case '^':
                if (this.Lookahead() == '=') {
                    this.token.kind = SyntaxKind.HatEqualsToken
                    this.Advance()
                    this.Advance()
                } else {
                    this.token.kind = SyntaxKind.HatToken
                    this.Advance()
                }
                break
            case '&':
                if (this.Lookahead() == '&') {
                    this.token.kind = SyntaxKind.AmpersandAmpersandToken
                    this.Advance()
                    this.Advance()
                } else if (this.Lookahead() == '=') {
                    this.token.kind = SyntaxKind.AmpersandEqualsToken
                    this.Advance()
                    this.Advance()
                } else {
                    this.token.kind = SyntaxKind.AmpersandToken
                    this.Advance()
                }
                break
            case '|':
                if (this.Lookahead() == '|') {
                    this.token.kind = SyntaxKind.PipePipeToken
                    this.Advance()
                    this.Advance()
                } else if (this.Lookahead() == '=') {
                    this.token.kind = SyntaxKind.PipeEqualsToken
                    this.Advance()
                    this.Advance()
                } else {
                    this.token.kind = SyntaxKind.PipeToken
                    this.Advance()
                }
                break
            case '{':
                this.token.kind = SyntaxKind.LeftBraceToken
                this.Advance()
                break
            case '}':
                this.token.kind = SyntaxKind.RightBraceToken
                this.Advance()
                break
            case '(':
                this.token.kind = SyntaxKind.LeftParenToken
                this.Advance()
                break
            case ')':
                this.token.kind = SyntaxKind.RightParenToken
                this.Advance()
                break
            case '[':
                this.token.kind = SyntaxKind.LeftBracketToken
                this.Advance()
                break
            case ']':
                this.token.kind = SyntaxKind.RightBracketToken
                this.Advance()
                break
            case '=':
                if (this.Lookahead() == '=') {
                    this.token.kind = SyntaxKind.EqualsEqualsToken
                    this.Advance()
                    this.Advance()
                } else {
                    this.token.kind = SyntaxKind.EqualsToken
                    this.Advance()
                }
                break
            case '!':
                if (this.Lookahead() == '=') {
                    this.token.kind = SyntaxKind.BangEqualsToken
                    this.Advance()
                    this.Advance()
                } else {
                    this.Advance()
                    this.token.kind = SyntaxKind.BangToken
                }
                break
            case '<':
                if (this.Lookahead() == '=') {
                    this.token.kind = SyntaxKind.LessEqualsToken
                    this.Advance()
                    this.Advance()
                } else if (this.Lookahead() == '<') {
                    this.token.kind = SyntaxKind.LessLessToken
                    this.Advance()
                    this.Advance()
                    if (this.Current() == '=') {
                        this.token.kind = SyntaxKind.LessLessEqualsToken
                        this.Advance()
                    }
                } else {
                    this.token.kind = SyntaxKind.LessToken
                    this.Advance()
                }
                break
            case '>':
                if (this.Lookahead() == '=') {
                    this.token.kind = SyntaxKind.GreaterEqualsToken
                    this.Advance()
                    this.Advance()
                } else if (this.Lookahead() == '>') {
                    this.token.kind = SyntaxKind.GreaterGreaterToken
                    this.Advance()
                    this.Advance()
                    if (this.Current() == '=') {
                        this.token.kind = SyntaxKind.GreaterGreaterEqualsToken
                        this.Advance()
                    }
                } else {
                    this.token.kind = SyntaxKind.GreaterToken
                    this.Advance()
                }
                break
            default: {
                let ch = this.Current()
                if (IsDigit(ch)) {
                    this.ReadIntegerLiteral()
                } else if (IsLetter(ch) || (ch == '_')) {
                    this.ReadIdentifierOrKeyword()
                } else if (ch == '\'') {
                    this.ReadCharacterLiteral()
                } else if (ch == '"') {
                    this.ReadStringLiteral()
                } else {
                    this.tree.diagnostics.ReportError(
                        this.GetLocation(),
                        `Unexpected character '${ch}'`,
                    )
                    this.Advance()
                }
            }
        }

        this.token.location = this.GetLocation()
        this.token.text = this.token.location.GetText()
    }

    private ReadIdentifierOrKeyword()
    {
        while (IsLetter(this.Current()) || IsDigit(this.Current()) || this.Current() == "_") {
            this.Advance()
        }
        let text = this.GetLocation().GetText()
        this.token.kind = SyntaxFacts.GetKeywordForIdentifier(text)
    }

    private ReadStringLiteral()
    {
        this.Advance() // Skip opening quote

        let start = this.pos
        while (this.Current() != '\0' && this.Current() != '\n') {
            let ch = this.Current()
            if (ch == '\\' && this.Lookahead() == '"') {
                this.Advance()
                this.Advance()
                continue
            }
            if (ch == '"')
                break
            this.Advance()
        }
        let end = this.pos

        let closingQuote = this.Current()
        if (closingQuote != '"')
            this.tree.diagnostics.ReportError(this.GetLocation(), "Unterminated string literal")
        else
            this.Advance()

        this.token.kind = SyntaxKind.StringLiteralToken
        this.token.stringValue = this.source.content.substring(start, end)
    }

    private ReadIntegerLiteral()
    {
        let isNegative = false
        if (this.Current() == '-') {
            isNegative = true
            this.Advance()
        }

        let radix = 10
        if (this.Current() == '0' && this.Lookahead() == 'x') {
            radix = 16
            this.Advance()
            this.Advance()
        }

        let intvalue = this.ReadPositiveIntegerLiteralWithRadix(radix)
        this.token.kind = SyntaxKind.IntegerLiteralToken
        this.token.intValueIsHex = radix == 16
        this.token.intValue = isNegative ? -intvalue : intvalue
    }

    private ReadCharacterLiteral()
    {
        this.Advance() // Skip opening quote

        let ch = this.ReadCharWithEscapeSequence()

        let closingQuote = this.Current()
        if (closingQuote != '\'')
            this.tree.diagnostics.ReportError(
                this.GetLocation(),
                `Expected closing quote \"'\" character after character literal but got '${closingQuote}`
            )
        else
            this.Advance()

        this.token.kind = SyntaxKind.CharacterLiteralToken
        this.token.stringValue = ch
    }

    private ReadCharWithEscapeSequence(): string
    {
        let start = this.pos
        let ch = this.Advance()
        if (ch == '\\') {
            let escape = this.Advance()
            switch (escape) {
                case 'b':
                    this.token.intValue = '\b'.charCodeAt(0)
                    return '\b' // backspace
                case 'f':
                    this.token.intValue = '\f'.charCodeAt(0)
                    return '\f' // form feed
                case 'n':
                    this.token.intValue = '\n'.charCodeAt(0)
                    return '\n' // line feed
                case 'r':
                    this.token.intValue = '\r'.charCodeAt(0)
                    return '\r' // carriage return
                case 't':
                    this.token.intValue = '\t'.charCodeAt(0)
                    return '\t' // horizontal tab
                case 'v':
                    this.token.intValue = '\v'.charCodeAt(0)
                    return '\v' // vertival tab
                case '\\':
                    this.token.intValue = '\\'.charCodeAt(0)
                    return '\\' // backslash
                case '"':
                    this.token.intValue = '\"'.charCodeAt(0)
                    return '"'  // double quote
                case '\'':
                    this.token.intValue = '\''.charCodeAt(0)
                    return '\'' // single quote
                case '0':
                    this.token.intValue = '\0'.charCodeAt(0)
                    return '\0' // zero terminator
                case 'x': {
                    let intValue = this.ReadPositiveIntegerLiteralWithRadix(16)
                    if (intValue > 255) {
                        this.tree.diagnostics.ReportError(
                            this.GetLocation(),
                            "Hexadecimal character literal cannot be bigger than '\\xFF'"
                        )
                        intValue = 255
                    }
                    let end = this.pos
                    this.token.intValue = intValue
                    return this.source.content.substring(start, end)
                }
                default:
                    this.tree.diagnostics.ReportError(
                        this.GetLocation(),
                        `Unknown escape sequence '\\${escape}'`
                    )
            }
        }
        return ch
    }

    private ReadPositiveIntegerLiteralWithRadix(radix: number): number
    {
        if ("0123456789ABCDEF".indexOf(this.Current().toUpperCase()) == -1) {
            this.tree.diagnostics.ReportError(
                this.GetLocation(),
                `Unexpected character in integer literal '${this.Current()}'`,
            )
        }

        let result = 0
        let digit = 0
        while (true) {
            digit = "0123456789ABCDEF".indexOf(this.Current().toUpperCase())
            if (digit == -1)
                break
            if (digit >= radix) {
                this.tree.diagnostics.ReportError(
                    this.GetLocation(),
                    `Invalid digit in integer literal '${this.Current()}'`
                )
            }
            result = result * radix + digit
            this.Advance()
        }

        return result
    }


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // Helpers 

    private Current(): string { return this.Peek(0) }
    private Lookahead(): string { return this.Peek(1) }
    private Peek(offset: number): string
    {
        let index = this.pos + offset
        if (index >= this.source.content.length)
            return "\0"
        else
            return this.source.content[index]
    }
    private Advance(): string
    {
        let result = this.Current()
        if (this.pos < this.source.content.length)
            this.pos += 1

        this.debugAlreadyScanned = this.source.content.substring(0, this.pos)
        this.debugToScan = this.source.content.substring(this.pos)

        return result
    }
    private GetLocation()
    {
        return SourceLocation.FromStartEnd(this.source, this.start, this.pos)
    }
}
