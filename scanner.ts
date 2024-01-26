// deno-lint-ignore-file prefer-const

import { Source, SourceLocation } from "./common.ts"
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

        let nestingLevel = 1
        while (nestingLevel > 0) {
            switch (this.Current()) {
                case '/':
                    this.Advance()
                    if (this.Current() == '*') {
                        this.Advance()
                        nestingLevel += 1
                    }
                    break
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
                        nestingLevel -= 1
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
                } else if (IsDigit(this.Lookahead())) {
                    this.ReadNumberLiteral()
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
                    this.ReadNumberLiteral()
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
                    this.ReadNumberLiteral()
                } else if (IsLetter(ch) || (ch == '_')) {
                    this.ReadIdentifierOrKeyword()
                } else if (ch == "`") {
                    this.ReadStringLiteral()
                } else if (ch == "'") {
                    this.ReadStringLiteral()
                } else if (ch == '"') {
                    this.ReadStringLiteral()
                } else {
                    this.Advance()
                    this.tree.diagnostics.ReportError(
                        this.GetLocation(),
                        `Unexpected character '${ch}'`,
                    )
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
        let startingQuote = this.Advance()

        let start = this.pos
        while (this.Current() != '\0' && this.Current() != '\n') {
            let ch = this.Current()
            if (ch == '\\' && this.Lookahead() == startingQuote) {
                this.Advance()
                this.Advance()
                continue
            }
            if (ch == startingQuote)
                break
            this.Advance()
        }
        let end = this.pos

        let closingQuote = this.Current()
        if (closingQuote != startingQuote)
            this.tree.diagnostics.ReportError(this.GetLocation(), "Unterminated string literal")
        else
            this.Advance()

        this.token.kind = SyntaxKind.StringLiteralToken
        this.token.stringValue = this.source.content.substring(start, end)
    }

    private ReadNumberLiteral()
    {
        if (this.Current() == '-') {
            this.Advance()
        } else if (this.Current() == '+') {
            this.Advance()
        }

        let isFloat = false
        let isHex = false
        if (this.Current() == '0' && this.Lookahead() == 'x') {
            isHex = true
            this.Advance()
            this.Advance()
            if (!"0123456789ABCDEF".includes(this.Current().toUpperCase())) {
                this.tree.diagnostics.ReportError(
                    this.GetLocation(),
                    `Expected hexadecimal number after '0x' prefix but got '${this.Current()}`
                )
                this.token.kind = SyntaxKind.BadToken
                this.token.numValueIsHex = false
                this.token.numValueIsFloat = false
                this.token.numValue = 0
                return
            }
            while ("0123456789ABCDEF".includes(this.Current().toUpperCase())) {
                this.Advance()
            }
        } else {
            while (IsDigit(this.Current())) {
                this.Advance()
            }
            if (this.Current() == "." && IsDigit(this.Lookahead())) {
                this.Advance()
                isFloat = true
                while (IsDigit(this.Current())) {
                    this.Advance()
                }
            }
        }

        let text = this.GetLocation().GetText()
        this.token.kind = SyntaxKind.NumberLiteralToken
        this.token.numValueIsHex = isHex
        this.token.numValueIsFloat = isFloat
        this.token.numValue = isFloat ? parseFloat(text) : parseInt(text)
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

        this.debugAlreadyScanned = this.source.content.substring(this.pos - 5, this.pos)
        this.debugToScan = this.source.content.substring(this.pos, this.pos + 5)

        return result
    }
    private GetLocation()
    {
        return SourceLocation.FromStartEnd(this.source, this.start, this.pos)
    }
}
