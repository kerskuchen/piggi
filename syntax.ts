// deno-lint-ignore-file prefer-const

import { SourceLocation, DiagnosticBag, Source } from "./common.ts"

////////////////////////////////////////////////////////////////////////////////////////////////////

export enum SyntaxKind
{
    MissingToken = "MissingToken",
    BadToken = "BadToken",

    // ---------------------------------------------------------------------------------------------
    // Trivia

    SkippedTextTrivia = "SkippedTextTrivia",
    LineBreakTrivia = "LineBreakTrivia",
    WhitespaceTrivia = "WhitespaceTrivia",
    SingleLineCommentTrivia = "SingleLineCommentTrivia",
    MultiLineCommentTrivia = "MultiLineCommentTrivia",

    // ---------------------------------------------------------------------------------------------
    // Tokens

    EndOfFileToken = "EndOfFileToken",

    /////// Unary operators

    BangToken = "BangToken",
    TildeToken = "TildeToken",

    /////// Binary operators

    PlusToken = "PlusToken",
    MinusToken = "MinusToken",
    StarToken = "StarToken",
    SlashToken = "SlashToken",
    PercentToken = "PercentToken",

    // Assignments
    EqualsToken = "EqualsToken",
    PlusEqualsToken = "PlusEqualsToken",
    MinusEqualsToken = "MinusEqualsToken",
    StarEqualsToken = "StarEqualsToken",
    SlashEqualsToken = "SlashEqualsToken",
    PercentEqualsToken = "PercentEqualsToken",
    LessLessEqualsToken = "LessLessEqualsToken",
    GreaterGreaterEqualsToken = "GreaterGreaterEqualsToken",
    HatEqualsToken = "HatEqualsToken",
    PipeEqualsToken = "PipeEqualsToken",
    AmpersandEqualsToken = "AmpersandEqualsToken",

    // Logical
    PipePipeToken = "PipePipeToken",
    AmpersandAmpersandToken = "AmpersandAmpersandToken",

    // Comparisons
    EqualsEqualsToken = "EqualsEqualsToken",
    BangEqualsToken = "BangEqualsToken",
    LessToken = "LessToken",
    LessEqualsToken = "LessEqualsToken",
    GreaterToken = "GreaterToken",
    GreaterEqualsToken = "GreaterEqualsToken",

    // Bits
    LessLessToken = "LessLessToken",
    GreaterGreaterToken = "GreaterGreaterToken",
    HatToken = "HatToken",
    PipeToken = "PipeToken",
    AmpersandToken = "AmpersandToken",

    /////// Punctuation
    QuestionmarkToken = "QuestionmarkToken",
    ColonToken = "ColonToken",
    ColonColonToken = "ColonColonToken",
    ArrowToken = "ArrowToken",
    DotToken = "DotToken",
    DotDotToken = "DotDotToken",
    CommaToken = "CommaToken",
    SemicolonToken = "SemicolonToken",
    LeftBraceToken = "LeftBraceToken",
    RightBraceToken = "RightBraceToken",
    LeftParenToken = "LeftParenToken",
    RightParenToken = "RightParenToken",
    LeftBracketToken = "LeftBracketToken",
    RightBracketToken = "RightBracketToken",

    /////// Literals
    IntegerLiteralToken = "IntegerLiteralToken",
    CharacterLiteralToken = "CharacterLiteralToken",
    StringLiteralToken = "StringLiteralToken",

    /////// Keywords and identifiers

    IdentifierToken = "IdentifierToken",

    // Primitive types
    VoidKeyword = "VoidKeyword",
    CharKeyword = "CharKeyword",
    ByteKeyword = "ByteKeyword",
    ShortKeyword = "ShortKeyword",
    IntKeyword = "IntKeyword",
    LongKeyword = "LongKeyword",
    NullKeyword = "NullKeyword",
    CStringKeyword = "CStringKeyword",
    BoolKeyword = "BoolKeyword",
    TrueKeyword = "TrueKeyword",
    FalseKeyword = "FalseKeyword",

    // Declarations
    LetKeyword = "LetKeyword",
    LetLocalPersistKeyword = "LetLocalPersistKeyword",
    FunKeyword = "FunKeyword",
    StructKeyword = "StructKeyword",
    UnionKeyword = "UnionKeyword",
    EnumKeyword = "EnumKeyword",
    ClassKeyword = "ClassKeyword",
    ImportKeyword = "ImportKeyword",

    // Control flow
    IfKeyword = "IfKeyword",
    ElseKeyword = "ElseKeyword",
    DoKeyword = "DoKeyword",
    WhileKeyword = "WhileKeyword",
    ForKeyword = "ForKeyword",
    ReturnKeyword = "ReturnKeyword",
    BreakKeyword = "BreakKeyword",
    ContinueKeyword = "ContinueKeyword",
    SwitchKeyword = "SwitchKeyword",
    CaseKeyword = "CaseKeyword",
    DefaultKeyword = "DefaultKeyword",

    // Misc
    AsKeyword = "AsKeyword",
    SizeOfKeyword = "SizeOfKeyword",

    // Storage location
    ExternKeyword = "ExternKeyword",


    // ---------------------------------------------------------------------------------------------
    // 

    // Expressions
    UnaryExpression = "UnaryExpression",
    BinaryExpression = "BinaryExpression",
    FuncCallExpression = "FuncCallExpression",
    ArrayIndexExpression = "ArrayIndexExpression",
    MemberAccessExpression = "MemberAccessExpression",
    TypeCastExpression = "TypeCastExpression",
    ParenthesizedExpression = "ParenthesizedExpression",
    TernaryConditionalExpression = "TernaryConditionalExpression",
    SizeOfExpression = "SizeOfExpression",
    NameExpression = "NameExpression",
    TypeExpression = "TypeExpression",

    // Literals
    NullLiteralExpression = "NullLiteralExpression",
    IntegerLiteralExpression = "IntegerLiteralExpression",
    CharacterLiteralExpression = "CharacterLiteralExpression",
    BoolLiteralExpression = "BoolLiteralExpression",
    StringLiteralExpression = "StringLiteralExpression",
    EnumValueLiteralExpression = "EnumValueLiteralExpression",
    ArrayLiteralExpression = "ArrayLiteralExpression",

    // Misc
    EnumMemberClauseSyntax = "EnumMemberClauseSyntax",

    // Statements
    BlockStatement = "BlockStatement",
    ExpressionStatement = "ExpressionStatement",

    IfStatement = "IfStatement",
    DoWhileStatement = "DoWhileStatement",
    WhileStatement = "WhileStatement",
    ForStatement = "ForStatement",
    ReturnStatement = "ReturnStatement",
    BreakStatement = "BreakStatement",
    ContinueStatement = "ContinueStatement",
    SwitchStatement = "SwitchStatement",
    CaseStatement = "CaseStatement",
    DefaultStatement = "DefaultStatement",
    VariableDeclarationStatement = "VariableDeclarationStatement",

    // Module
    Module = "Module",
    ImportDeclarationStatement = "ImportDeclarationStatement",
    GlobalVariableDeclarationStatement = "GlobalVariableDeclarationStatement",
    EnumDeclarationStatement = "EnumDeclarationStatement",
    EnumDefinitionStatement = "EnumDefinitionStatement",
    StructOrUnionDeclarationStatement = "StructOrUnionDeclarationStatement",
    StructOrUniontDefinitionStatement = "StructOrUniontDefinitionStatement",
    FunctionDeclarationStatement = "FunctionDeclarationStatement",
    FunctionDefinitionStatement = "FunctionDefinitionStatement",
};

export class SyntaxFacts
{
    static TokenKindToString(kind: SyntaxKind): string
    {
        switch (kind) {
            case SyntaxKind.EndOfFileToken:
                return "eof"

            case SyntaxKind.PlusToken:
                return "+"
            case SyntaxKind.PlusEqualsToken:
                return "+="
            case SyntaxKind.MinusToken:
                return "-"
            case SyntaxKind.MinusEqualsToken:
                return "-="
            case SyntaxKind.StarToken:
                return "*"
            case SyntaxKind.StarEqualsToken:
                return "*="
            case SyntaxKind.SlashToken:
                return "/"
            case SyntaxKind.SlashEqualsToken:
                return "/="
            case SyntaxKind.PercentToken:
                return "%"
            case SyntaxKind.PercentEqualsToken:
                return "%="

            case SyntaxKind.EqualsToken:
                return "="
            case SyntaxKind.EqualsEqualsToken:
                return "=="
            case SyntaxKind.BangToken:
                return "!"
            case SyntaxKind.BangEqualsToken:
                return "!="
            case SyntaxKind.LessToken:
                return "<"
            case SyntaxKind.LessEqualsToken:
                return "<="
            case SyntaxKind.GreaterToken:
                return ">"
            case SyntaxKind.GreaterEqualsToken:
                return ">="

            case SyntaxKind.LessLessToken:
                return "<<"
            case SyntaxKind.LessLessEqualsToken:
                return "<<="
            case SyntaxKind.GreaterGreaterToken:
                return ">>"
            case SyntaxKind.GreaterGreaterEqualsToken:
                return ">>="
            case SyntaxKind.TildeToken:
                return "~"
            case SyntaxKind.HatToken:
                return "^"
            case SyntaxKind.HatEqualsToken:
                return "^="
            case SyntaxKind.PipeToken:
                return "|"
            case SyntaxKind.PipeEqualsToken:
                return "|="
            case SyntaxKind.PipePipeToken:
                return "||"
            case SyntaxKind.AmpersandToken:
                return "&"
            case SyntaxKind.AmpersandEqualsToken:
                return "&="
            case SyntaxKind.AmpersandAmpersandToken:
                return "&&"

            case SyntaxKind.QuestionmarkToken:
                return "?"
            case SyntaxKind.ColonToken:
                return ":"
            case SyntaxKind.ColonColonToken:
                return "."
            case SyntaxKind.ArrowToken:
                return "->"
            case SyntaxKind.DotToken:
                return "."
            case SyntaxKind.DotDotToken:
                return ".."
            case SyntaxKind.CommaToken:
                return ","
            case SyntaxKind.SemicolonToken:
                return ";"
            case SyntaxKind.LeftBraceToken:
                return "{"
            case SyntaxKind.RightBraceToken:
                return "}"
            case SyntaxKind.LeftParenToken:
                return "("
            case SyntaxKind.RightParenToken:
                return ")"
            case SyntaxKind.LeftBracketToken:
                return "["
            case SyntaxKind.RightBracketToken:
                return "]"

            case SyntaxKind.IntegerLiteralToken:
                return "int-lit"
            case SyntaxKind.CharacterLiteralToken:
                return "chr-lit"
            case SyntaxKind.StringLiteralToken:
                return "str-lit"

            case SyntaxKind.CharKeyword:
                return "char"
            case SyntaxKind.ByteKeyword:
                return "byte"
            case SyntaxKind.ShortKeyword:
                return "short"
            case SyntaxKind.IntKeyword:
                return "int"
            case SyntaxKind.LongKeyword:
                return "longint"
            case SyntaxKind.VoidKeyword:
                return "void"
            case SyntaxKind.NullKeyword:
                return "nullptr"
            case SyntaxKind.CStringKeyword:
                return "cstring"
            case SyntaxKind.BoolKeyword:
                return "bool"
            case SyntaxKind.TrueKeyword:
                return "true"
            case SyntaxKind.FalseKeyword:
                return "false"

            case SyntaxKind.IfKeyword:
                return "if"
            case SyntaxKind.ElseKeyword:
                return "else"
            case SyntaxKind.DoKeyword:
                return "do"
            case SyntaxKind.WhileKeyword:
                return "while"
            case SyntaxKind.ForKeyword:
                return "for"
            case SyntaxKind.ReturnKeyword:
                return "return"
            case SyntaxKind.BreakKeyword:
                return "break"
            case SyntaxKind.ContinueKeyword:
                return "continue"
            case SyntaxKind.SwitchKeyword:
                return "switch"
            case SyntaxKind.CaseKeyword:
                return "case"
            case SyntaxKind.DefaultKeyword:
                return "default"
            case SyntaxKind.AsKeyword:
                return "as"
            case SyntaxKind.SizeOfKeyword:
                return "sizeof"

            case SyntaxKind.LetKeyword:
                return "let"
            case SyntaxKind.LetLocalPersistKeyword:
                return "letpersist"
            case SyntaxKind.FunKeyword:
                return "fun"
            case SyntaxKind.StructKeyword:
                return "struct"
            case SyntaxKind.UnionKeyword:
                return "union"
            case SyntaxKind.EnumKeyword:
                return "enum"
            case SyntaxKind.ClassKeyword:
                return "class"

            case SyntaxKind.ImportKeyword:
                return "import"
            case SyntaxKind.ExternKeyword:
                return "extern"

            case SyntaxKind.IdentifierToken:
                return "identifier"

            default:
                throw Error(`Unrecognized token given in 'TokenKindToString(..)': ${kind}`)
        }
    }

    static GetKeywordForIdentifier(identifier: string): SyntaxKind
    {
        switch (identifier) {
            case SyntaxFacts.TokenKindToString(SyntaxKind.CharKeyword):
                return SyntaxKind.CharKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.ByteKeyword):
                return SyntaxKind.ByteKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.ShortKeyword):
                return SyntaxKind.ShortKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.IntKeyword):
                return SyntaxKind.IntKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.LongKeyword):
                return SyntaxKind.LongKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.VoidKeyword):
                return SyntaxKind.VoidKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.NullKeyword):
                return SyntaxKind.NullKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.CStringKeyword):
                return SyntaxKind.CStringKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.BoolKeyword):
                return SyntaxKind.BoolKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.TrueKeyword):
                return SyntaxKind.TrueKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.FalseKeyword):
                return SyntaxKind.FalseKeyword

            case SyntaxFacts.TokenKindToString(SyntaxKind.IfKeyword):
                return SyntaxKind.IfKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.ElseKeyword):
                return SyntaxKind.ElseKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.DoKeyword):
                return SyntaxKind.DoKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.WhileKeyword):
                return SyntaxKind.WhileKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.ForKeyword):
                return SyntaxKind.ForKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.ReturnKeyword):
                return SyntaxKind.ReturnKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.BreakKeyword):
                return SyntaxKind.BreakKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.ContinueKeyword):
                return SyntaxKind.ContinueKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.SwitchKeyword):
                return SyntaxKind.SwitchKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.CaseKeyword):
                return SyntaxKind.CaseKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.DefaultKeyword):
                return SyntaxKind.DefaultKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.AsKeyword):
                return SyntaxKind.AsKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.SizeOfKeyword):
                return SyntaxKind.SizeOfKeyword

            case SyntaxFacts.TokenKindToString(SyntaxKind.FunKeyword):
                return SyntaxKind.FunKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.LetKeyword):
                return SyntaxKind.LetKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.LetLocalPersistKeyword):
                return SyntaxKind.LetLocalPersistKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.StructKeyword):
                return SyntaxKind.StructKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.UnionKeyword):
                return SyntaxKind.UnionKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.EnumKeyword):
                return SyntaxKind.EnumKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.ClassKeyword):
                return SyntaxKind.ClassKeyword

            case SyntaxFacts.TokenKindToString(SyntaxKind.ImportKeyword):
                return SyntaxKind.ImportKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.ExternKeyword):
                return SyntaxKind.ExternKeyword
        }

        return SyntaxKind.IdentifierToken
    }
}


////////////////////////////////////////////////////////////////////////////////////////////////////
// SyntaxNodes

export class SyntaxTrivia
{
    debugText: string

    constructor(
        public kind: SyntaxKind,
        public tree: SyntaxTree,
        public location: SourceLocation
    )
    {
        this.debugText = this.GetText()
    }

    GetText(): string
    {
        return this.location.GetText()
    }
}


export abstract class SyntaxNode
{
    get parent(): SyntaxNode | null { return this.tree.GetNodeParent(this) }

    constructor(
        public kind: SyntaxKind,
        public tree: SyntaxTree,
    ) { }

    GetLocation(): SourceLocation
    {
        // This assumes there is always at least a child node (SyntaxToken) in every node
        let children = this.GetChildren()
        let first = children[0].GetLocation()
        let last = children[children.length - 1].GetLocation()
        return SourceLocation.FromStartEnd(this.tree.source, first.start, last.end)
    }

    GetLocationIncludingTrivia(): SourceLocation 
    {
        let children = this.GetChildren()
        let first = children[0].GetLocationIncludingTrivia()
        let last = children[children.length - 1].GetLocationIncludingTrivia()
        return SourceLocation.FromStartEnd(this.tree.source, first.start, last.end)
    }

    GetChildren(): SyntaxNode[]
    {
        let result: SyntaxNode[] = []
        let propertyNames = Object.keys(this)
        for (let key of propertyNames) {
            let child = this[key as keyof typeof this]
            if (child instanceof SyntaxNode) {
                result.push(child as SyntaxNode)
            } else if (child instanceof Array) {
                for (let elem of child) {
                    if (elem instanceof SyntaxNode)
                        result.push(elem as SyntaxNode)
                }
            }
        }
        return result
    }

    PrettyPrint(indent = "", isLast = true): string
    {
        let result = ""

        let hasTrailingTrivia = false
        if (this instanceof SyntaxToken) {
            for (let trivia of this.leadingTrivia) {
                result += indent
                result += "├──"
                result += `L: ${trivia.kind}`
                if (trivia.kind == SyntaxKind.SkippedTextTrivia)
                    result += ` "${trivia.GetText()}"`
                result += "\n"
            }
            hasTrailingTrivia = this.trailingTrivia.length != 0
        }

        let tokenMarker = !hasTrailingTrivia && isLast ? "└──" : "├──"
        result += indent
        result += tokenMarker
        result += this.kind

        if (this instanceof SyntaxToken) {
            result += " "
            result += `'${this.GetText()}'`
            if (this.intValue !== null) {
                result += " "
                result += this.intValueIsHex ? "0x" + this.intValue.toString(16) : this.intValue
            }
            if (this.stringValue !== null) {
                result += " "
                result += this.stringValue
            }
        }

        result += "\n"
        if (this instanceof SyntaxToken) {
            let token = (this as SyntaxToken)
            for (let trivia of token.trailingTrivia) {
                let isLastTrailingTrivia = trivia == token.trailingTrivia[token.trailingTrivia.length - 1]
                let triviaMarker = isLast && isLastTrailingTrivia ? "└──" : "├──"

                result += indent
                result += triviaMarker

                result += `T: ${trivia.kind}\n`
                if (trivia.kind == SyntaxKind.SkippedTextTrivia)
                    result += `"${trivia.GetText()}"`
            }
        }

        indent += isLast ? "    " : "│  "

        let children = this.GetChildren()
        if (children.length == 0)
            return result

        let lastChild = children[children.length - 1]
        for (let child of this.GetChildren())
            result += child.PrettyPrint(indent, child === lastChild)

        return result
    }
}

export class SyntaxToken extends SyntaxNode
{
    // NOTE: A token is missing if it was inserted by the parser and doesn't appear in source.
    public isMissing: boolean
    public intValue: number | null = null
    public intValueIsHex = false
    public stringValue: string | null = null

    constructor(
        kind: SyntaxKind,
        tree: SyntaxTree,
        public location: SourceLocation,
        public text: string | null,
        public leadingTrivia: SyntaxTrivia[],
        public trailingTrivia: SyntaxTrivia[])
    {
        super(kind, tree)
        this.isMissing = text == null
    }

    static CreateEmpty(tree: SyntaxTree): SyntaxToken
    {
        return new SyntaxToken(SyntaxKind.BadToken, tree, SourceLocation.FromStartEnd(tree.source, 0, 0), null, [], [])
    }

    GetText(): string
    {
        return this.text!
    }

    GetLocation(): SourceLocation
    {
        return this.location
    }

    GetLocationIncludingTrivia(): SourceLocation
    {
        let start = this.leadingTrivia.length == 0
            ? this.location.start
            : this.leadingTrivia[0].location.start
        let end = this.trailingTrivia.length == 0
            ? this.location.end
            : this.trailingTrivia[this.trailingTrivia.length - 1].location.end
        return SourceLocation.FromStartEnd(this.tree.source, start, end)
    }
}

export class SyntaxTree
{
    diagnostics: DiagnosticBag = new DiagnosticBag()
    _root: SyntaxNode | null = null
    _parents = new Map<SyntaxNode, SyntaxNode>()

    get root(): SyntaxNode { return this._root! }
    get parents(): Map<SyntaxNode, SyntaxNode> { return this._parents! }

    constructor(public source: Source)
    {
    }

    AssignRoot(root: SyntaxNode, diagnostics: DiagnosticBag)
    {
        this._root = root
        this.diagnostics.Append(diagnostics)
        this.CreateParentsDictionary(this.root)
    }

    GetNodeParent(node: SyntaxNode): SyntaxNode | null
    {
        return this.parents.get(node) ?? null
    }

    CreateParentsDictionary(node: SyntaxNode)
    {
        for (let child of node.GetChildren()) {
            this.parents.set(child, node)
            this.CreateParentsDictionary(child)
        }
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Expressions

////////////////////////////////////////////////////////////////////////////////////////////////////
// Statements

////////////////////////////////////////////////////////////////////////////////////////////////////
// Module