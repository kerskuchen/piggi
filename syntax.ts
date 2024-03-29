// deno-lint-ignore-file prefer-const

import { SourceLocation, DiagnosticBag, Source } from "./common.ts"

////////////////////////////////////////////////////////////////////////////////////////////////////

export enum SyntaxKind
{
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

    NumberLiteralToken = "NumberLiteralToken",
    StringLiteralToken = "StringLiteralToken",

    /////// Keywords and identifiers

    IdentifierToken = "IdentifierToken",

    // Primitive types
    NumberKeyword = "NumberKeyword",
    StringKeyword = "StringKeyword",
    NullKeyword = "NullKeyword",
    BoolKeyword = "BoolKeyword",
    TrueKeyword = "TrueKeyword",
    FalseKeyword = "FalseKeyword",
    AnyKeyword = "AnyKeyword",

    // Declarations
    LetKeyword = "LetKeyword",
    LetLocalPersistKeyword = "LetLocalPersistKeyword",
    FunKeyword = "FunKeyword",
    MetKeyword = "MetKeyword",
    StructKeyword = "StructKeyword",
    UnionKeyword = "UnionKeyword",
    EnumKeyword = "EnumKeyword",
    ClassKeyword = "ClassKeyword",
    ImportKeyword = "ImportKeyword",
    ImplKeyword = "ImplKeyword",

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
    InKeyword = "InKeyword",
    AsKeyword = "AsKeyword",
    ThisKeyword = "ThisKeyword",
    ExternKeyword = "ExternKeyword",
    StaticKeyword = "StaticKeyword",


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
    NullLiteral = "NullLiteral",
    NumberLiteral = "IntegerLiteral",
    BoolLiteral = "BoolLiteral",
    StringLiteral = "StringLiteral",
    ArrayLiteral = "ArrayLiteral",

    // Misc
    EnumMemberClauseSyntax = "EnumMemberClauseSyntax",
    FunctionParameterClauseSyntax = "FunctionParameterClauseSyntax",

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
    VariableDeclaration = "VariableDeclaration",

    // Module
    Module = "Module",
    ImportDeclaration = "ImportDeclaration",
    GlobalVariableDeclaration = "GlobalVariableDeclaration",
    EnumDeclaration = "EnumDeclaration",
    StructDeclaration = "StructDeclaration",
    FunctionDeclaration = "FunctionDeclaration",
    ImplDelcaration = "ImplDelcaration",
}

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

            case SyntaxKind.NumberLiteralToken:
                return "int-lit"
            case SyntaxKind.StringLiteralToken:
                return "str-lit"

            case SyntaxKind.NumberKeyword:
                return "number"
            case SyntaxKind.StringKeyword:
                return "string"
            case SyntaxKind.NullKeyword:
                return "null"
            case SyntaxKind.BoolKeyword:
                return "bool"
            case SyntaxKind.TrueKeyword:
                return "true"
            case SyntaxKind.FalseKeyword:
                return "false"
            case SyntaxKind.AnyKeyword:
                return "any"

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
            case SyntaxKind.InKeyword:
                return "in"
            case SyntaxKind.ThisKeyword:
                return "this"

            case SyntaxKind.LetKeyword:
                return "let"
            case SyntaxKind.LetLocalPersistKeyword:
                return "letpersist"
            case SyntaxKind.FunKeyword:
                return "fun"
            case SyntaxKind.MetKeyword:
                return "met"
            case SyntaxKind.StructKeyword:
                return "struct"
            case SyntaxKind.UnionKeyword:
                return "union"
            case SyntaxKind.EnumKeyword:
                return "enum"
            case SyntaxKind.ClassKeyword:
                return "class"
            case SyntaxKind.ImplKeyword:
                return "impl"

            case SyntaxKind.ImportKeyword:
                return "import"
            case SyntaxKind.ExternKeyword:
                return "extern"
            case SyntaxKind.StaticKeyword:
                return "static"

            case SyntaxKind.IdentifierToken:
                return "identifier"

            default:
                throw Error(`Unrecognized token given in 'TokenKindToString(..)': ${kind}`)
        }
    }

    static GetKeywordForIdentifier(identifier: string): SyntaxKind
    {
        switch (identifier) {
            case SyntaxFacts.TokenKindToString(SyntaxKind.NumberKeyword):
                return SyntaxKind.NumberKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.StringKeyword):
                return SyntaxKind.StringKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.NullKeyword):
                return SyntaxKind.NullKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.BoolKeyword):
                return SyntaxKind.BoolKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.TrueKeyword):
                return SyntaxKind.TrueKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.FalseKeyword):
                return SyntaxKind.FalseKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.AnyKeyword):
                return SyntaxKind.AnyKeyword

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
            case SyntaxFacts.TokenKindToString(SyntaxKind.InKeyword):
                return SyntaxKind.InKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.ThisKeyword):
                return SyntaxKind.ThisKeyword

            case SyntaxFacts.TokenKindToString(SyntaxKind.FunKeyword):
                return SyntaxKind.FunKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.MetKeyword):
                return SyntaxKind.MetKeyword
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
            case SyntaxFacts.TokenKindToString(SyntaxKind.ImplKeyword):
                return SyntaxKind.ImplKeyword

            case SyntaxFacts.TokenKindToString(SyntaxKind.ImportKeyword):
                return SyntaxKind.ImportKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.ExternKeyword):
                return SyntaxKind.ExternKeyword
            case SyntaxFacts.TokenKindToString(SyntaxKind.StaticKeyword):
                return SyntaxKind.StaticKeyword
        }

        return SyntaxKind.IdentifierToken
    }

    static GetUnaryOperatorPrecedence(kind: SyntaxKind): number
    {
        switch (kind) {
            case SyntaxKind.PlusToken:      // Identity
            case SyntaxKind.MinusToken:     // Negation
            case SyntaxKind.BangToken:      // Logical negation
            case SyntaxKind.TildeToken:     // Bitwise negation
                return 14
            default:
                return 0
        }
    }

    static IsBinaryOperatorRightAssociative(kind: SyntaxKind): boolean 
    {
        switch (kind) {
            case SyntaxKind.EqualsToken:
            case SyntaxKind.PlusEqualsToken:
            case SyntaxKind.MinusEqualsToken:
            case SyntaxKind.StarEqualsToken:
            case SyntaxKind.SlashEqualsToken:
            case SyntaxKind.PercentEqualsToken:
            case SyntaxKind.HatEqualsToken:
            case SyntaxKind.AmpersandEqualsToken:
            case SyntaxKind.PipeEqualsToken:
            case SyntaxKind.LessLessEqualsToken:
            case SyntaxKind.GreaterGreaterEqualsToken:
                return true

            default:
                return false
        }
    }

    static GetBinaryOperatorPrecedence(kind: SyntaxKind): number
    {
        switch (kind) {
            // TODO: Add exponentiation ** 
            // case SyntaxKind.StarStarToken:
            //    return 13

            case SyntaxKind.StarToken:
            case SyntaxKind.SlashToken:
            case SyntaxKind.PercentToken:
                return 12

            case SyntaxKind.PlusToken:
            case SyntaxKind.MinusToken:
                return 11

            // BitshiftIng
            case SyntaxKind.LessLessToken:
            case SyntaxKind.GreaterGreaterToken:
                return 10

            case SyntaxKind.LessToken:
            case SyntaxKind.LessEqualsToken:
            case SyntaxKind.GreaterToken:
            case SyntaxKind.GreaterEqualsToken:
                return 9

            case SyntaxKind.EqualsEqualsToken:
            case SyntaxKind.BangEqualsToken:
                return 8

            case SyntaxKind.AmpersandToken: // Bitwise AND
                return 7

            case SyntaxKind.HatToken:       // Bitwise XOR
                return 6

            case SyntaxKind.PipeToken:      // Bitwise OR
                return 5

            case SyntaxKind.AmpersandAmpersandToken: // Logical AND
                return 4

            case SyntaxKind.PipePipeToken: // Logical OR
                return 3

            // Ternary conditional (is handled in in the parser as special node)
            // case SyntaxKind.QuestionMarkToken:
            //  return 2

            // Assignment (is handled in in the parser as special node)
            // case SyntaxKind.EqualsToken: 
            // case SyntaxKind.PlusEqualsToken: 
            // case SyntaxKind.MinusEqualsToken: 
            // case SyntaxKind.StarEqualsToken: 
            // case SyntaxKind.SlashEqualsToken:
            // case SyntaxKind.PercentEqualsToken:
            // case SyntaxKind.HatEqualsToken:
            // case SyntaxKind.AmpersandEqualsToken:
            // case SyntaxKind.PipeEqualsToken:
            // case SyntaxKind.LessLessEqualsToken:
            // case SyntaxKind.GreaterGreaterEqualsToken:
            //      return 1;

            default:
                return 0
        }
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

    DumpTree(indent = "", isLast = true): string
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
            if (this.numValue !== null) {
                result += " "
                result += this.numValueIsHex ? "0x" + this.numValue.toString(16) : this.numValue
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
            result += child.DumpTree(indent, child === lastChild)

        return result
    }
}

export class SyntaxToken extends SyntaxNode
{
    // NOTE: A token is missing if it was inserted by the parser and doesn't appear in source.
    public numValue: number | null = null
    public numValueIsHex = false
    public numValueIsFloat = false
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
    }

    public IsMissing() { return this.text == null }

    static CreateEmpty(tree: SyntaxTree): SyntaxToken
    {
        return new SyntaxToken(SyntaxKind.BadToken, tree, SourceLocation.FromStartEnd(tree.source, 0, 0), null, [], [])
    }

    GetText(): string
    {
        if (this.text == null)
            return "<missing token>"
        else if (this.kind == SyntaxKind.EndOfFileToken)
            return "<EOF>"
        else if (this.text == "")
            throw new Error("Empty token text")
        else
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
    _root: ModuleSyntax | null = null
    _parents = new Map<SyntaxNode, SyntaxNode>()

    get root(): ModuleSyntax { return this._root! }
    get parents(): Map<SyntaxNode, SyntaxNode> { return this._parents! }

    constructor(public source: Source)
    {
    }

    AssignRoot(root: ModuleSyntax)
    {
        this._root = root
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

export abstract class ExpressionSyntax extends SyntaxNode
{
    private _expressionDummy = 0 // Or else derived classes of this can also pass as StatementSyntax
}
export abstract class StatementSyntax extends SyntaxNode
{
    private _statementDummy = 0 // Or else derived classes of this can also pass as ExpressionSyntax
}
export abstract class ModuleMemberSyntax extends SyntaxNode
{
    private _memberDummy = 0 // Or else derived classes of this can also pass as other non MemberSyntax
}

export class ModuleSyntax extends SyntaxNode
{
    constructor(
        syntaxTree: SyntaxTree,
        public moduleName: string,
        public members: ModuleMemberSyntax[],
        public endOfFileToken: SyntaxToken
    )
    {
        super(SyntaxKind.Module, syntaxTree)
    }
}


////////////////////////////////////////////////////////////////////////////////////////////////////
// Module

export class ImportDeclarationSyntax extends ModuleMemberSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public importKeyword: SyntaxToken,
        public modulenameIdent: SyntaxToken,
    )
    {
        super(SyntaxKind.ImportDeclaration, syntaxTree)
    }
}

export class GlobalVariableDeclarationSyntax extends ModuleMemberSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public externKeyword: SyntaxToken | null,
        public declaration: VariableDeclarationSyntax,
    )
    {
        super(SyntaxKind.GlobalVariableDeclaration, syntaxTree)
    }
}

export class EnumValueClauseSyntax extends SyntaxNode
{
    constructor(
        syntaxTree: SyntaxTree,
        public valueIdentifier: SyntaxToken,
        public equals: SyntaxNode | null,
        public integerLiteral: SyntaxToken | null,
        public comma: SyntaxNode | null,
    )
    {
        super(SyntaxKind.EnumMemberClauseSyntax, syntaxTree)
    }
}

export class EnumDeclarationSyntax extends ModuleMemberSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public externKeyword: SyntaxToken | null,
        public enumKeyword: SyntaxToken,
        public identifier: SyntaxToken,
        public leftBrace: SyntaxToken,
        public values: EnumValueClauseSyntax[],
        public rightBrace: SyntaxToken,
    )
    {
        super(SyntaxKind.EnumDeclaration, syntaxTree)
    }
}

export class StructDeclarationSyntax extends ModuleMemberSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public externKeyword: SyntaxToken | null,
        public structKeyword: SyntaxToken,
        public identifier: SyntaxToken,
        public leftBrace: SyntaxToken,
        public membersAndSeparators: SyntaxNode[],
        public rightBrace: SyntaxNode,
    )
    {
        super(SyntaxKind.StructDeclaration, syntaxTree)
    }
}

export class FunctionDeclarationSyntax extends ModuleMemberSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public externKeyword: SyntaxToken | null,
        public funOrMetKeyword: SyntaxToken,
        public identifier: SyntaxToken,
        public leftParen: SyntaxToken,
        public paramsAndSeparators: SyntaxNode[],
        public rightParen: SyntaxNode,
        public colon: SyntaxNode | null,
        public returnType: TypeExpressionSyntax | null,
        public body: BlockStatementSyntax | null,
    )
    {
        super(SyntaxKind.FunctionDeclaration, syntaxTree)
    }
}

export class ImplDeclarationSyntax extends ModuleMemberSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public externKeyword: SyntaxToken | null,
        public implKeyword: SyntaxToken,
        public implIdent: SyntaxToken,
        public leftBrace: SyntaxToken,
        public members: ModuleMemberSyntax[],
        public rightBrace: SyntaxNode,
    )
    {
        super(SyntaxKind.ImplDelcaration, syntaxTree)
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Statements

export class BlockStatementSyntax extends StatementSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public leftBrace: SyntaxToken | null,
        public statements: StatementSyntax[],
        public righBrace: SyntaxToken | null,
    )
    {
        super(SyntaxKind.BlockStatement, syntaxTree)
    }
}

export class ExpressionStatementSyntax extends StatementSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public expression: ExpressionSyntax,
    )
    {
        super(SyntaxKind.ExpressionStatement, syntaxTree)
    }
}

export class IfStatementSyntax extends StatementSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public ifKeyword: SyntaxToken,
        public condition: ExpressionSyntax,
        public thenStatement: StatementSyntax,
        public elseKeyword: SyntaxToken | null,
        public elseStatement: StatementSyntax | null,
    )
    {
        super(SyntaxKind.IfStatement, syntaxTree)
    }
}

export class DoWhileStatementSyntax extends StatementSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public doKeyword: SyntaxToken,
        public body: StatementSyntax,
        public whileKeyword: SyntaxToken,
        public condition: ExpressionSyntax,
    )
    {
        super(SyntaxKind.DoWhileStatement, syntaxTree)
    }
}

export class WhileStatementSyntax extends StatementSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public whileKeyword: SyntaxToken,
        public condition: ExpressionSyntax,
        public body: StatementSyntax,
    )
    {
        super(SyntaxKind.WhileStatement, syntaxTree)
    }
}

export class ForStatementSyntax extends StatementSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public forKeyword: SyntaxToken,
        public iteratorIdent: SyntaxToken,
        public inKeyword: SyntaxToken,
        public lowerBound: ExpressionSyntax,
        public dotdot: SyntaxToken,
        public equals: SyntaxToken | null,
        public upperBound: ExpressionSyntax,
        public body: StatementSyntax,
    )
    {
        super(SyntaxKind.ForStatement, syntaxTree)
    }
}

export class ReturnStatementSyntax extends StatementSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public returnKeyword: SyntaxToken,
        public returnExpression: ExpressionSyntax | null,
    )
    {
        super(SyntaxKind.ReturnStatement, syntaxTree)
    }
}

export class BreakStatementSyntax extends StatementSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public breakKeyword: SyntaxToken,
    )
    {
        super(SyntaxKind.BreakStatement, syntaxTree)
    }
}

export class ContinueStatementSyntax extends StatementSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public continueKeyword: SyntaxToken,
    )
    {
        super(SyntaxKind.ContinueStatement, syntaxTree)
    }
}

export class SwitchStatementSyntax extends StatementSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public switchKeyword: SyntaxToken,
        public switchExpression: ExpressionSyntax,
        public leftBrace: SyntaxToken,
        public caseStatements: CaseStatementSyntax[],
        public rightBrace: SyntaxToken,
    )
    {
        super(SyntaxKind.SwitchStatement, syntaxTree)
    }
}

export class CaseStatementSyntax extends StatementSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public caseOrDefaultKeyword: SyntaxToken,
        public caseExpression: ExpressionSyntax | null,
        public colon: SyntaxToken,
        public body: BlockStatementSyntax | null,
    )
    {
        let kind = caseOrDefaultKeyword.kind == SyntaxKind.DefaultKeyword
            ? SyntaxKind.DefaultKeyword
            : SyntaxKind.CaseStatement
        super(kind, syntaxTree)
    }
}

export class VariableDeclarationSyntax extends StatementSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public letKeyword: SyntaxToken | null,
        public identifier: SyntaxToken,
        public colon: SyntaxToken | null,
        public type: TypeExpressionSyntax | null,
        public equals: SyntaxToken | null,
        public initializer: ExpressionSyntax | null,
    )
    {
        super(SyntaxKind.VariableDeclaration, syntaxTree)
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Expressions

export class NameExpressionSyntax extends ExpressionSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public identifier: SyntaxToken,
    )
    {
        super(SyntaxKind.NameExpression, syntaxTree)
    }
}

export class UnaryExpressionSyntax extends ExpressionSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public operator: SyntaxToken,
        public operand: ExpressionSyntax,
    )
    {
        super(SyntaxKind.UnaryExpression, syntaxTree)
    }
}

export class BinaryExpressionSyntax extends ExpressionSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public left: ExpressionSyntax,
        public operator: SyntaxToken,
        public right: ExpressionSyntax,
    )
    {
        super(SyntaxKind.BinaryExpression, syntaxTree)
    }
}

export class ParenthesizedExpressionSyntax extends ExpressionSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public leftParen: SyntaxToken,
        public inner: ExpressionSyntax,
        public rightParen: SyntaxToken,
    )
    {
        super(SyntaxKind.ParenthesizedExpression, syntaxTree)
    }
}

export class FuncCallExpressionSyntax extends ExpressionSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public func: ExpressionSyntax,
        public leftParen: SyntaxToken,
        public argumentsWithSeparators: SyntaxNode[],
        public rightParen: SyntaxToken,
    )
    {
        super(SyntaxKind.FuncCallExpression, syntaxTree)
    }
}

export class ArrayIndexExpressionSyntax extends ExpressionSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public array: ExpressionSyntax,
        public leftBracket: SyntaxToken,
        public indexExpression: ExpressionSyntax,
        public rightBracket: SyntaxToken,
    )
    {
        super(SyntaxKind.ArrayIndexExpression, syntaxTree)
    }
}

export class MemberAccessExpressionSyntax extends ExpressionSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public container: ExpressionSyntax,
        public dot: SyntaxToken,
        public memberIdentifier: SyntaxToken,
    )
    {
        super(SyntaxKind.MemberAccessExpression, syntaxTree)
    }
}

export class TypeCastExpressionSyntax extends ExpressionSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public expression: ExpressionSyntax,
        public asKeyword: SyntaxToken,
        public targetType: TypeExpressionSyntax,
    )
    {
        super(SyntaxKind.TypeCastExpression, syntaxTree)
    }
}

export class TernaryConditionalExpressionSyntax extends ExpressionSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public condition: ExpressionSyntax,
        public questionmark: SyntaxToken,
        public thenExpression: ExpressionSyntax,
        public colon: SyntaxToken,
        public elseExpression: ExpressionSyntax,
    )
    {
        super(SyntaxKind.TernaryConditionalExpression, syntaxTree)
    }
}

export class SizeofExpressionSyntax extends ExpressionSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public sizeofKeyword: SyntaxToken,
        public leftParen: SyntaxToken,
        public typeExpression: TypeExpressionSyntax,
        public rightParen: SyntaxToken,
    )
    {
        super(SyntaxKind.SizeOfExpression, syntaxTree)
    }
}

export abstract class TypeExpressionSyntax extends ExpressionSyntax
{
}

export class BaseTypeExpressionSyntax extends ExpressionSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public typeIdentifier: SyntaxToken,
    )
    {
        super(SyntaxKind.TypeExpression, syntaxTree)
    }
}

export class NullableTypeExpressionSyntax extends ExpressionSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public baseType: TypeExpressionSyntax,
        public questionMark: SyntaxToken,
    )
    {
        super(SyntaxKind.TypeExpression, syntaxTree)
    }
}

export class ArrayTypeExpressionSyntax extends ExpressionSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public elemType: TypeExpressionSyntax,
        public leftBracket: SyntaxToken,
        public righBracket: SyntaxToken,
    )
    {
        super(SyntaxKind.TypeExpression, syntaxTree)
    }
}


////////////////////////////////////////////////////////////////////////////////////////////////////
// Literals

export class StringLiteralSyntax extends ExpressionSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public stringLiteral: SyntaxToken,
    )
    {
        super(SyntaxKind.StringLiteral, syntaxTree)
    }
}

export class NullLiteralSyntax extends ExpressionSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public nullLiteral: SyntaxToken,
    )
    {
        super(SyntaxKind.NullLiteral, syntaxTree)
    }
}

export class NumberLiteralSyntax extends ExpressionSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public numberLiteral: SyntaxToken,
    )
    {
        super(SyntaxKind.NumberLiteral, syntaxTree)
    }
}

export class BoolLiteralSyntax extends ExpressionSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public boolLiteral: SyntaxToken,
    )
    {
        super(SyntaxKind.BoolLiteral, syntaxTree)
    }
}

export class ArrayLiteralSyntax extends ExpressionSyntax
{
    constructor(
        syntaxTree: SyntaxTree,
        public leftBracket: SyntaxToken,
        public elemsWithSeparators: SyntaxNode[],
        public rightBracket: SyntaxToken,
    )
    {
        super(SyntaxKind.ArrayLiteral, syntaxTree)
    }
}