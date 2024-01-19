// deno-lint-ignore-file prefer-const

import { Symbol, SymbolTable } from "./symbols.ts"
import { ImportDeclarationSyntax, SyntaxNode } from "./syntax.ts"
import { Type } from "./types.ts"

export enum BoundUnaryOperatorKind
{
    Identity = "Identity",
    Negation = "Negation",
    LogicalNegation = "LogicalNegation",
    BitwiseNegation = "BitwiseNegation",
}

export function BoundUnaryOperatorKindToString(kind: BoundUnaryOperatorKind): string
{
    switch (kind) {
        case BoundUnaryOperatorKind.Identity:
            return "+"
        case BoundUnaryOperatorKind.Negation:
            return "-"
        case BoundUnaryOperatorKind.LogicalNegation:
            return "!"
        case BoundUnaryOperatorKind.BitwiseNegation:
            return "~"

        default:
            throw new Error(`Unexpected unary operator kind: ${kind}`)
    }

}

export enum BoundBinaryOperatorKind
{
    Assignment = "Assignment",
    Add = "Add",
    AddAssignment = "AddAssignment",
    Subtract = "Subtract",
    SubtractAssignment = "SubtractAssignment",
    Multiply = "Multiply",
    MultiplyAssignment = "MultiplyAssignment",
    Divide = "Divide",
    DivideAssignment = "DivideAssignment",
    Remainder = "Remainder",
    RemainderAssignment = "RemainderAssignment",

    // Bits
    BitwiseXor = "BitwiseXor",
    BitwiseXorAssignment = "BitwiseXorAssignment",
    BitwiseAnd = "BitwiseAnd",
    BitwiseAndAssignment = "BitwiseAndAssignment",
    BitwiseOr = "BitwiseOr",
    BitwiseOrAssignment = "BitwiseOrAssignment",
    BitshiftLeft = "BitshiftLeft",
    BitshiftLeftAssignment = "BitshiftLeftAssignment",
    BitshiftRight = "BitshiftRight",
    BitshiftRightAssignment = "BitshiftRightAssignment",

    // Logical
    LogicalAnd = "LogicalAnd",
    LogicalOr = "LogicalOr",

    // Comparisons
    Equals = "Equals",
    NotEquals = "NotEquals",
    Less = "Less",
    LessEquals = "LessEquals",
    Greater = "Greater",
    GreaterEquals = "GreaterEquals",
}

export function BoundBinaryOperatorIsAssignment(kind: BoundBinaryOperatorKind): boolean
{
    switch (kind) {
        case BoundBinaryOperatorKind.Assignment:
        case BoundBinaryOperatorKind.AddAssignment:
        case BoundBinaryOperatorKind.SubtractAssignment:
        case BoundBinaryOperatorKind.MultiplyAssignment:
        case BoundBinaryOperatorKind.DivideAssignment:
        case BoundBinaryOperatorKind.RemainderAssignment:
        case BoundBinaryOperatorKind.BitwiseXorAssignment:
        case BoundBinaryOperatorKind.BitwiseAndAssignment:
        case BoundBinaryOperatorKind.BitwiseOrAssignment:
        case BoundBinaryOperatorKind.BitshiftLeftAssignment:
        case BoundBinaryOperatorKind.BitshiftRightAssignment:
            return true
        default:
            return false
    }

}

export function BoundBinaryOperatorKindToString(kind: BoundBinaryOperatorKind): string
{
    switch (kind) {
        case BoundBinaryOperatorKind.Assignment:
            return "="
        case BoundBinaryOperatorKind.Add:
            return "+"
        case BoundBinaryOperatorKind.AddAssignment:
            return "+="
        case BoundBinaryOperatorKind.Subtract:
            return "-"
        case BoundBinaryOperatorKind.SubtractAssignment:
            return "-="
        case BoundBinaryOperatorKind.Multiply:
            return "*"
        case BoundBinaryOperatorKind.MultiplyAssignment:
            return "*="
        case BoundBinaryOperatorKind.Divide:
            return "/"
        case BoundBinaryOperatorKind.DivideAssignment:
            return "/="
        case BoundBinaryOperatorKind.Remainder:
            return "%"
        case BoundBinaryOperatorKind.RemainderAssignment:
            return "%="

        case BoundBinaryOperatorKind.BitwiseXor:
            return "^"
        case BoundBinaryOperatorKind.BitwiseXorAssignment:
            return "^="
        case BoundBinaryOperatorKind.BitwiseAnd:
            return "&"
        case BoundBinaryOperatorKind.BitwiseAndAssignment:
            return "&="
        case BoundBinaryOperatorKind.BitwiseOr:
            return "|"
        case BoundBinaryOperatorKind.BitwiseOrAssignment:
            return "|="
        case BoundBinaryOperatorKind.BitshiftLeft:
            return "<<"
        case BoundBinaryOperatorKind.BitshiftLeftAssignment:
            return "<<="
        case BoundBinaryOperatorKind.BitshiftRight:
            return ">>"
        case BoundBinaryOperatorKind.BitshiftRightAssignment:
            return ">>="

        case BoundBinaryOperatorKind.LogicalAnd:
            return "&&"
        case BoundBinaryOperatorKind.LogicalOr:
            return "||"

        case BoundBinaryOperatorKind.Equals:
            return "=="
        case BoundBinaryOperatorKind.NotEquals:
            return "!="
        case BoundBinaryOperatorKind.Less:
            return "<"
        case BoundBinaryOperatorKind.LessEquals:
            return "<="
        case BoundBinaryOperatorKind.Greater:
            return ">"
        case BoundBinaryOperatorKind.GreaterEquals:
            return ">="

        default:
            throw new Error(`Unexpected unary operator kind: ${kind}`)
    }

}


export enum BoundNodeKind
{
    // Expressions
    MissingExpression = "MissingExpression",
    NameExpression = "NameExpression",
    ThisExpression = "ThisExpression",
    FunctionCallExpression = "FunctionCallExpression",
    ArrayIndexExpression = "ArrayIndexExpression",
    MemberAccessExpression = "MemberAccessExpression",
    TypeCastExpression = "TypeCastExpression",
    ParenthesizedExpression = "ParenthesizedExpression",
    UnaryExpression = "UnaryExpression",
    BinaryExpression = "BinaryExpression",
    TernaryConditionalExpression = "TernaryConditionalExpression",
    SizeOfExpression = "SizeOfExpression",
    TypeExpression = "TypeExpression",

    // Literals
    NullLiteral = "NullLiteral",
    NumberLiteral = "NumberLiteral",
    BoolLiteral = "BoolLiteral",
    StringLiteral = "StringLiteral",
    ArrayLiteral = "ArrayLiteral",

    // Statements
    MissingStatement = "MissingStatement",
    ExpressionStatement = "ExpressionStatement",
    BlockStatement = "BlockStatement",
    VariableDeclarationStatement = "VariableDeclarationStatement",

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

    // Contains all global function- and variable declarations
    CompilationUnit = "CompilationUnit",
}

export abstract class BoundNode
{
    constructor(
        public kind: BoundNodeKind,
        public syntax: SyntaxNode | null,
        public symbolTable: SymbolTable, // Can be the global table but can also be a local scope
    ) { }

    GetChildren(): BoundNode[]
    {
        let result: BoundNode[] = []
        let propertyNames = Object.keys(this)
        for (let key of propertyNames) {
            let child = this[key as keyof typeof this]
            if (child instanceof BoundNode) {
                result.push(child as BoundNode)
            } else if (child instanceof Array) {
                for (let elem of child) {
                    if (elem instanceof BoundNode)
                        result.push(elem as BoundNode)
                }
            }
        }
        return result
    }

    DumpTree(indent = "", isLast = true): string
    {
        let result = ""

        let nodeMarker = isLast ? "└──" : "├──"
        result += indent
        result += nodeMarker
        result += this.kind

        result += "\n"

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

////////////////////////////////////////////////////////////////////////////////////////////////////
// Compilation Unit

export class BoundCompilationUnit extends BoundNode
{
    constructor(
        symbolTable: SymbolTable,
        public importedModules: Map<string, ImportDeclarationSyntax>,
        public functions: Map<string, Symbol>,
        public enums: Map<string, Symbol>,
        public structs: Map<string, Symbol>,
        public globalVars: Map<string, Symbol>,
        public resolvedSortedGlobalVariableInitializers: Symbol[],
    ) { super(BoundNodeKind.CompilationUnit, null, symbolTable) }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Statements

export abstract class BoundStatement extends BoundNode
{
    private _dummyStmt = 0
    constructor(
        kind: BoundNodeKind,
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
    ) { super(kind, syntax, symbolTable) }
}

// Created by the binder only when an error occurred
export class BoundMissingStatement extends BoundStatement
{
    private _dummyMissing = 0
    constructor(
        syntax: SyntaxNode,
        symbolTable: SymbolTable,
    ) { super(BoundNodeKind.MissingStatement, syntax, symbolTable) }
}

export class BoundExpressionStatement extends BoundStatement
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public expression: BoundExpression,
    ) { super(BoundNodeKind.ExpressionStatement, syntax, symbolTable) }
}

export class BoundBlockStatement extends BoundStatement
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public statements: BoundStatement[]
    ) { super(BoundNodeKind.BlockStatement, syntax, symbolTable) }
}

export class BoundVariableDeclarationStatement extends BoundStatement
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public symbol: Symbol,
        public initializer: BoundExpression | null
    ) { super(BoundNodeKind.VariableDeclarationStatement, syntax, symbolTable) }
}

export class BoundIfStatement extends BoundStatement
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public condition: BoundExpression,
        public thenStatement: BoundStatement,
        public elseStatement: BoundStatement | null,
    ) { super(BoundNodeKind.IfStatement, syntax, symbolTable) }
}

export class BoundWhileStatement extends BoundStatement
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public condition: BoundExpression,
        public body: BoundStatement,
    ) { super(BoundNodeKind.WhileStatement, syntax, symbolTable) }
}

export class BoundDoWhileStatement extends BoundStatement
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public condition: BoundExpression,
        public body: BoundStatement,
    ) { super(BoundNodeKind.DoWhileStatement, syntax, symbolTable) }
}

export class BoundForStatement extends BoundStatement
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public iteratorSymbol: Symbol,
        public lowerBound: BoundExpression,
        public upperBound: BoundExpression,
        public upperBoundIsInclusive: boolean,
        public body: BoundStatement,
    ) { super(BoundNodeKind.ForStatement, syntax, symbolTable) }
}

export class BoundReturnStatement extends BoundStatement
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public returnExpression: BoundExpression | null,
    ) { super(BoundNodeKind.ReturnStatement, syntax, symbolTable) }
}

export class BoundBreakStatement extends BoundStatement
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
    ) { super(BoundNodeKind.BreakStatement, syntax, symbolTable) }
}

export class BoundContinueStatement extends BoundStatement
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
    ) { super(BoundNodeKind.ContinueStatement, syntax, symbolTable) }
}

export class BoundSwitchStatement extends BoundStatement
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public switchExpression: BoundExpression,
        public caseStatements: BoundCaseStatement[]
    ) { super(BoundNodeKind.SwitchStatement, syntax, symbolTable) }
}

export class BoundCaseStatement extends BoundStatement
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public caseExpression: BoundExpression | null,
        public body: BoundBlockStatement | null
    )
    {
        super(caseExpression == null
            ? BoundNodeKind.DefaultStatement
            : BoundNodeKind.CaseStatement,
            syntax, symbolTable)
    }
}


////////////////////////////////////////////////////////////////////////////////////////////////////
// Expressions

export abstract class BoundExpression extends BoundNode
{
    private _dummyExp = 0

    // TODO: nullable symbol is not good. we should remove this and put it in nodes that actually need it
    // then we need to think about how to make the callsites sane
    public symbol: Symbol | null = null
    constructor(
        kind: BoundNodeKind,
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public type: Type,
        public isRValue: boolean,
    ) { super(kind, syntax, symbolTable) }
}

// Created by the binder only when an error occurred
export class BoundMissingExpression extends BoundExpression
{
    private _dummyMissing = 0
    constructor(
        syntax: SyntaxNode,
        symbolTable: SymbolTable,
    )
    {
        super(
            BoundNodeKind.MissingExpression,
            syntax,
            symbolTable,
            Type.Any,
            true
        )
    }
}

export class BoundTypeCastExpression extends BoundExpression
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        type: Type,
        public expression: BoundExpression

    )
    {
        super(BoundNodeKind.TypeCastExpression, syntax, symbolTable, type, expression.isRValue)
        if (!expression.isRValue)
            this.symbol = expression.symbol
    }
}

export class BoundNameExpression extends BoundExpression
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        type: Type,
        symbol: Symbol,
    )
    {
        super(BoundNodeKind.NameExpression, syntax, symbolTable, type, false)
        this.symbol = symbol
    }
}

export class BoundThisExpression extends BoundExpression
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        containerSymbol: Symbol,
    )
    {
        super(BoundNodeKind.ThisExpression, syntax, symbolTable, containerSymbol.type, false)
        this.symbol = containerSymbol
    }
}


export class BoundParenthesizedExpression extends BoundExpression
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public inner: BoundExpression,
    )
    {
        super(BoundNodeKind.ParenthesizedExpression, syntax, symbolTable, inner.type, inner.isRValue)
        if (!inner.isRValue)
            this.symbol = inner.symbol
    }
}

export class BoundUnaryExpression extends BoundExpression
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public operator: BoundUnaryOperatorKind,
        public operand: BoundExpression,
        public resultType: Type,
        public resultIsRValue: boolean
    )
    {
        super(BoundNodeKind.UnaryExpression, syntax, symbolTable, resultType, resultIsRValue)
    }
}

export class BoundBinaryExpression extends BoundExpression
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public operator: BoundBinaryOperatorKind,
        public left: BoundExpression,
        public right: BoundExpression,
        public resultType: Type,
        public resultIsRValue: boolean
    )
    {
        super(BoundNodeKind.BinaryExpression, syntax, symbolTable, resultType, resultIsRValue)
    }
}

export class BoundTernaryConditionalExpression extends BoundExpression
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public condition: BoundExpression,
        public thenExpression: BoundExpression,
        public elseExpression: BoundExpression,
    )
    {
        super(BoundNodeKind.TernaryConditionalExpression, syntax, symbolTable, thenExpression.type, true)
    }
}

export class BoundFunctionCallExpression extends BoundExpression
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public left: BoundExpression,
        funcSym: Symbol,
        public isConstructor: boolean,
        public args: BoundExpression[],
    )
    {
        super(BoundNodeKind.FunctionCallExpression, syntax, symbolTable, funcSym.type, false)
        this.symbol = funcSym
    }
}

export class BoundArrayIndexExpression extends BoundExpression
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        elemType: Type,
        public array: BoundExpression,
        public index: BoundExpression,
    )
    {
        super(BoundNodeKind.ArrayIndexExpression, syntax, symbolTable, elemType, false)
    }
}

export class BoundMemberAccessExpression extends BoundExpression
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public container: BoundExpression,
        public memberSymbol: Symbol,
    )
    {
        super(BoundNodeKind.MemberAccessExpression, syntax, symbolTable, memberSymbol.type, false)
        this.symbol = memberSymbol
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Literals

export class BoundPrimitiveLiteral extends BoundExpression
{
    public boolValue: boolean | null = null
    public stringValue: string | null = null
    public numValue: number | null = null
    public numValueIsFloat: boolean | null = null
    public numValueIsHex: boolean | null = null
    constructor(
        kind: BoundNodeKind,
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        type: Type,
        public tokenText: string
    ) { super(kind, syntax, symbolTable, type, true) }
}

export class BoundArrayLiteral extends BoundExpression
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public elemType: Type,
        public values: BoundExpression[],
    ) { super(BoundNodeKind.ArrayLiteral, syntax, symbolTable, elemType, true) }
}