// deno-lint-ignore-file prefer-const

import { BoundBinaryOperator, BoundUnaryOperator } from "./operators.ts"
import { Symbol, SymbolTable } from "./symbols.ts"
import { SyntaxNode } from "./syntax.ts"
import { Type } from "./types.ts"

export enum BoundUnaryOperatorKind
{
    Identity = "Identity",
    Negation = "Negation",
    LogicalNegation = "LogicalNegation",
    BitwiseNegation = "BitwiseNegation",
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

    // Pointer math
    AddToPointer = "AddToPointer",
    AddToPointerAssignment = "AddToPointerAssignment",
    SubtractFromPointer = "SubtractFromPointer",
    SubtractFromPointerAssignment = "SubtractFromPointerAssignment",
    DistanceBetweenPointers = "DistanceBetweenPointers",

    // Comparisons
    Equals = "Equals",
    NotEquals = "NotEquals",
    Less = "Less",
    LessEquals = "LessEquals",
    Greater = "Greater",
    GreaterEquals = "GreaterEquals",
}

export enum BoundNodeKind
{
    // Expressions
    MissingExpression = "MissingExpression",
    NameExpression = "NameExpression",
    FunctionCallExpression = "FunccallExpression",
    ArrayIndexExpression = "Arrayindexing",
    MemberAccessExpression = "Memberaccess",
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
    EnumValueLiteral = "EnumValueLiteral",
    ArrayLiteral = "ArrayLiteral",

    // Statements
    MissingStatement = "MissingStatement",
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

    ImportDeclaration = "ImportDeclaration",
    EnumDeclaration = "EnumDeclaration",
    StructDeclaration = "StructDeclaration",
    FunctionDeclaration = "FunctionDeclaration",
    VariableDeclaration = "VariableDeclaration",
    ArrayDeclaration = "ArrayDeclaration",

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

export abstract class BoundStatement extends BoundNode
{
    private _dummyStmt = 0
    constructor(
        kind: BoundNodeKind,
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
    ) { super(kind, syntax, symbolTable) }
}

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

////////////////////////////////////////////////////////////////////////////////////////////////////
// Compilation Unit

export class BoundCompilationUnit extends BoundNode
{
    constructor(
        symbolTable: SymbolTable,
        public globalDeclarations: BoundStatement[]
    ) { super(BoundNodeKind.CompilationUnit, null, symbolTable) }
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

export class BoundImportDeclaration extends BoundStatement
{
    constructor(
        syntax: SyntaxNode,
        symbolTable: SymbolTable,
        public modulename: string
    ) { super(BoundNodeKind.ImportDeclaration, syntax, symbolTable) }
}

export class BoundStructDeclaration extends BoundStatement
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public symbol: Symbol,
        public isForwardDeclaration: boolean,
    )
    {
        super(
            BoundNodeKind.StructDeclaration,
            syntax,
            symbolTable
        )
    }
}

export class BoundEnumDeclaration extends BoundStatement
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public symbol: Symbol,
        public isForwardDeclaration: boolean,
    )
    {
        super(
            BoundNodeKind.EnumDeclaration,
            syntax,
            symbolTable
        )
    }
}

export class BoundFunctionDeclaration extends BoundStatement
{
    public isForwardDeclaration: boolean
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public symbol: Symbol,
        public body: BoundBlockStatement | null
    )
    {
        super(
            BoundNodeKind.FunctionDeclaration,
            syntax,
            symbolTable
        )
        this.isForwardDeclaration = body == null
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Statements

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

export class BoundVariableDeclaration extends BoundStatement
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public symbol: Symbol,
        public initializer: BoundExpression | null
    ) { super(BoundNodeKind.VariableDeclaration, syntax, symbolTable) }
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
        public operator: BoundUnaryOperator,
        public operand: BoundExpression,
    )
    {
        super(BoundNodeKind.UnaryExpression, syntax, symbolTable, operator.resultType, operator.resultIsRValue)
        if (operator.operandMustBeLValue && !operator.resultIsRValue)
            this.symbol = operand.symbol
    }
}

export class BoundBinaryExpression extends BoundExpression
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public operator: BoundBinaryOperator,
        public left: BoundExpression,
        public right: BoundExpression,
    )
    {
        super(BoundNodeKind.BinaryExpression, syntax, symbolTable, operator.resultType, operator.resultIsRValue)
        if (operator.leftMustBeLValue && !operator.resultIsRValue)
            this.symbol = left.symbol
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
        funcSym: Symbol,
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

export class BoundEnumValueLiteral extends BoundExpression
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public enumType: Type,
        public enumValueSymbol: Symbol,
    ) { super(BoundNodeKind.EnumValueLiteral, syntax, symbolTable, enumType, true) }
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