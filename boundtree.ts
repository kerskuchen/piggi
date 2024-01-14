// deno-lint-ignore-file prefer-const

import { BoundBinaryOperator, BoundUnaryOperator } from "./binder.ts"
import { Symbol, SymbolTable } from "./old/symbols.ts"
import { SyntaxNode } from "./syntax.ts"
import { BasicTypeKind, Type } from "./types.ts"

export enum BoundUnaryOperatorKind
{
    Identity = "Identity",
    Negation = "Negation",
    LogicalNegation = "LogicalNegation",
    BitwiseNegation = "BitwiseNegation",
    Dereference = "Dereference",
    Address = "Address",
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
    NameExpression = "NameExpression",
    FunccallExpression = "FunccallExpression",
    Arrayindexing = "Arrayindexing",
    Memberaccess = "Memberaccess",
    TypeCastExpression = "TypeCastExpression",
    ParenthesizedExpression = "ParenthesizedExpression",
    UnaryExpression = "UnaryExpression",
    BinaryExpression = "BinaryExpression",
    TernaryConditionalExpression = "TernaryConditionalExpression",
    SizeOfExpression = "SizeOfExpression",
    TypeExpression = "TypeExpression",

    // Literals
    NullLiteral = "NullLiteral",
    IntegerLiteral = "IntegerLiteral",
    CharacterLiteral = "CharacterLiteral",
    BoolLiteral = "BoolLiteral",
    StringLiteral = "StringLiteral",
    EnumValueLiteral = "EnumValueLiteral",
    ArrayLiteral = "ArrayLiteral",

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

    EnumDeclarationStatement = "EnumDeclarationStatement",
    EnumDefinitionStatement = "EnumDefinitionStatement",
    StructDeclarationStatement = "StructDeclarationStatement",
    StructDefinitionStatement = "StructDefinitionStatement",
    UnionDeclarationStatement = "UnionDeclarationStatement",
    UnionDefinitionStatement = "UnionDefinitionStatement",
    FunctionDeclarationStatement = "FunctionDeclarationStatement",
    FunctionDefinitionStatement = "FunctionDefinitionStatement",
    VariableDeclarationStatement = "VariableDeclarationStatement",
    ArrayDeclarationStatement = "ArrayDeclarationStatement",

    // Contains global function- and variable declarations
    Module = "Module",
}

export abstract class BoundNode
{
    constructor(
        public kind: BoundNodeKind,
        public syntax: SyntaxNode | null,
        public symbolTable: SymbolTable, // Can be the global table but can also be a local scope
    ) { }
}

export abstract class BoundExpression extends BoundNode
{
    private _dummyExp = 0

    public symbol: Symbol | null = null
    constructor(
        kind: BoundNodeKind,
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public type: Type,
        public isRValue: boolean,
    ) { super(kind, syntax, symbolTable) }
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


////////////////////////////////////////////////////////////////////////////////////////////////////
// Literals

export class BoundPrimitiveLiteral extends BoundExpression
{
    constructor(
        kind: BoundNodeKind,
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        type: Type,
    ) { super(kind, syntax, symbolTable, type, true) }
}

export class BoundEnumValueLiteral extends BoundExpression
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        type: Type,
        public value: number,
    ) { super(BoundNodeKind.EnumValueLiteral, syntax, symbolTable, type, true) }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Expressions

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

export class BoundSizeofExpression extends BoundExpression
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public typeExpression: Type
    ) { super(BoundNodeKind.TypeCastExpression, syntax, symbolTable, Type.FromPrimitive(BasicTypeKind.Int), true) }
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
        type: Type,
        public inner: BoundExpression,
    )
    {
        super(BoundNodeKind.NameExpression, syntax, symbolTable, type, inner.isRValue)
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
        super(BoundNodeKind.UnaryExpression, syntax, symbolTable, operator.resultType, operator.resultIsRValue)
        if (operator.leftMustBeLValue && !operator.resultIsRValue)
            this.symbol = left.symbol
    }
}

export class BoundTernaryConditionalExpression extends BoundExpression
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        type: Type,
        public condition: BoundExpression,
        public thenExpression: BoundExpression,
        public elseExpression: BoundExpression,
    )
    {
        super(BoundNodeKind.UnaryExpression, syntax, symbolTable, type, true)
    }
}


export class BoundFunctionCallExpression extends BoundExpression
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        func: Symbol,
        public args: BoundExpression[],
    )
    {
        super(BoundNodeKind.UnaryExpression, syntax, symbolTable, func.type, false)
        this.symbol = func
    }
}

export class BoundArrayIndexExpression extends BoundExpression
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        type: Type,
        public array: BoundExpression,
        public index: BoundExpression,
    )
    {
        super(BoundNodeKind.UnaryExpression, syntax, symbolTable, type, false)
    }
}




////////////////////////////////////////////////////////////////////////////////////////////////////
// Statements

export class BoundBlockStatement extends BoundStatement
{
    constructor(
        syntax: SyntaxNode | null,
        symbolTable: SymbolTable,
        public statements: BoundStatement[]
    ) { super(BoundNodeKind.BlockStatement, syntax, symbolTable) }
}
/*
export class BoundNameExpression
    // Expressions
    FunccallExpression = "FunccallExpression",
    Arrayindexing = "Arrayindexing",
    Memberaccess = "Memberaccess",
    ParenthesizedExpression = "ParenthesizedExpression",
    UnaryExpression = "UnaryExpression",
    BinaryExpression = "BinaryExpression",
    TernaryConditionalExpression = "TernaryConditionalExpression",

    // Literals
    EnumValueLiteral = "EnumValueLiteral",
    ArrayLiteral = "ArrayLiteral",

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

    EnumDeclarationStatement = "EnumDeclarationStatement",
    EnumDefinitionStatement = "EnumDefinitionStatement",
    StructDeclarationStatement = "StructDeclarationStatement",
    StructDefinitionStatement = "StructDefinitionStatement",
    UnionDeclarationStatement = "UnionDeclarationStatement",
    UnionDefinitionStatement = "UnionDefinitionStatement",
    FunctionDeclarationStatement = "FunctionDeclarationStatement",
    FunctionDefinitionStatement = "FunctionDefinitionStatement",
    VariableDeclarationStatement = "VariableDeclarationStatement",
    ArrayDeclarationStatement = "ArrayDeclarationStatement",

    // Contains global function- and variable declarations
    Module = "Module",
*/