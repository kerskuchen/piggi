// deno-lint-ignore-file prefer-const

import { BoundArrayIndexExpression, BoundBinaryExpression, BoundBinaryOperatorKind, BoundBlockStatement, BoundExpression, BoundFunctionCall, BoundNameExpression, BoundNode, BoundNodeKind, BoundParenthesizedExpression, BoundPrimitiveLiteral, BoundSizeofExpression, BoundStatement, BoundTernaryConditionalExpression, BoundTypeCastExpression, BoundUnaryExpression, BoundUnaryOperatorKind } from "./boundtree.ts"
import { DiagnosticBag } from "./common.ts"
import { SymbolTable, SymbolKind, SymbolScopeKind } from "./old/symbols.ts"
import { ArrayIndexExpressionSyntax, ArrayLiteralSyntax, ArrayTypeExpressionSyntax, BinaryExpressionSyntax, BlockStatementSyntax, BoolLiteralSyntax, BreakStatementSyntax, CaseStatementSyntax, CharacterLiteralSyntax, ContinueStatementSyntax, DoWhileStatementSyntax, EnumDeclarationStatementSyntax, ExpressionStatementSyntax, ExpressionSyntax, ForStatementSyntax, FuncCallExpressionSyntax, FunctionDeclarationStatementSyntax, GlobalVariableDeclarationStatementSyntax, IfStatementSyntax, IntegerLiteralSyntax, MemberAccessExpressionSyntax, ModuleMemberSyntax, ModuleSyntax, NameExpressionSyntax, NullLiteralSyntax, ParenthesizedExpressionSyntax, ReturnStatementSyntax, SizeofExpressionSyntax, StatementSyntax, StringLiteralSyntax, StructDeclarationStatementSyntax, StructDefinitionStatementSyntax, SwitchStatementSyntax, SyntaxNode, SyntaxToken, SyntaxTree, TernaryConditionalExpressionSyntax, TypeCastExpressionSyntax, TypeExpressionSyntax, UnaryExpressionSyntax, VariableDeclarationStatementSyntax, WhileStatementSyntax } from "./syntax.ts"
import { SyntaxKind } from "./syntax.ts"
import { ArrayType, BasicType, BasicTypeKind, IndirectionType, Type, TypeConversionResult, TypeKind } from "./types.ts"

export class BoundUnaryOperator
{
    private constructor(
        public tokenKind: SyntaxKind,
        public operatorKind: BoundUnaryOperatorKind,
        public operandType: Type,
        public resultType: Type,
        public resultIsRValue: boolean,
        public operandMustBeLValue: boolean,
    ) { }

    static FromTokenAndOperandType(diagnostics: DiagnosticBag, token: SyntaxToken, operandType: Type): BoundUnaryOperator | null
    {
        let operatorKind = null
        let tokenKind = token.kind
        let resultType = operandType
        let resultIsRValue = true
        let operandMustBeLValue = false

        if (operandType.IsPointerType()) {
            if (token.kind == SyntaxKind.StarToken) {
                operatorKind = BoundUnaryOperatorKind.Dereference
                resultType = operandType.DecreaseIndirection()
                resultIsRValue = false
                operandMustBeLValue = true
            }
        }
        if (operandType.IsNumberType()) {
            if (token.kind == SyntaxKind.PlusToken)
                operatorKind = BoundUnaryOperatorKind.Identity
            if (token.kind == SyntaxKind.MinusToken)
                operatorKind = BoundUnaryOperatorKind.Negation
            if (token.kind == SyntaxKind.BangToken)
                operatorKind = BoundUnaryOperatorKind.LogicalNegation
            if (token.kind == SyntaxKind.TildeToken)
                operatorKind = BoundUnaryOperatorKind.BitwiseNegation
        }
        if (token.kind == SyntaxKind.AmpersandToken) {
            operatorKind = BoundUnaryOperatorKind.Address
            resultType = operandType.IncreaseIndirection()
            resultIsRValue = false
            operandMustBeLValue = true
        }

        if (operatorKind != null)
            return new BoundUnaryOperator(tokenKind, operatorKind, operandType, resultType, resultIsRValue, operandMustBeLValue)

        diagnostics.ReportError(
            token.GetLocation(),
            `No applicable unary operation for combination token '${token.GetText()}', type '${operandType.PrettyPrint()}'`,
        )
        return null
    }
}

export class BoundBinaryOperator
{
    private constructor(
        public tokenKind: SyntaxKind,
        public operatorKind: BoundBinaryOperatorKind,
        public leftType: Type,
        public rightType: Type,
        public resultType: Type,
        public resultIsRValue: boolean,
        public leftMustBeLValue: boolean,
        public rightMustBeLValue: boolean,
    ) { }

    static FromTokenAndOperandTypes(diagnostics: DiagnosticBag, token: SyntaxToken, leftType: Type, rightType: Type): BoundBinaryOperator | null
    {
        let operatorKind = null
        let tokenKind = token.kind
        let resultType = leftType
        let resultIsRValue = true
        let leftMustBeLValue = false
        let rightMustBeLValue = false

        if (token.kind == SyntaxKind.EqualsToken) {
            let conversion = rightType.CanConvertTo(leftType)
            if (conversion == TypeConversionResult.NonConvertible) {
                diagnostics.ReportError(
                    token.GetLocation(),
                    `Incompatible types for assignment '${leftType.PrettyPrint()}' = '${rightType.PrettyPrint()}'`,
                )
            }
            if (conversion == TypeConversionResult.ExplicitlyConvertible) {
                diagnostics.ReportError(
                    token.GetLocation(),
                    `Cannot implicitly convert types for assignment '${leftType.PrettyPrint()}' = '${rightType.PrettyPrint()}'`,
                )
            }
            operatorKind = BoundBinaryOperatorKind.Assignment
            leftMustBeLValue = true
        }

        if ((token.kind == SyntaxKind.PlusToken)
            && leftType.IsPointerType() && rightType.IsNumberType()) {
            operatorKind = BoundBinaryOperatorKind.AddToPointer
            resultIsRValue = false
            leftMustBeLValue = true
        }
        if ((token.kind == SyntaxKind.PlusEqualsToken)
            && leftType.IsPointerType() && rightType.IsNumberType()) {
            operatorKind = BoundBinaryOperatorKind.AddToPointerAssignment
            resultIsRValue = false
            leftMustBeLValue = true
        }
        if ((token.kind == SyntaxKind.MinusToken)
            && leftType.IsPointerType() && rightType.IsNumberType()) {
            operatorKind = BoundBinaryOperatorKind.SubtractFromPointer
            resultIsRValue = false
            leftMustBeLValue = true
        }
        if ((token.kind == SyntaxKind.MinusEqualsToken)
            && leftType.IsPointerType() && rightType.IsNumberType()) {
            operatorKind = BoundBinaryOperatorKind.SubtractFromPointerAssignment
            resultIsRValue = false
            leftMustBeLValue = true
        }
        if ((token.kind == SyntaxKind.MinusToken)
            && leftType.IsPointerType() && rightType.IsPointerType()
            && leftType.GetIndirectionLevel() == rightType.GetIndirectionLevel()
            && (leftType.kind == rightType.kind)) {
            operatorKind = BoundBinaryOperatorKind.DistanceBetweenPointers
            leftMustBeLValue = true
            rightMustBeLValue = true
            resultType = Type.FromPrimitive(BasicTypeKind.Long)
        }

        if (token.kind == SyntaxKind.EqualsEqualsToken) {
            let conversion = leftType.CanConvertTo(rightType)
            if (conversion == TypeConversionResult.ImplictlyConvertible
                || conversion == TypeConversionResult.Identical) {
                operatorKind = BoundBinaryOperatorKind.Equals
                resultType = Type.FromPrimitive(BasicTypeKind.Bool)
            }
        }
        if (token.kind == SyntaxKind.BangEqualsToken) {
            let conversion = leftType.CanConvertTo(rightType)
            if (conversion == TypeConversionResult.ImplictlyConvertible
                || conversion == TypeConversionResult.Identical) {
                operatorKind = BoundBinaryOperatorKind.NotEquals
                resultType = Type.FromPrimitive(BasicTypeKind.Bool)
            }
        }

        if (leftType.IsEnumType() && rightType.IsEnumType()
            || leftType.IsNumberType() && rightType.IsNumberType()
            || leftType.IsCharType() && rightType.IsCharType()) {
            switch (token.kind) {
                case SyntaxKind.EqualsEqualsToken: {
                    operatorKind = BoundBinaryOperatorKind.Equals
                    resultType = Type.FromPrimitive(BasicTypeKind.Bool)
                    break
                }
                case SyntaxKind.BangEqualsToken: {
                    operatorKind = BoundBinaryOperatorKind.NotEquals
                    resultType = Type.FromPrimitive(BasicTypeKind.Bool)
                    break
                }
                case SyntaxKind.LessToken: {
                    operatorKind = BoundBinaryOperatorKind.Less
                    resultType = Type.FromPrimitive(BasicTypeKind.Bool)
                    break
                }
                case SyntaxKind.LessEqualsToken: {
                    operatorKind = BoundBinaryOperatorKind.LessEquals
                    resultType = Type.FromPrimitive(BasicTypeKind.Bool)
                    break
                }
                case SyntaxKind.GreaterToken: {
                    operatorKind = BoundBinaryOperatorKind.Greater
                    resultType = Type.FromPrimitive(BasicTypeKind.Bool)
                    break
                }
                case SyntaxKind.GreaterEqualsToken: {
                    operatorKind = BoundBinaryOperatorKind.GreaterEquals
                    resultType = Type.FromPrimitive(BasicTypeKind.Bool)
                    break
                }
            }
        }

        // TODO: introduce concept of 'truthyness' 
        // We basically allow almost any types for the logical operators
        if (token.kind == SyntaxKind.AmpersandAmpersandToken
            && !leftType.IsVoidType() && !rightType.IsVoidType()) {
            operatorKind = BoundBinaryOperatorKind.LogicalAnd
        }
        if (token.kind == SyntaxKind.PipePipeToken
            && !leftType.IsVoidType() && !rightType.IsVoidType()) {
            operatorKind = BoundBinaryOperatorKind.LogicalOr
        }

        if (leftType.IsNumberType() && rightType.IsNumberType() || leftType.IsCharType() && rightType.IsCharType()) {
            switch (token.kind) {
                case SyntaxKind.PlusToken: {
                    operatorKind = BoundBinaryOperatorKind.Add
                    break
                }
                case SyntaxKind.PlusEqualsToken: {
                    operatorKind = BoundBinaryOperatorKind.AddAssignment
                    resultIsRValue = false
                    leftMustBeLValue = true
                    break
                }
                case SyntaxKind.MinusToken: {
                    operatorKind = BoundBinaryOperatorKind.Subtract
                    break
                }
                case SyntaxKind.MinusEqualsToken: {
                    operatorKind = BoundBinaryOperatorKind.SubtractAssignment
                    resultIsRValue = false
                    leftMustBeLValue = true
                    break
                }
                case SyntaxKind.StarToken: {
                    operatorKind = BoundBinaryOperatorKind.Multiply
                    break
                }
                case SyntaxKind.StarEqualsToken: {
                    operatorKind = BoundBinaryOperatorKind.MultiplyAssignment
                    resultIsRValue = false
                    leftMustBeLValue = true
                    break
                }
                case SyntaxKind.SlashToken: {
                    operatorKind = BoundBinaryOperatorKind.Divide
                    break
                }
                case SyntaxKind.SlashEqualsToken: {
                    operatorKind = BoundBinaryOperatorKind.DivideAssignment
                    resultIsRValue = false
                    leftMustBeLValue = true
                    break
                }
                case SyntaxKind.PercentToken: {
                    operatorKind = BoundBinaryOperatorKind.Remainder
                    break
                }
                case SyntaxKind.PercentEqualsToken: {
                    operatorKind = BoundBinaryOperatorKind.RemainderAssignment
                    resultIsRValue = false
                    leftMustBeLValue = true
                    break
                }

                case SyntaxKind.LessLessToken: {
                    operatorKind = BoundBinaryOperatorKind.BitshiftLeft
                    break
                }
                case SyntaxKind.LessLessEqualsToken: {
                    operatorKind = BoundBinaryOperatorKind.BitshiftLeftAssignment
                    resultIsRValue = false
                    leftMustBeLValue = true
                    break
                }
                case SyntaxKind.GreaterGreaterToken: {
                    operatorKind = BoundBinaryOperatorKind.BitshiftRight
                    break
                }
                case SyntaxKind.GreaterGreaterEqualsToken: {
                    operatorKind = BoundBinaryOperatorKind.BitshiftRightAssignment
                    resultIsRValue = false
                    leftMustBeLValue = true
                    break
                }
                case SyntaxKind.HatToken: {
                    operatorKind = BoundBinaryOperatorKind.BitwiseXor
                    break
                }
                case SyntaxKind.HatEqualsToken: {
                    operatorKind = BoundBinaryOperatorKind.BitwiseXorAssignment
                    resultIsRValue = false
                    leftMustBeLValue = true
                    break
                }
                case SyntaxKind.AmpersandToken: {
                    operatorKind = BoundBinaryOperatorKind.BitwiseAnd
                    break
                }
                case SyntaxKind.AmpersandEqualsToken: {
                    operatorKind = BoundBinaryOperatorKind.BitwiseAndAssignment
                    resultIsRValue = false
                    leftMustBeLValue = true
                    break
                }
                case SyntaxKind.PipeToken: {
                    operatorKind = BoundBinaryOperatorKind.BitwiseOr
                    break
                }
                case SyntaxKind.PipeEqualsToken: {
                    operatorKind = BoundBinaryOperatorKind.BitwiseOrAssignment
                    resultIsRValue = false
                    leftMustBeLValue = true
                    break
                }
            }
        }

        if (operatorKind != null)
            return new BoundBinaryOperator(tokenKind, operatorKind, leftType, rightType, resultType, resultIsRValue, leftMustBeLValue, rightMustBeLValue)

        diagnostics.ReportError(
            token.GetLocation(),
            `No applicable binary operation for combination token '${token.GetText()}', left type '${leftType.PrettyPrint()}, right type '${rightType.PrettyPrint()}'`,
        )
        return null
    }
}

export class Binder
{
    diagnostics = new DiagnosticBag()
    loopLevel: number = 0
    switchCaseLevel: number = 0
    currentFunctionSymbol: Symbol | null = null

    constructor(
        public symbolTable: SymbolTable
    ) { }


    WrapInBlockStatementIfNecessary(node: BoundStatement): BoundStatement
    {
        if (node.kind != BoundNodeKind.BlockStatement) {
            let statements = [node]
            return new BoundBlockStatement(null, this.symbolTable, statements)
        }
        return node
    }

    // Turns {{{ a; {b c} d; e; }}} . { a; {b c} d; e; }
    FlattenBlockStatementIfNecessary(node: BoundStatement): BoundStatement
    {
        if (node instanceof BoundBlockStatement) {
            if (node.statements.length == 1 && node.statements[0].kind == BoundNodeKind.BlockStatement) {
                let result = node.statements[0] as BoundBlockStatement
                result.symbolTable.parent = node.symbolTable.parent
                return this.FlattenBlockStatementIfNecessary(result)
            }
        }
        return node
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    // Types and specials

    BindTypeRecursive(syntax: TypeExpressionSyntax | ArrayTypeExpressionSyntax): Type
    {
        if (syntax instanceof TypeExpressionSyntax) {
            let baseType
            if (syntax.baseType instanceof ArrayTypeExpressionSyntax) {
                baseType = this.BindType(syntax.baseType)
            } else {
                let basicTypeName = null
                let basicTypeKind = BasicTypeKind.Void
                let basicTypeToken = syntax.baseType as SyntaxToken
                switch (basicTypeToken.kind) {
                    case SyntaxKind.VoidKeyword:
                        basicTypeKind = BasicTypeKind.Void
                        break
                    case SyntaxKind.CharKeyword:
                        basicTypeKind = BasicTypeKind.Char
                        break
                    case SyntaxKind.BoolKeyword:
                        basicTypeKind = BasicTypeKind.Bool
                        break
                    case SyntaxKind.ByteKeyword:
                        basicTypeKind = BasicTypeKind.Byte
                        break
                    case SyntaxKind.ShortKeyword:
                        basicTypeKind = BasicTypeKind.Short
                        break
                    case SyntaxKind.IntKeyword:
                        basicTypeKind = BasicTypeKind.Int
                        break
                    case SyntaxKind.LongKeyword:
                        basicTypeKind = BasicTypeKind.Long
                        break
                    case SyntaxKind.CStringKeyword:
                        basicTypeKind = BasicTypeKind.CString
                        break
                    case SyntaxKind.IdentifierToken: {
                        let name = basicTypeToken.GetText()
                        let symbol = this.symbolTable.GetSymbol(name)
                        if (symbol != null) {
                            if (symbol.kind == SymbolKind.Struct) {
                                basicTypeKind = BasicTypeKind.Struct
                                basicTypeName = name
                                break
                            } else if (symbol.kind == SymbolKind.Union) {
                                basicTypeKind = BasicTypeKind.Union
                                basicTypeName = name
                                break
                            } else if (symbol.kind == SymbolKind.Enum) {
                                basicTypeKind = BasicTypeKind.Enum
                                basicTypeName = name
                                break
                            }
                        }
                    } // Fallthrough
                    default:
                        this.diagnostics.ReportError(
                            basicTypeToken.GetLocation(),
                            `SyntaxToken '${basicTypeToken.GetText()}' is not a type`
                        )
                }
                baseType = new BasicType(basicTypeKind, name)
            }

            let indirectionLevel = syntax.starTokens.length
            if (indirectionLevel == 0)
                return baseType
            else
                return new IndirectionType(baseType, indirectionLevel)

        } else if (syntax instanceof ArrayTypeExpressionSyntax) {
            let elemType = this.BindType(syntax.elemType)
            return new ArrayType(elemType, syntax.arraySizeLiteral.intValue!)
        }

        throw new Error("unreachable")
    }

    BindType(syntax: TypeExpressionSyntax | ArrayTypeExpressionSyntax): Type
    {
        let result = this.BindTypeRecursive(syntax)
        if (result instanceof BasicType && result.IsStructType()) {
            let symbol = this.symbolTable.GetSymbol(result.name)
            if (!symbol.alreadyDefined) {
                this.diagnostics.ReportError(
                    syntax.GetLocation(),
                    `Usage of undefined but forward declared type '${result.name}' is only allowed as pointer`,
                )
            }
        }
        return result
    }

    BindTypeCastExpression(syntax: TypeCastExpressionSyntax): BoundNode
    {
        let expression = this.BindExpression(syntax.expression)
        let targetType = this.BindType(syntax.targetType)
        let conversion = expression.type.CanConvertTo(targetType)
        if (conversion == TypeConversionResult.NonConvertible) {
            this.diagnostics.ReportError(
                syntax.asKeyword.GetLocation(),
                `Cast from type '${expression.type.PrettyPrint()}' to type '${targetType.PrettyPrint()}' is impossible`,
            )
        }
        return new BoundTypeCastExpression(syntax, this.symbolTable, targetType, expression)
    }

    BindSizeOfExpression(syntax: SizeofExpressionSyntax): BoundNode
    {
        let typeExpression = this.BindType(syntax.typeExpression)
        return new BoundSizeofExpression(syntax, this.symbolTable, typeExpression)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    // Literals

    BindNullLiteral(syntax: NullLiteralSyntax): BoundNode
    {
        let type = Type.FromPrimitivePointer(BasicTypeKind.Void, 1)
        return new BoundPrimitiveLiteral(BoundNodeKind.NullLiteral, syntax, this.symbolTable, type)
    }

    BindCharacterLiteral(syntax: CharacterLiteralSyntax): BoundNode
    {
        let type = Type.FromPrimitive(BasicTypeKind.Char)
        return new BoundPrimitiveLiteral(BoundNodeKind.CharacterLiteral, syntax, this.symbolTable, type)
    }

    BindStringLiteral(syntax: StringLiteralSyntax): BoundNode
    {
        if (syntax.kind != SyntaxKind.StringLiteral)
            throw new Error(`Invalid syntax - expected 'StringLiteral'`)

        let type = Type.FromPrimitive(BasicTypeKind.CString)
        return new BoundPrimitiveLiteral(BoundNodeKind.StringLiteral, syntax, this.symbolTable, type)
    }

    BindBoolLiteral(syntax: BoolLiteralSyntax): BoundNode
    {
        let type = Type.FromPrimitive(BasicTypeKind.Bool)
        return new BoundPrimitiveLiteral(BoundNodeKind.BoolLiteral, syntax, this.symbolTable, type)
    }

    BindIntegerLiteral(syntax: IntegerLiteralSyntax): BoundNode
    {
        let value = syntax.integerLiteral.intValue!
        let type
        if (-128 <= value && value <= 127)
            type = Type.FromPrimitive(BasicTypeKind.Byte)
        else if (-32767 <= value && value <= 32767)
            type = Type.FromPrimitive(BasicTypeKind.Short)
        else if (-2147483647 <= value && value <= 2147483647)
            type = Type.FromPrimitive(BasicTypeKind.Int)
        else
            type = Type.FromPrimitive(BasicTypeKind.Long)

        return new BoundPrimitiveLiteral(BoundNodeKind.IntegerLiteral, syntax, this.symbolTable, type)
    }

    /*

    BindEnumValueLiteral(syntax: SyntaxNode): BoundNode
    {
        if (syntax.kind != SyntaxKind.EnumValueLiteral)
            throw new Error(`Invalid syntax - expected 'EnumValueLiteral'`)

        let SyntaxToken enumIdentifier = syntax.enumLiteralExpr.enumIdentifier
        let SyntaxToken valueIdentifier = syntax.enumLiteralExpr.valueIdentifier

        let String enumText = TokenGetText(enumIdentifier)
        let Symbol* enumSymbol = this.symbolTable.GetSymbol(enumText)
        if (enumSymbol == null) {
            this.diagnostics.ReportError(
                enumIdentifier.GetLocation(),
                `Undeclared identifier '${}'`,
                enumText.cstr
            )
        }
        if (enumSymbol.kind != SymbolKind.Enum) {
            this.diagnostics.ReportError(
                enumIdentifier.GetLocation(),
                `Identifier '${}' is not an enum`,
                enumText.cstr
            )
        }

        let String valueText = TokenGetText(valueIdentifier)
        let Symbol* valueSymbol = GetSymbol(enumSymbol.membersSymbolTable, valueText)
        if (valueSymbol == null) {
            this.diagnostics.ReportError(
                enumIdentifier.GetLocation(),
                `Identifier '${}' is not a member of enum '${}'`,
                valueText.cstr, enumText.cstr
            )
        }
        assert(valueSymbol.kind == SymbolKind.Enumvalue)

        let BoundNode result = BoundNodeCreate2(BoundNodeKind.EnumValueLiteral, this.symbolTable, syntax)
        result.isRValue = true
        result.symbol = valueSymbol
        result.type = valueSymbol.type
        return result
    }

    BindArrayLiteral(Symbol* arraySymbol, syntax: ArrayLiteralSyntax): BoundNode
    {
        if (syntax.kind != SyntaxKind.ArrayLiteral)
            throw new Error(`Invalid syntax - expected 'ArrayLiteral'`)
        assert(arraySymbol.type.isArray)

        let BoundNode result = BoundNodeCreate2(BoundNodeKind.ArrayLiteral, this.symbolTable, syntax)
        let Type arrayElemType = GetElementTypeForArrayType(arraySymbol.type)

        for (let int index = 0; index < syntax.arrayLiteralExpr.elemsWithSeparators.count; index += 2) {
            let SyntaxNode* expression = syntax.arrayLiteralExpr.elemsWithSeparators.nodes[index]
            let BoundNode boundExpression = this.BindExpression(expression)
            let TypeConversionResult conversion = CanConvertTypeFromTo(boundExpression.type, arrayElemType)
            if (conversion == TypeConversionResult.NonConvertible
                || conversion == TypeConversionResult.ExplicitlyConvertible) {
                this.diagnostics.ReportError(
                    expression.token.GetLocation(),
                    `Cannot convert type '${}' of element ${} in array initializer to array type '${}'`,
                    TypeGetText(boundExpression.type).cstr, index / 2 + 1, TypeGetText(arrayElemType).cstr
                )
            }
            BoundNodeArrayPush(& result.children, boundExpression)
        }

        let int elementCount = result.children.count
        let SyntaxToken leftBrace = syntax.arrayLiteralExpr.leftBrace
        if (arraySymbol.type.arrayElementCount == -1)
            arraySymbol.type.arrayElementCount = elementCount
        if (elementCount == 0) {
            this.diagnostics.ReportError(
                leftBrace.GetLocation(),
                `Element count cannot be zero in array initializer of array '${}'`,
                arraySymbol.name.cstr
            )
        }
        if (arraySymbol.type.arrayElementCount != elementCount) {
            this.diagnostics.ReportError(
                leftBrace.GetLocation(),
                `Element count ${} of array initializer does not match element count ${} of array '${}'`,
                elementCount, arraySymbol.type.arrayElementCount, arraySymbol.name.cstr
            )
        }

        result.isRValue = true
        return result
    }
    */

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    // Expressions

    BindNameExpression(syntax: NameExpressionSyntax): BoundNode
    {
        let identifier = syntax.identifier
        let identifierText = identifier.GetText()
        let symbol = this.symbolTable.GetSymbol(identifierText)
        if (symbol == null) {
            this.diagnostics.ReportError(
                identifier.GetLocation(),
                `Undeclared identifier '${identifierText}'`,
            )
        }
        return new BoundNameExpression(syntax, this.symbolTable, symbol.type, symbol)
    }

    BindParenthesizedExpression(syntax: ParenthesizedExpressionSyntax): BoundNode
    {
        let inner = this.BindExpression(syntax.inner)
        return new BoundParenthesizedExpression(syntax, this.symbolTable, inner.type, inner)
    }

    BindFunctionCallExpression(syntax: FuncCallExpressionSyntax): BoundNode
    {
        let left = this.BindExpression(syntax.func)
        let leftParen = syntax.leftParen

        let funcSymbol = left.symbol
        if (funcSymbol == null) {
            this.diagnostics.ReportError(
                leftParen.GetLocation(),
                `Expression left of '${leftParen.GetText()}' is not a known symbol`,
            )
            return left
        }
        if (funcSymbol.kind != SymbolKind.Function) {
            // TODO: change this to a span that prints the whole left expression
            this.diagnostics.ReportError(
                leftParen.GetLocation(),
                `Identifier '${funcSymbol.name}' is not a callable function`,
            )
            return left
        }

        let argumentList = []
        for (let index = 0; index < syntax.argumentsWithSeparators.length; index += 2) {
            let arg = syntax.argumentsWithSeparators[index]
            let boundArg = this.BindExpression(arg as ExpressionSyntax)
            argumentList.push(boundArg)
        }

        if (funcSymbol.membersSymbolTable == null)
            throw new Error("Missing membersSymbolTable")

        if (funcSymbol.isVariadric) {
            if (argumentList.length < funcSymbol.membersSymbolTable.symbols.length) {
                this.diagnostics.ReportError(
                    leftParen.GetLocation(),
                    `Function '${funcSymbol.name}' expects at least ${funcSymbol.membersSymbolTable.symbols.length} arguments but ${argumentList.length} arguments were provided`,
                )
            }
        } else {
            if (argumentList.length != funcSymbol.membersSymbolTable.symbols.length) {
                this.diagnostics.ReportError(
                    leftParen.GetLocation(),
                    `Function '${funcSymbol.name}' expects ${funcSymbol.membersSymbolTable.symbols.length} arguments but ${argumentList.length} arguments were provided`,
                )
            }
        }

        for (let argumentIndex = 0; argumentIndex < funcSymbol.membersSymbolTable.symbols.length; argumentIndex += 1) {
            let argumentType = argumentList[argumentIndex].type
            let expectedType = funcSymbol.membersSymbolTable.symbols[argumentIndex].type

            let conversion = argumentType.CanConvertTo(expectedType)
            if (conversion == TypeConversionResult.NonConvertible) {
                this.diagnostics.ReportError(
                    leftParen.GetLocation(),
                    `Passed incompatible type '${argumentType.PrettyPrint()}' for argument ${argumentIndex + 1} to function '${funcSymbol.name}' - expected type '${expectedType.PrettyPrint()}'`,
                )
            }
            if (conversion == TypeConversionResult.ExplicitlyConvertible) {
                this.diagnostics.ReportError(
                    leftParen.GetLocation(),
                    `Cannot implicitly convert type '${argumentType.PrettyPrint()}' of argument ${argumentIndex + 1}  to expected type '${expectedType.PrettyPrint()}' in function call of '${funcSymbol.name}'`,
                )
            }
        }

        return new BoundFunctionCall(syntax, this.symbolTable, funcSymbol, argumentList)
    }

    BindArrayIndexingExpression(syntax: ArrayIndexExpressionSyntax): BoundNode
    {
        if (syntax.kind != SyntaxKind.ArrayIndexExpression)
            throw new Error(`Invalid syntax - expected 'ArrayIndexExpression'`)

        let leftBracket = syntax.leftBracket
        let left = this.BindExpression(syntax.array)
        let index = this.BindExpression(syntax.indexExpression)
        if (!index.type.IsNumberType()) {
            this.diagnostics.ReportError(
                leftBracket.GetLocation(),
                `Array index after '${leftBracket.GetText()}' must be number type`,
            )
            return left
        }
        if (left.symbol == null || left.symbol.type.GetIndirectionLevel() == 0) {
            this.diagnostics.ReportError(
                leftBracket.GetLocation(),
                `Left hand side of array index operator '${leftBracket.GetText()}' is not a known array or pointer`,
            )
            return left
        }

        let elemType
        if (left.symbol.type instanceof ArrayType)
            elemType = left.symbol.type.elementType
        else
            elemType = left.symbol.type.DecreaseIndirection()

        return new BoundArrayIndexExpression(syntax, this.symbolTable, elemType, left, index)
    }

    BindMemberAccessExpression(syntax: MemberAccessExpressionSyntax): BoundNode
    {
        if (syntax.kind != SyntaxKind.MemberAccessExpression)
            throw new Error(`Invalid syntax - expected 'MemberAccessExpression'`)

        let container = this.BindExpression(syntax.container)
        let accessorToken = syntax.dot
        if (container.type.kind != TypeKind.Struct) {
            this.diagnostics.ReportError(
                accessorToken.GetLocation(),
                `Attempt to access member of non struct identifier '${container.syntax!.GetLocation().GetText()}'`,
            )
        }

        let Symbol* containerSymbol = this.symbolTable.GetSymbol(container.type.name)
        assert(containerSymbol != null)
        if (containerSymbol.kind != SymbolKind.Struct && containerSymbol.kind != SymbolKind.Union) {
            this.diagnostics.ReportError(
                accessorToken.GetLocation(),
                `Attempt to access member of non struct identifier '${}'`,
                containerSymbol.name.cstr
            )
        }
        if (!containerSymbol.alreadyDefined) {
            this.diagnostics.ReportError(
                accessorToken.GetLocation(),
                `Attempt to access member of forward declared but undefined struct '${}'`,
                containerSymbol.name.cstr
            )
        }

        let bool isArrow = accessorToken.kind == SyntaxKind.ArrowToken
        if (isArrow && TypeGetIndirectionLevel(container.type) == 0) {
            this.diagnostics.ReportError(
                accessorToken.GetLocation(),
                `Member access of '${}' with '.' is only allowed for pointer types`,
                containerSymbol.name.cstr
            )
        }
        if (!isArrow && TypeGetIndirectionLevel(container.type) > 0) {
            this.diagnostics.ReportError(
                accessorToken.GetLocation(),
                `Member access of '${}' with '.' is only allowed for non-pointer types`,
                containerSymbol.name.cstr
            )
        }

        let SyntaxToken memberIdentifier = syntax.memberIdentifier
        let String identifierText = TokenGetText(memberIdentifier)
        let Symbol* memberSymbol = GetSymbol(containerSymbol.membersSymbolTable, identifierText)
        if (memberSymbol == null) {
            this.diagnostics.ReportError(
                memberIdentifier.GetLocation(),
                `Undeclared struct or union member '${}'`,
                identifierText.cstr
            )
        }
        assert(memberSymbol.kind == SymbolKind.Member)

        let BoundNode result = BoundNodeCreate2(BoundNodeKind.Memberaccess, this.symbolTable, syntax)
        result.symbol = memberSymbol
        result.type = memberSymbol.type
        result.left = container
        return result
    }


    BindUnaryExpression(syntax: UnaryExpressionSyntax): BoundNode
    {
        let operatorToken = syntax.operator
        let operand = this.BindExpression(syntax.operand)
        let operator = BoundUnaryOperator.FromTokenAndOperandType(this.diagnostics, operatorToken, operand.type)
        if (operator == null)
            return operand

        if (operator.operandMustBeLValue && operand.isRValue) {
            this.diagnostics.ReportError(
                operatorToken.GetLocation(),
                `Operand of operator '${operatorToken.kind}' must be an storage location`,
            )
        }
        return new BoundUnaryExpression(syntax, this.symbolTable, operator, operand)
    }

    BindBinaryExpression(syntax: BinaryExpressionSyntax): BoundNode
    {
        let left = this.BindExpression(syntax.left)
        let right = this.BindExpression(syntax.left)
        let operatorToken = syntax.operator

        let operator = BoundBinaryOperator.FromTokenAndOperandTypes(this.diagnostics, operatorToken, left.type, right.type)
        if (operator == null)
            return left

        if (operator.leftMustBeLValue && left.isRValue) {
            this.diagnostics.ReportError(
                operatorToken.GetLocation(),
                `Left argument of operator '${operatorToken.GetText()}' must be an storage location`,
            )
        }
        if (operator.rightMustBeLValue && right.isRValue) {
            this.diagnostics.ReportError(
                operatorToken.GetLocation(),
                `Right argument of operator '${operatorToken.GetText()}' must be a storage location`,
            )
        }
        return new BoundBinaryExpression(syntax, this.symbolTable, operator, left, right)
    }

    BindTernaryConditionalExpression(syntax: TernaryConditionalExpressionSyntax): BoundNode
    {
        let condition = this.BindExpression(syntax.condition)
        let thenExpression = this.BindExpression(syntax.thenExpression)
        let elseExpression = this.BindExpression(syntax.elseExpression)
        let type = Type.GetTypeThatFitsBothTypes(thenExpression.type, elseExpression.type)
        if (type == null) {
            this.diagnostics.ReportError(
                syntax.questionmark.GetLocation(),
                `Incompatible expression types in ternary operator - then branch: '${thenExpression.type.PrettyPrint()}', else branch: '${elseExpression.type.PrettyPrint()}'`,
            )
            type = thenExpression.type
        }

        return new BoundTernaryConditionalExpression(syntax, this.symbolTable, type, condition, thenExpression, elseExpression)
    }

    BindExpression(syntax: ExpressionSyntax): BoundExpression
    {
        let BoundNode result = null
        switch (syntax.kind) {
            // Expressions
            case SyntaxKind.UnaryExpression:
                result = this.BindUnaryExpression(syntax as UnaryExpressionSyntax)
                break
            case SyntaxKind.BinaryExpression:
                result = this.BindBinaryExpression(syntax as BinaryExpressionSyntax)
                break
            case SyntaxKind.FuncCallExpression:
                result = this.BindFunctionCallExpression(syntax as FuncCallExpressionSyntax)
                break
            case SyntaxKind.ArrayIndexExpression:
                result = this.BindArrayIndexingExpression(syntax as ArrayIndexExpressionSyntax)
                break
            case SyntaxKind.MemberAccessExpression:
                result = this.BindMemberAccessExpression(syntax as MemberAccessExpressionSyntax)
                break
            case SyntaxKind.TypeCastExpression:
                result = this.BindTypeCastExpression(syntax as TypeCastExpressionSyntax)
                break
            case SyntaxKind.ParenthesizedExpression:
                result = this.BindParenthesizedExpression(syntax as ParenthesizedExpressionSyntax)
                break
            case SyntaxKind.TernaryConditionalExpression:
                result = this.BindTernaryConditionalExpression(syntax as TernaryConditionalExpressionSyntax)
                break
            case SyntaxKind.SizeOfExpression:
                result = this.BindSizeOfExpression(syntax as SizeofExpressionSyntax)
                break
            case SyntaxKind.NameExpression:
                result = this.BindNameExpression(syntax as NameExpressionSyntax)
                break
            case SyntaxKind.TypeExpression:
                result = this.BindType(syntax as TypeExpressionSyntax | ArrayTypeExpressionSyntax)
                break

            // Literals
            case SyntaxKind.NullLiteral:
                result = this.BindNullLiteral(syntax)
                break
            case SyntaxKind.IntegerLiteral:
                result = this.BindIntegerLiteral(syntax)
                break
            case SyntaxKind.CharacterLiteral:
                result = this.BindCharacterLiteral(syntax)
                break
            case SyntaxKind.BoolLiteral:
                result = this.BindBoolLiteral(syntax)
                break
            case SyntaxKind.StringLiteral:
                result = this.BindStringLiteral(syntax)
                break
            case SyntaxKind.EnumValueLiteral:
                result = this.BindEnumValueLiteral(syntax)
                break

            default:
                assert(false && `Unexpected expression in binder`)
        }
        if (result.type.isArray) {
            // TODO: is this the appropriate place for this check?
            result.isRValue = true // Arrays cannot be assigned to without indexing
        }
        return result
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    // Statements

    BindExpressionStatement(syntax: ExpressionStatementSyntax): BoundNode
    {
        if (syntax.kind != SyntaxKind.ExpressionStatement)
            throw new Error(`Invalid syntax - expected 'ExpressionStatement'`)

        let BoundNode result = BoundNodeCreate2(BoundNodeKind.ExpressionStatement, this.symbolTable, syntax)
        result.left = this.BindExpression(syntax.expressionStmt.expression)
        return result
    }

    BindVariableDefinitionStatement(syntax: VariableDeclarationStatementSyntax, SymbolScopeKind symbolScopeKind): BoundNode
    {
        if (syntax.kind != SyntaxKind.VariableDeclarationStatement)
            throw new Error(`Invalid syntax - expected 'VariableDeclarationStatement'`)

        let SyntaxToken identifier = syntax.variableDeclarationStmt.identifier
        let bool isLocalPersist = syntax.variableDeclarationStmt.letKeyword.kind == SyntaxKind.LetLocalPersistKeyword
        if (isLocalPersist) {
            if (symbolScopeKind != SymbolScopeKind.Local) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Cannot mark global variable '${}' as local persistent`,
                    TokenGetText(identifier).cstr
                )
            }
            symbolScopeKind = SymbolScopeKind.LocalPersist
        }

        let Type type = this.BindType(syntax.variableDeclarationStmt.typeExpression)
        let Symbol* varSymbol = AddSymbol(this.symbolTable, TokenGetText(identifier), SymbolKind.Variable, symbolScopeKind, type)
        if (varSymbol == null) {
            this.diagnostics.ReportError(
                identifier.GetLocation(),
                `Symbol was '${}' already declared in current scope`,
                TokenGetText(identifier).cstr
            )
        }

        if (IsVoidType(type)) {
            this.diagnostics.ReportError(
                identifier.GetLocation(),
                `'void' not allowed as variables '${}' storage type`,
                TokenGetText(identifier).cstr
            )
        }

        if (syntax.variableDeclarationStmt.leftBracket.kind == SyntaxKind.LeftBracketToken) {
            // Array definition
            let longint arrayElementCount = -1
            let bool arrayElementCountDefined = false
            if (syntax.variableDeclarationStmt.arraySizeLiteral.kind == SyntaxKind.IntegerLiteralToken) {
                arrayElementCount = syntax.variableDeclarationStmt.arraySizeLiteral.intvalue
                arrayElementCountDefined = true
            }
            if (arrayElementCountDefined && arrayElementCount <= 0) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Array size must be greater than zero for '${}'`,
                    TokenGetText(identifier).cstr
                )
            }
            varSymbol.type.isArray = true
            varSymbol.type.arrayElementCount = arrayElementCount
        }

        let BoundNode result = BoundNodeCreate2(BoundNodeKind.VariableDeclarationStatement, this.symbolTable, syntax)
        result.symbol = varSymbol
        if (syntax.variableDeclarationStmt.initializerExpression != null) {
            let BoundNode boundInitializer = null
            if (result.symbol.type.isArray)
                boundInitializer = this.BindArrayLiteral(result.symbol, syntax.variableDeclarationStmt.initializerExpression)
            else
                boundInitializer = this.BindExpression(syntax.variableDeclarationStmt.initializerExpression)
            result.left = boundInitializer
        }
        return result
    }

    BindIfStatement(syntax: IfStatementSyntax): BoundNode
    {
        if (syntax.kind != SyntaxKind.IfStatement)
            throw new Error(`Invalid syntax - expected 'IfStatement'`)
        assert(this.currentFunctionSymbol != null)

        let BoundNode boundCondition = this.BindExpression(syntax.ifStmt.condition)
        // TODO we should check that conditions is truthy

        let BoundNode boundThenBranch = this.BindStatement(syntax.ifStmt.thenBlock)
        boundThenBranch = _WrapInBlockStatementIfNecessary(boundThenBranch)

        let BoundNode boundElseBranch = null
        if (syntax.ifStmt.elseBlock != null) {
            boundElseBranch = this.BindStatement(syntax.ifStmt.elseBlock)
            boundElseBranch = _WrapInBlockStatementIfNecessary(boundElseBranch)
        }

        let BoundNode result = BoundNodeCreate2(BoundNodeKind.IfStatement, this.symbolTable, syntax)
        result.left = boundCondition
        result.right = boundThenBranch
        result.extra1 = boundElseBranch
        return result
    }

    BindWhileStatement(syntax: WhileStatementSyntax): BoundNode
    {
        if (syntax.kind != SyntaxKind.WhileStatement)
            throw new Error(`Invalid syntax - expected 'WhileStatement'`)
        assert(this.currentFunctionSymbol != null)

        let BoundNode boundCondition = this.BindExpression(syntax.whileStmt.condition)
        // TODO we should check that conditions is truthy

        this.loopLevel += 1
        let BoundNode boundBody = this.BindStatement(syntax.whileStmt.body)
        boundBody = _WrapInBlockStatementIfNecessary(boundBody)
        this.loopLevel -= 1

        let BoundNode result = BoundNodeCreate2(BoundNodeKind.WhileStatement, this.symbolTable, syntax)
        result.left = boundCondition
        result.right = boundBody
        return result
    }

    BindDoWhileStatement(syntax: DoWhileStatementSyntax): BoundNode
    {
        if (syntax.kind != SyntaxKind.DoWhileStatement)
            throw new Error(`Invalid syntax - expected 'DoWhileStatement'`)
        assert(this.currentFunctionSymbol != null)

        this.loopLevel += 1
        let BoundNode boundBody = this.BindStatement(syntax.doWhileStmt.body)
        boundBody = _WrapInBlockStatementIfNecessary(boundBody)
        this.loopLevel -= 1

        let BoundNode boundCondition = this.BindExpression(syntax.doWhileStmt.condition)
        // TODO we should check that conditions is truthy

        let BoundNode result = BoundNodeCreate2(BoundNodeKind.DoWhileStatement, this.symbolTable, syntax)
        result.left = boundCondition
        result.right = boundBody
        return result
    }

    BindForStatement(syntax: ForStatementSyntax): BoundNode
    {
        if (syntax.kind != SyntaxKind.ForStatement)
            throw new Error(`Invalid syntax - expected 'ForStatement'`)
        assert(this.currentFunctionSymbol != null)

        // Push symboltable scope early to also make the index local to the for statement
        this.symbolTable = SymbolTableCreate(this.symbolTable)
        let BoundNode result = BoundNodeCreate2(BoundNodeKind.ForStatement, this.symbolTable, syntax)

        let BoundNode boundInitializer = null
        if (syntax.forStmt.initializerStatement.kind == SyntaxKind.VariableDeclarationStatement)
            boundInitializer = this.BindVariableDefinitionStatement(syntax.forStmt.initializerStatement, SymbolScopeKind.Local)
        else
            boundInitializer = this.BindExpressionStatement(syntax.forStmt.initializerStatement)
        let BoundNode boundCondition = this.BindExpressionStatement(syntax.forStmt.conditionStatement)
        // TODO we should check that conditions is truthy
        let BoundNode boundIncrementExpr = this.BindExpression(syntax.forStmt.incrementExpression)

        this.loopLevel += 1
        let BoundNode boundBody = this.BindStatement(syntax.forStmt.body)
        boundBody = _WrapInBlockStatementIfNecessary(boundBody)
        this.loopLevel -= 1

        // Pop symboltable
        this.symbolTable = this.symbolTable.parent

        result.left = boundInitializer
        result.right = boundCondition
        result.extra1 = boundIncrementExpr
        result.extra2 = boundBody
        return result
    }

    BindReturnStatement(syntax: ReturnStatementSyntax): BoundNode
    {
        if (syntax.kind != SyntaxKind.ReturnStatement)
            throw new Error(`Invalid syntax - expected 'ReturnStatement'`)
        assert(this.currentFunctionSymbol != null)

        if (this.currentFunctionSymbol == null) {
            this.diagnostics.ReportError(
                syntax.returnStmt.returnKeyword.GetLocation(),
                `Invalid 'return' statement found outside of function definition`
            )
        }

        let Type functionReturnType = this.currentFunctionSymbol.type
        if (IsVoidType(functionReturnType) && syntax.returnStmt.returnExpression != null) {
            this.diagnostics.ReportError(
                syntax.returnStmt.returnKeyword.GetLocation(),
                `Invalid return expression in void function`
            )
        }
        if (!IsVoidType(functionReturnType) && syntax.returnStmt.returnExpression == null) {
            this.diagnostics.ReportError(
                syntax.returnStmt.returnKeyword.GetLocation(),
                `Must return expression in non-void function`
            )
        }

        let BoundNode boundExpression = null
        if (syntax.returnStmt.returnExpression != null) {
            boundExpression = this.BindExpression(syntax.returnStmt.returnExpression)

            let TypeConversionResult conversion = CanConvertTypeFromTo(boundExpression.type, functionReturnType)
            if (conversion == TypeConversionResult.NonConvertible) {
                this.diagnostics.ReportError(
                    syntax.returnStmt.returnKeyword.GetLocation(),
                    `Incompatible types for return expression '${}'`,
                    TokenKindToString(syntax.returnStmt.returnKeyword.kind).cstr
                )
            }
            if (conversion == TypeConversionResult.ExplicitlyConvertible) {
                this.diagnostics.ReportError(
                    syntax.returnStmt.returnKeyword.GetLocation(),
                    `Types cannot be implicitly converted for return expression '${}'`,
                    TokenKindToString(syntax.returnStmt.returnKeyword.kind).cstr
                )
            }
        }
        let BoundNode result = BoundNodeCreate2(BoundNodeKind.ReturnStatement, this.symbolTable, syntax)
        result.left = boundExpression
        return result
    }

    BindBreakStatement(syntax: BreakStatementSyntax): BoundNode
    {
        if (syntax.kind != SyntaxKind.BreakStatement)
            throw new Error(`Invalid syntax - expected 'BreakStatement'`)
        assert(this.currentFunctionSymbol != null)

        if (this.loopLevel == 0 && this.switchCaseLevel == 0) {
            this.diagnostics.ReportError(
                syntax.breakStmt.breakKeyword.GetLocation(),
                `Invalid 'break' statement found outside of loop or switch-case definition`
            )
        }
        let BoundNode result = BoundNodeCreate2(BoundNodeKind.BreakStatement, this.symbolTable, syntax)
        return result
    }

    BindContinueStatement(syntax: ContinueStatementSyntax): BoundNode
    {
        if (syntax.kind != SyntaxKind.ContinueStatement)
            throw new Error(`Invalid syntax - expected 'ContinueStatement'`)
        assert(this.currentFunctionSymbol != null)

        if (this.loopLevel == 0) {
            this.diagnostics.ReportError(
                syntax.continueStmt.continueKeyword.GetLocation(),
                `Invalid 'continue' statement found outside of loop definition`
            )
        }
        let BoundNode result = BoundNodeCreate2(BoundNodeKind.ContinueStatement, this.symbolTable, syntax)
        return result
    }

    BindBlockStatement(syntax: BlockStatementSyntax): BoundNode
    {
        if (syntax.kind != SyntaxKind.BlockStatement)
            throw new Error(`Invalid syntax - expected 'BlockStatement'`)
        assert(this.currentFunctionSymbol != null)

        // Push symboltable scope
        this.symbolTable = SymbolTableCreate(this.symbolTable)

        let BoundNode result = BoundNodeCreate2(BoundNodeKind.BlockStatement, this.symbolTable, syntax)
        for (let int index = 0; index < syntax.blockStmt.statements.count; index += 1) {
            let SyntaxNode* statement = syntax.blockStmt.statements.nodes[index]
            let BoundNode boundStatement = this.BindStatement(statement)
            BoundNodeArrayPush(& result.children, boundStatement)
        }

        // Pop symboltable
        this.symbolTable = this.symbolTable.parent
        return _FlattenBlockStatementIfNecessary(result)
    }

    BindCaseStatement(switchExpression: BoundNode, syntax: CaseStatementSyntax): BoundNode
    {
        assert(syntax.kind == SyntaxKind.CaseStatement || syntax.kind == SyntaxKind.DefaultStatement)
        assert(this.currentFunctionSymbol != null)
        assert(this.switchCaseLevel != 0)

        let bool isDefault = syntax.kind == SyntaxKind.DefaultStatement

        let BoundNode caseExpression = null
        if (syntax.kind == SyntaxKind.CaseStatement) {
            caseExpression = this.BindExpression(syntax.caseStmt.literalExpression)
            if (caseExpression.kind != BoundNodeKind.IntegerLiteral
                && caseExpression.kind != BoundNodeKind.StringLiteral
                && caseExpression.kind != BoundNodeKind.CharacterLiteral
                && caseExpression.kind != BoundNodeKind.EnumValueLiteral) {
                this.diagnostics.ReportError(
                    caseExpression.token.GetLocation(),
                    `Expected literal in case label but got '${}'`,
                    TokenKindToString(caseExpression.token.kind).cstr
                )
            }
            let TypeConversionResult conversion = CanConvertTypeFromTo(caseExpression.type, switchExpression.type)
            if (conversion != TypeConversionResult.Identical && conversion != TypeConversionResult.ImplictlyConvertible) {
                this.diagnostics.ReportError(
                    caseExpression.token.GetLocation(),
                    `Cannot convert type '${}' of case label literal '${}' to its switch expression type '${}'`,
                    TypeGetText(caseExpression.type).cstr, TokenGetText(caseExpression.token).cstr, TypeGetText(switchExpression.type).cstr
                )
            }
        }

        let BoundNode body = this.BindBlockStatement(isDefault
            ? syntax.defaultStmt.body
            : syntax.caseStmt.body)
        if (body.children.count == 0)
            body = null

        let BoundNode result = BoundNodeCreate2(isDefault
            ? BoundNodeKind.DefaultStatement
            : BoundNodeKind.CaseStatement, this.symbolTable, syntax)
        result.left = body
        result.right = caseExpression
        return result
    }

    BindSwitchStatement(syntax: SwitchStatementSyntax): BoundNode
    {
        if (syntax.kind != SyntaxKind.SwitchStatement)
            throw new Error(`Invalid syntax - expected 'SwitchStatement'`)
        assert(this.currentFunctionSymbol != null)

        let BoundNode switchExpression = this.BindExpression(syntax.switchStmt.switchExpression)

        this.switchCaseLevel += 1

        let int defaultStatementEncountered = false
        let BoundNodeArray caseStatements = BoundNodeArrayCreate()
        for (let int index = 0; index < syntax.switchStmt.caseStatements.count; index += 1) {
            let SyntaxNode* caseStatement = syntax.switchStmt.caseStatements.nodes[index]
            let BoundNode boundCaseStatement = this.BindCaseStatement(switchExpression, caseStatement)
            if (defaultStatementEncountered) {
                this.diagnostics.ReportError(
                    caseStatement.caseStmt.caseKeyword.GetLocation(),
                    `Unexpected case statement after default statement was already defined`
                )
            }
            if (caseStatement.kind == SyntaxKind.DefaultStatement)
                defaultStatementEncountered = true
            BoundNodeArrayPush(& caseStatements, boundCaseStatement)
        }

        this.switchCaseLevel -= 1

        if (caseStatements.count == 0) {
            this.diagnostics.ReportError(
                syntax.switchStmt.switchKeyword.GetLocation(),
                `Empty switch statements are not allowed`
            )
        }

        // Check that we don't have any duplicate case labels
        for (let int index = 0; index < caseStatements.count; index += 1) {
            let BoundNode a = caseStatements.nodes[index]
            for (let int inner = index + 1; inner < caseStatements.count; inner += 1) {
                let BoundNode b = caseStatements.nodes[inner]
                if (a.right && b.right && AreLiteralsEqual(a.right, b.right)) {
                    this.diagnostics.ReportError(
                        b.token.GetLocation(),
                        `Duplicate switch case literal '${}'`,
                        TokenGetText(b.right.token).cstr
                    )
                }
            }
        }

        let BoundNode result = BoundNodeCreate2(BoundNodeKind.SwitchStatement, this.symbolTable, syntax)
        result.left = switchExpression
        result.children = caseStatements
        return result
    }

    BindStatement(syntax: StatementSyntax): BoundNode
    {
        assert(this.currentFunctionSymbol != null)
        switch (syntax.kind) {
            case SyntaxKind.BlockStatement:
                return BindBlockStatement(syntax)
            case SyntaxKind.IfStatement:
                return BindIfStatement(syntax)
            case SyntaxKind.DoWhileStatement:
                return BindDoWhileStatement(syntax)
            case SyntaxKind.WhileStatement:
                return BindWhileStatement(syntax)
            case SyntaxKind.ForStatement:
                return BindForStatement(syntax)
            case SyntaxKind.ReturnStatement:
                return BindReturnStatement(syntax)
            case SyntaxKind.BreakStatement:
                return BindBreakStatement(syntax)
            case SyntaxKind.ContinueStatement:
                return BindContinueStatement(syntax)
            case SyntaxKind.SwitchStatement:
                return BindSwitchStatement(syntax)
            case SyntaxKind.VariableDeclarationStatement:
                return BindVariableDefinitionStatement(syntax, SymbolScopeKind.Local)
            case SyntaxKind.ExpressionStatement:
                return BindExpressionStatement(syntax)
            default:
                assert(false && `Unexpected statement in binder`)
        }
        exit(1)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    // Module

    BindGlobalVariableDefinitionStatement(syntax: GlobalVariableDeclarationStatementSyntax): BoundNode
    {
        assert(this.currentFunctionSymbol == null)
        if (syntax.kind != SyntaxKind.GlobalVariableDeclarationStatement)
            throw new Error(`Invalid syntax - expected 'GlobalVariableDeclarationStatement'`)

        let bool isExternal = syntax.globalVariableStmt.externKeyword.kind == SyntaxKind.ExternKeyword
        let SymbolScopeKind symbolScopeKind = isExternal ? SymbolScopeKind.Extern : SymbolScopeKind.Global
        return BindVariableDefinitionStatement(syntax.globalVariableStmt.variableDeclarationStatement, symbolScopeKind)
    }

    BindStructOrUnionDefinitionStatement(syntax: StructDeclarationStatementSyntax | StructDefinitionStatementSyntax): BoundNode
    {
        assert(this.currentFunctionSymbol == null)
        assert(syntax.kind == SyntaxKind.StructOrUnionDeclarationStatement || syntax.kind == SyntaxKind.StructOrUniontDefinitionStatement)

        let bool isExternal = syntax.structOrUnionDeclarationStmt.externKeyword.kind == SyntaxKind.ExternKeyword
        let SymbolScopeKind symbolScopeKind = isExternal ? SymbolScopeKind.Extern : SymbolScopeKind.Global

        let bool isUnion = syntax.structOrUnionDeclarationStmt.structOrUnionKeyword.kind == SyntaxKind.UnionKeyword

        let SyntaxToken identifier = syntax.structOrUnionDeclarationStmt.identifier
        let String name = TokenGetText(identifier)

        let Symbol* structSymbol = this.symbolTable.GetSymbol(name)
        if (structSymbol != null) {
            if (isUnion && structSymbol.kind != SymbolKind.Union) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Another symbol with the same name '${}' but different type was already declared in current scope`,
                    TokenGetText(identifier).cstr
                )
            }
            if (!isUnion && structSymbol.kind != SymbolKind.Struct) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Another symbol with the same name '${}' but different type was already declared in current scope`,
                    TokenGetText(identifier).cstr
                )
            }
            if (structSymbol.scopeKind != symbolScopeKind) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Struct or union '${} was previously declared but with different scope attribute`,
                    TokenGetText(identifier).cstr
                )
            }
        }
        if (structSymbol == null) {
            let Type type = TypeCreate(isUnion ? TypeKind.Union : TypeKind.Struct, 0, name)
            structSymbol = AddSymbol(this.symbolTable, name, isUnion ? SymbolKind.Union : SymbolKind.Struct, symbolScopeKind, type)
        }

        if (syntax.kind == SyntaxKind.StructOrUnionDeclarationStatement) {
            let BoundNode result = BoundNodeCreate2(isUnion
                ? BoundNodeKind.UnionDeclarationStatement
                : BoundNodeKind.StructDeclarationStatement, this.symbolTable, syntax)
            result.symbol = structSymbol
            return result
        } else {
            structSymbol.membersSymbolTable = SymbolTableCreate(this.symbolTable)

            // Push union/struct member symboltable scope to parse variable declarations as members
            this.symbolTable = structSymbol.membersSymbolTable

            for (let int index = 0; index < syntax.structOrUnionDefinitionStmt.memberDeclarationStatements.count; index += 1) {
                let SyntaxNode* memberDeclaration = syntax.structOrUnionDefinitionStmt.memberDeclarationStatements.nodes[index]
                let BoundNode memberNode = this.BindVariableDefinitionStatement(memberDeclaration, SymbolScopeKind.Local)
                memberNode.symbol.kind = SymbolKind.Member
            }

            // Pop symboltable
            this.symbolTable = this.symbolTable.parent

            if (structSymbol.membersSymbolTable.symbols.length == 0) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Struct or union '${}' needs at least one member`,
                    name.cstr
                )
            }
            if (structSymbol.alreadyDefined) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Duplicate struct or union definition of '${}'`,
                    name.cstr
                )
            }
            structSymbol.alreadyDefined = true

            let BoundNode result = BoundNodeCreate2(isUnion
                ? BoundNodeKind.UnionDefinitionStatement
                : BoundNodeKind.StructDefinitionStatement, this.symbolTable, syntax)
            result.symbol = structSymbol
            return result
        }
    }

    BindEnumDefinitionStatement(syntax: EnumDeclarationStatementSyntax | EnumDeclarationStatementSyntax): BoundNode
    {
        assert(this.currentFunctionSymbol == null)
        assert(syntax.kind == SyntaxKind.EnumDeclarationStatement || syntax.kind == SyntaxKind.EnumDefinitionStatement)

        let bool isExternal = syntax.enumDeclarationStmt.externKeyword.kind == SyntaxKind.ExternKeyword
        let SymbolScopeKind symbolScopeKind = isExternal ? SymbolScopeKind.Extern : SymbolScopeKind.Global

        let SyntaxToken identifier = syntax.enumDeclarationStmt.identifier
        let String name = TokenGetText(identifier)

        if (this.currentFunctionSymbol != null) {
            this.diagnostics.ReportError(
                identifier.GetLocation(),
                `Unexpected enum declaration of '${}' while already parsing function`,
                name.cstr
            )
        }

        let Symbol* enumSymbol = this.symbolTable.GetSymbol(name)
        if (enumSymbol != null) {
            if (enumSymbol.kind != SymbolKind.Enum) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Another symbol with the same name '${}' but different type was already declared in current scope`,
                    TokenGetText(identifier).cstr
                )
            }
            if (enumSymbol.scopeKind != symbolScopeKind) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Enum '${} was previously declared but with different scope attribute`,
                    TokenGetText(identifier).cstr
                )
            }
        }
        if (enumSymbol == null) {
            let Type type = TypeCreate(TypeKind.Enum, 0, name)
            enumSymbol = AddSymbol(this.symbolTable, name, SymbolKind.Enum, symbolScopeKind, type)
        }

        if (syntax.kind == SyntaxKind.EnumDeclarationStatement) {
            let BoundNode result = BoundNodeCreate2(BoundNodeKind.EnumDeclarationStatement, this.symbolTable, syntax)
            result.symbol = enumSymbol
            return result
        } else {
            enumSymbol.membersSymbolTable = SymbolTableCreate(this.symbolTable)

            // Push enum member symboltable scope to parse declarations as members
            this.symbolTable = enumSymbol.membersSymbolTable

            let longint valueCounter = 0
            let Type memberType = TypeCreate(TypeKind.Enum, 0, enumSymbol.name)
            for (let int index = 0; index < syntax.enumDefinitionStmt.memberClauses.count; index += 1) {
                let SyntaxNode* memberClause = syntax.enumDefinitionStmt.memberClauses.nodes[index]

                let String valueName = TokenGetText(memberClause.enumMember.identifier)
                let Symbol* valueSymbol = AddSymbol(this.symbolTable, valueName, SymbolKind.Enumvalue, SymbolScopeKind.Global, memberType)
                if (valueSymbol == null) {
                    this.diagnostics.ReportError(
                        identifier.GetLocation(),
                        `Symbol was '${}' already declared in current scope`,
                        TokenGetText(identifier).cstr
                    )
                }

                if (memberClause.enumMember.integerLiteral.kind == SyntaxKind.IntegerLiteralToken) {
                    let SyntaxToken valueToken = memberClause.enumMember.integerLiteral
                    if (valueToken.intvalue < valueCounter) {
                        this.diagnostics.ReportError(
                            valueToken.GetLocation(),
                            `Assigned value of enum value literal '${}' must chosen such that all enum values of '${}' are unique - chosen value '%lld' would lead to duplicates`,
                            TokenGetText(memberClause.enumMember.identifier).cstr, enumSymbol.name.cstr, valueToken.intvalue
                        )
                    }
                    valueCounter = valueToken.intvalue
                }
                valueSymbol.kind = SymbolKind.Enumvalue
                valueSymbol.enumValue = valueCounter
                valueCounter += 1
            }

            // Pop symboltable
            this.symbolTable = this.symbolTable.parent

            if (enumSymbol.membersSymbolTable.symbols.length == 0) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Enum '${}' needs at least one member`,
                    name.cstr
                )
            }

            if (enumSymbol.alreadyDefined) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Duplicate enum definition of '${}'`,
                    name.cstr
                )
            }
            enumSymbol.alreadyDefined = true

            let BoundNode result = BoundNodeCreate2(BoundNodeKind.EnumDefinitionStatement, this.symbolTable, syntax)
            result.symbol = enumSymbol
            return result
        }
    }

    BindFunctionDefinitionStatement(syntax: FunctionDeclarationStatementSyntax | FunctionDeclarationStatementSyntax): BoundNode
    {
        assert(this.currentFunctionSymbol == null)
        assert(syntax.kind == SyntaxKind.FunctionDeclarationStatement || syntax.kind == SyntaxKind.FunctionDefinitionStatement)

        let Type returnType = this.BindType(syntax.functionDeclarationStmt.returnType)
        let SyntaxToken identifier = syntax.functionDeclarationStmt.identifier

        let bool isExternal = syntax.functionDeclarationStmt.externKeyword.kind == SyntaxKind.ExternKeyword
        let SymbolScopeKind symbolScopeKind = isExternal ? SymbolScopeKind.Extern : SymbolScopeKind.Global

        let Symbol* functionSymbol = this.symbolTable.GetSymbol(TokenGetText(identifier))
        if (functionSymbol != null) {
            if (functionSymbol.kind != SymbolKind.Function) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Another symbol with the same name '${}' but different type was already declared in current scope`,
                    TokenGetText(identifier).cstr
                )
            }
            if (functionSymbol.scopeKind != symbolScopeKind) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Function '${} was previously declared but with different scope attribute`,
                    TokenGetText(identifier).cstr
                )
            }
        }

        if (functionSymbol == null)
            functionSymbol = AddSymbol(this.symbolTable, TokenGetText(identifier), SymbolKind.Function, symbolScopeKind, returnType)

        let SymbolTable* functionParamsSymbolTable = SymbolTableCreate(this.symbolTable)

        // Push function symboltable scope to parse variables as function parameters
        this.symbolTable = functionParamsSymbolTable

        let bool isVariadric = false
        for (let int index = 0; index < syntax.functionDeclarationStmt.params.count; index += 1) {
            let SyntaxNode* param = syntax.functionDeclarationStmt.params.nodes[index]
            if (param.kind == SyntaxKind.DotDotDotToken) {
                isVariadric = true
                assert(index == syntax.functionDeclarationStmt.params.count - 1 && `'...' must be last param`)
                break
            }

            let BoundNode paramNode = this.BindVariableDefinitionStatement(param, SymbolScopeKind.Local)
            paramNode.symbol.kind = SymbolKind.Parameter

            if (param.variableDeclarationStmt.terminatorToken.kind != SyntaxKind.CommaToken) {
                assert(index == syntax.functionDeclarationStmt.params.count - 1 && `param omitting ',' must be last param`)
                break
            }
        }

        // Pop symboltable
        this.symbolTable = this.symbolTable.parent

        if (functionSymbol.membersSymbolTable != null) {
            // The function was already declared before, we need to make sure its types and 
            // parameters match up with the previous defined type and parameters
            if (!TypesIdentical(returnType, functionSymbol.type)) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Return type of function '${}' does not match return type of a previous declaration`,
                    TokenGetText(identifier).cstr
                )
            }

            if (functionSymbol.isVariadric != isVariadric) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Vadriaticity of function '${}' does not match with a previous declaration`,
                    TokenGetText(identifier).cstr
                )
            }

            if (functionParamsSymbolTable.count != functionSymbol.membersSymbolTable.symbols.length) {
                this.diagnostics.ReportError(
                    syntax.functionDeclarationStmt.leftParen.GetLocation(),
                    `Function '${}' was previously declared with ${} parameters wheras new declaration has ${} parameters`,
                    functionSymbol.name.cstr, functionSymbol.membersSymbolTable.symbols.length, functionParamsSymbolTable.count
                )
            }

            for (let int paramIndex = 0; paramIndex < functionParamsSymbolTable.count; paramIndex += 1) {
                let Type paramType = functionParamsSymbolTable.symbols[paramIndex].type
                let Type previosType = functionSymbol.membersSymbolTable.symbols[paramIndex].type

                if (!TypesIdentical(paramType, previosType)) {
                    this.diagnostics.ReportError(
                        syntax.functionDeclarationStmt.leftParen.GetLocation(),
                        `Previous function '${}' parameter ${} declared type differs from current declared type`,
                        functionSymbol.name.cstr, paramIndex + 1
                    )
                }
            }
        }
        functionSymbol.membersSymbolTable = functionParamsSymbolTable
        functionSymbol.isVariadric = isVariadric

        let BoundNode body = null
        if (syntax.kind == SyntaxKind.FunctionDefinitionStatement) {
            if (isExternal) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Cannot define external function '${}'`,
                    TokenGetText(identifier).cstr
                )
            }
            if (functionSymbol.alreadyDefined) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Duplicate function definition of '${}'`,
                    TokenGetText(identifier).cstr
                )
            }
            // Bind function body
            this.symbolTable = functionSymbol.membersSymbolTable
            this.currentFunctionSymbol = functionSymbol
            body = this.BindBlockStatement(syntax.functionDefinitionStmt.body)
            // TODO: make sure function returns something if its return type is not void
            functionSymbol.alreadyDefined = true
            this.currentFunctionSymbol = null
            this.symbolTable = this.symbolTable.parent
        }

        let BoundNode result = BoundNodeCreate2(syntax.kind == SyntaxKind.FunctionDeclarationStatement
            ? BoundNodeKind.FunctionDeclarationStatement
            : BoundNodeKind.FunctionDefinitionStatement,
            this.symbolTable, syntax)
        result.symbol = functionSymbol
        result.left = body
        return result
    }

    BindModuleStatement(syntax: ModuleMemberSyntax): BoundNode
    {
        switch (syntax.kind) {
            case SyntaxKind.ImportDeclarationStatement:
                return null // We ignore these as they are only relevant for the binder
            case SyntaxKind.GlobalVariableDeclarationStatement:
                return BindGlobalVariableDefinitionStatement(syntax)
            case SyntaxKind.EnumDeclarationStatement:
            case SyntaxKind.EnumDefinitionStatement:
                return BindEnumDefinitionStatement(syntax)
            case SyntaxKind.StructOrUnionDeclarationStatement:
            case SyntaxKind.StructOrUniontDefinitionStatement:
                return BindStructOrUnionDefinitionStatement(syntax)
            case SyntaxKind.FunctionDeclarationStatement:
            case SyntaxKind.FunctionDefinitionStatement:
                return BindFunctionDefinitionStatement(syntax)
            default:
                assert(false && `Unexpected module statement in binder`)
        }
        return null
    }

    BindModule(syntax: ModuleSyntax): BoundNode
    {
        assert(syntax.info.kind == SyntaxKind.Module)

        let BoundNode result = BoundNodeCreate2(BoundNodeKind.Module, this.symbolTable, (as SyntaxNode *)syntax)
        for (let int index = 0; index < syntax.globalStatements.count; index += 1) {
            let SyntaxNode* statement = syntax.globalStatements.nodes[index]
            let BoundNode boundStatement = this.BindModuleStatement(statement)
            if (boundStatement != null)
                BoundNodeArrayPush(& result.children, boundStatement)
        }
        return result
    }

    BindCompilationUnit(trees: SyntaxTree[]): BoundNode
    {
        let BoundNode result = BoundNodeCreate2(BoundNodeKind.Module, this.symbolTable, null)
        for (let int index = 0; index < trees.count; index += 1) {
            let SyntaxTree* tree = trees.trees[index]
            let ModuleStatementSyntax* syntax = tree.moduleRoot
            for (let int index = 0; index < syntax.globalStatements.count; index += 1) {
                let SyntaxNode* statement = syntax.globalStatements.nodes[index]
                let BoundNode boundStatement = this.BindModuleStatement(statement)
                if (boundStatement != null)
                    BoundNodeArrayPush(& result.children, boundStatement)
            }
        }
        return result
    }
}