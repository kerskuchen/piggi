// deno-lint-ignore-file prefer-const

import { BoundUnaryOperatorKind, BoundBinaryOperatorKind } from "./boundtree.ts"
import { DiagnosticBag } from "./common.ts"
import { SyntaxKind, SyntaxToken } from "./syntax.ts"
import { Type, TypeConversionResult } from "./types.ts"


////////////////////////////////////////////////////////////////////////////////////////////////////
// Unary operators

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

        if (operandType.IsNumber()) {
            if (token.kind == SyntaxKind.PlusToken)
                operatorKind = BoundUnaryOperatorKind.Identity
            if (token.kind == SyntaxKind.MinusToken)
                operatorKind = BoundUnaryOperatorKind.Negation
            if (token.kind == SyntaxKind.TildeToken)
                operatorKind = BoundUnaryOperatorKind.BitwiseNegation
        }
        if (operandType.IsBool()) {
            if (token.kind == SyntaxKind.BangToken)
                operatorKind = BoundUnaryOperatorKind.LogicalNegation
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

////////////////////////////////////////////////////////////////////////////////////////////////////
// Binary operators

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
            let conversion = rightType.ConvertTo(leftType)
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

        if (leftType.IsNullable() && rightType.IsNull() || leftType.IsNull() && rightType.IsNullable()) {
            if (token.kind == SyntaxKind.EqualsEqualsToken) {
                operatorKind = BoundBinaryOperatorKind.Equals
                resultType = Type.Bool
            }
            if (token.kind == SyntaxKind.BangEqualsToken) {
                operatorKind = BoundBinaryOperatorKind.NotEquals
                resultType = Type.Bool
            }
        }

        if (leftType.IsEnum() && rightType.IsEnum() || leftType.IsPrimitive() && rightType.IsPrimitive()) {
            switch (token.kind) {
                case SyntaxKind.EqualsEqualsToken: {
                    operatorKind = BoundBinaryOperatorKind.Equals
                    resultType = Type.Bool
                    break
                }
                case SyntaxKind.BangEqualsToken: {
                    operatorKind = BoundBinaryOperatorKind.NotEquals
                    resultType = Type.Bool
                    break
                }
                case SyntaxKind.LessToken: {
                    operatorKind = BoundBinaryOperatorKind.Less
                    resultType = Type.Bool
                    break
                }
                case SyntaxKind.LessEqualsToken: {
                    operatorKind = BoundBinaryOperatorKind.LessEquals
                    resultType = Type.Bool
                    break
                }
                case SyntaxKind.GreaterToken: {
                    operatorKind = BoundBinaryOperatorKind.Greater
                    resultType = Type.Bool
                    break
                }
                case SyntaxKind.GreaterEqualsToken: {
                    operatorKind = BoundBinaryOperatorKind.GreaterEquals
                    resultType = Type.Bool
                    break
                }
            }
        }

        if (token.kind == SyntaxKind.AmpersandAmpersandToken
            && leftType.IsBool() && rightType.IsBool()) {
            operatorKind = BoundBinaryOperatorKind.LogicalAnd
        }
        if (token.kind == SyntaxKind.PipePipeToken
            && leftType.IsBool() && rightType.IsBool()) {
            operatorKind = BoundBinaryOperatorKind.LogicalOr
        }

        if (leftType.IsString() && rightType.IsPrimitive()) {
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
            }
        }

        if (leftType.IsNumber() && rightType.IsNumber()) {
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
            `No applicable binary operation for combination token '${token.GetText()}', left type '${leftType.PrettyPrint()}', right type '${rightType.PrettyPrint()}'`,
        )
        return null
    }
}