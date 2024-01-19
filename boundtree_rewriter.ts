// deno-lint-ignore-file prefer-const

import { BoundNodeKind, BoundTypeCastExpression, BoundDoWhileStatement, BoundReturnStatement, BoundContinueStatement, BoundBreakStatement, BoundSwitchStatement, BoundCaseStatement, BoundMissingExpression, BoundArrayLiteral, BoundNameExpression, BoundPrimitiveLiteral, BoundFunctionCallExpression, BoundMemberAccessExpression, BoundArrayIndexExpression, BoundParenthesizedExpression, BoundTernaryConditionalExpression, } from "./boundtree.ts"
import { BoundExpression, BoundBinaryExpression, BoundUnaryExpression, BoundStatement, BoundBlockStatement, BoundExpressionStatement, BoundVariableDeclarationStatement, BoundIfStatement, BoundWhileStatement, BoundForStatement } from "./boundtree.ts"
import { Type } from "./types.ts"

export abstract class BoundTreeTraverser
{
    ////////////////////////////////////////////////////////////////////////////////////////////
    // Statements

    // Should not be derived
    protected TraverseStatement(node: BoundStatement): BoundStatement
    {
        switch (node.kind) {
            case BoundNodeKind.ExpressionStatement:
                return this.TraverseExpressionStatement(node as BoundExpressionStatement)
            case BoundNodeKind.BlockStatement:
                return this.TraverseBlockStatement(node as BoundBlockStatement)
            case BoundNodeKind.VariableDeclarationStatement:
                return this.TraverseVariableDeclarationStatement(node as BoundVariableDeclarationStatement)
            case BoundNodeKind.IfStatement:
                return this.TraverseIfStatement(node as BoundIfStatement)
            case BoundNodeKind.WhileStatement:
                return this.TraverseWhileStatement(node as BoundWhileStatement)
            case BoundNodeKind.DoWhileStatement:
                return this.TraverseDoWhileStatement(node as BoundDoWhileStatement)
            case BoundNodeKind.ForStatement:
                return this.TraverseForStatement(node as BoundForStatement)
            case BoundNodeKind.ReturnStatement:
                return this.TraverseReturnStatement(node as BoundReturnStatement)
            case BoundNodeKind.BreakStatement:
                return this.TraverseBreakStatement(node as BoundBreakStatement)
            case BoundNodeKind.ContinueStatement:
                return this.TraverseContinueStatement(node as BoundContinueStatement)
            case BoundNodeKind.SwitchStatement:
                return this.TraverseSwitchStatement(node as BoundSwitchStatement)
            default:
                throw new Error(`Unexpected statement in rewriter: ${node.kind}`)
        }
    }

    protected TraverseVariableDeclarationStatement(node: BoundVariableDeclarationStatement): BoundStatement
    {
        if (node.initializer == null)
            return node

        let initializer = this.TraverseExpression(node.initializer)
        if (initializer == node.initializer)
            return node

        return new BoundVariableDeclarationStatement(node.syntax, node.symbolTable, node.symbol, initializer)
    }

    protected TraverseExpressionStatement(node: BoundExpressionStatement): BoundStatement
    {
        let expression = this.TraverseExpression(node.expression)
        if (expression == node.expression)
            return node

        return new BoundExpressionStatement(node.syntax, node.symbolTable, expression)
    }

    protected TraverseBlockStatement(node: BoundBlockStatement): BoundStatement
    {
        let newStatements: BoundStatement[] = []
        for (let statement of node.statements) {
            let newStatement = this.TraverseStatement(statement)
            newStatements.push(newStatement)
        }

        let changed = false
        for (let index = 0; index < node.statements.length; index++) {
            if (node.statements[index] != newStatements[index]) {
                changed = true
                break
            }
        }

        if (!changed)
            return node

        return new BoundBlockStatement(node.syntax, node.symbolTable, newStatements)
    }

    protected TraverseIfStatement(node: BoundIfStatement): BoundStatement
    {
        let condition = this.TraverseExpression(node.condition)
        let thenStatement = this.TraverseStatement(node.thenStatement)
        let elseStatement = node.elseStatement == null ? null : this.TraverseStatement(node.elseStatement)
        if (condition == node.condition && thenStatement == node.thenStatement && elseStatement == node.elseStatement)
            return node

        return new BoundIfStatement(node.syntax, node.symbolTable, condition, thenStatement, elseStatement)
    }

    protected TraverseWhileStatement(node: BoundWhileStatement): BoundStatement
    {
        let condition = this.TraverseExpression(node.condition)
        let body = this.TraverseStatement(node.body)
        if (condition == node.condition && body == node.body)
            return node

        return new BoundWhileStatement(node.syntax, node.symbolTable, condition, body)
    }

    protected TraverseDoWhileStatement(node: BoundDoWhileStatement): BoundStatement
    {
        let body = this.TraverseStatement(node.body)
        let condition = this.TraverseExpression(node.condition)
        if (condition == node.condition && body == node.body)
            return node

        return new BoundDoWhileStatement(node.syntax, node.symbolTable, condition, body)
    }

    protected TraverseForStatement(node: BoundForStatement): BoundStatement
    {
        let lowerBound = this.TraverseExpression(node.lowerBound)
        let upperBound = this.TraverseExpression(node.upperBound)
        let body = this.TraverseStatement(node.body)
        if (lowerBound == node.lowerBound && upperBound == node.upperBound && body == node.body)
            return node

        return new BoundForStatement(node.syntax, node.symbolTable, node.iteratorSymbol, lowerBound, upperBound, node.upperBoundIsInclusive, body)
    }

    protected TraverseReturnStatement(node: BoundReturnStatement): BoundStatement
    {
        if (node.returnExpression == null)
            return node

        let returnExpression = this.TraverseExpression(node.returnExpression)
        if (returnExpression == node.returnExpression)
            return node

        return new BoundReturnStatement(node.syntax, node.symbolTable, returnExpression)
    }

    protected TraverseBreakStatement(node: BoundBreakStatement): BoundStatement
    {
        return node
    }

    protected TraverseContinueStatement(node: BoundContinueStatement): BoundStatement
    {
        return node
    }

    protected TraverseSwitchStatement(node: BoundSwitchStatement): BoundStatement
    {
        let switchExpression = this.TraverseExpression(node.switchExpression)

        let newCaseStatements: BoundCaseStatement[] = []
        for (let caseStatement of node.caseStatements) {
            let newCaseStatement = this.TraverseCaseStatement(caseStatement)
            newCaseStatements.push(newCaseStatement)
        }

        let changed = switchExpression != node.switchExpression
        for (let index = 0; index < node.caseStatements.length; index++) {
            if (node.caseStatements[index] != newCaseStatements[index]) {
                changed = true
                break
            }
        }

        if (!changed)
            return node

        return new BoundSwitchStatement(node.syntax, node.symbolTable, switchExpression, newCaseStatements)
    }

    protected TraverseCaseStatement(node: BoundCaseStatement): BoundCaseStatement
    {
        let caseExpression = node.caseExpression == null ? null : this.TraverseExpression(node.caseExpression)
        let body = node.body == null ? null : this.TraverseBlockStatement(node.body)
        if (caseExpression == node.caseExpression && body == node.body)
            return node

        if (body instanceof BoundBlockStatement || body == null) {
            return new BoundCaseStatement(node.syntax, node.symbolTable, caseExpression, body)
        } else {
            let block = new BoundBlockStatement(body.syntax, body.symbolTable, [body])
            return new BoundCaseStatement(node.syntax, node.symbolTable, caseExpression, block)
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////
    // Expressions

    // Should not be derived
    protected TraverseExpression(node: BoundExpression): BoundExpression
    {
        switch (node.kind) {
            case BoundNodeKind.MissingExpression:
                return this.TraverseMissingExpression(node as BoundMissingExpression)
            case BoundNodeKind.TypeCastExpression:
                return this.TraverseTypeCastExpression(node as BoundTypeCastExpression)
            case BoundNodeKind.ParenthesizedExpression:
                return this.TraverseParenthesizedExpression(node as BoundParenthesizedExpression)
            case BoundNodeKind.NameExpression:
                return this.TraverseNameExpression(node as BoundNameExpression)
            case BoundNodeKind.UnaryExpression:
                return this.TraverseUnaryExpression(node as BoundUnaryExpression)
            case BoundNodeKind.BinaryExpression:
                return this.TraverseBinaryExpression(node as BoundBinaryExpression)
            case BoundNodeKind.TernaryConditionalExpression:
                return this.TraverseTernaryConditionalExpression(node as BoundTernaryConditionalExpression)
            case BoundNodeKind.FunctionCallExpression:
                return this.TraverseFunctionCallExpression(node as BoundFunctionCallExpression)
            case BoundNodeKind.ArrayIndexExpression:
                return this.TraverseArrayIndexExpression(node as BoundArrayIndexExpression)
            case BoundNodeKind.MemberAccessExpression:
                return this.TraverseMemberAccessExpression(node as BoundMemberAccessExpression)
            case BoundNodeKind.NullLiteral:
            case BoundNodeKind.BoolLiteral:
            case BoundNodeKind.NumberLiteral:
            case BoundNodeKind.StringLiteral:
                return this.TraversePrimitiveLiteral(node as BoundPrimitiveLiteral)
            case BoundNodeKind.ArrayLiteral:
                return this.TraverseArrayLiteral(node as BoundArrayLiteral)
            default:
                throw new Error(`Unexpected expression in rewriter: ${node.kind}`)
        }
    }

    protected TraverseMissingExpression(node: BoundMissingExpression): BoundExpression
    {
        return node
    }

    protected TraverseNameExpression(node: BoundNameExpression): BoundExpression
    {
        return node
    }

    protected TraverseParenthesizedExpression(node: BoundParenthesizedExpression): BoundExpression
    {
        let inner = this.TraverseExpression(node.inner)
        if (inner == node.inner)
            return node

        return new BoundParenthesizedExpression(node.syntax, node.symbolTable, inner)
    }

    protected TraverseUnaryExpression(node: BoundUnaryExpression): BoundExpression
    {
        let operand = this.TraverseExpression(node.operand)
        if (operand == node.operand)
            return node

        return new BoundUnaryExpression(node.syntax, node.symbolTable, node.operator, operand, node.resultType, node.resultIsRValue)
    }

    protected TraverseBinaryExpression(node: BoundBinaryExpression): BoundExpression
    {
        let left = this.TraverseExpression(node.left)
        let right = this.TraverseExpression(node.right)
        if (left == node.left && right == node.right)
            return node

        return new BoundBinaryExpression(node.syntax, node.symbolTable, node.operator, left, right, node.resultType, node.resultIsRValue)
    }

    protected TraverseTernaryConditionalExpression(node: BoundTernaryConditionalExpression): BoundExpression
    {
        let condition = this.TraverseExpression(node.condition)
        let thenExpression = this.TraverseExpression(node.thenExpression)
        let elseExpression = this.TraverseExpression(node.elseExpression)
        if (condition == node.condition && thenExpression == node.thenExpression && elseExpression == node.elseExpression)
            return node

        if (!Type.Identical(thenExpression.type, elseExpression.type)) {
            throw new Error("Ternary expression types must be identical")
        }
        return new BoundTernaryConditionalExpression(node.syntax, node.symbolTable, condition, thenExpression, elseExpression)
    }

    protected TraverseFunctionCallExpression(node: BoundFunctionCallExpression): BoundExpression
    {
        let newLeft = this.TraverseExpression(node.left)
        let newArgs: BoundExpression[] = []
        for (let arg of node.args) {
            let newArg = this.TraverseExpression(arg)
            newArgs.push(newArg)
        }

        let changed = newLeft != node.left
        for (let index = 0; index < node.args.length; index++) {
            if (node.args[index] != newArgs[index]) {
                changed = true
                break
            }
        }

        if (!changed)
            return node

        return new BoundFunctionCallExpression(node.syntax, node.symbolTable, newLeft, node.symbol!, node.isConstructor, newArgs)
    }

    protected TraverseTypeCastExpression(node: BoundTypeCastExpression): BoundExpression
    {
        let expression = this.TraverseExpression(node.expression)
        if (expression == node.expression)
            return node

        return new BoundTypeCastExpression(node.syntax, node.symbolTable, node.type, expression)
    }

    protected TraverseMemberAccessExpression(node: BoundMemberAccessExpression): BoundMemberAccessExpression
    {
        let container = this.TraverseExpression(node.container)
        if (container == node.container)
            return node
        return new BoundMemberAccessExpression(node.syntax, node.symbolTable, container, node.memberSymbol)
    }

    protected TraverseArrayIndexExpression(node: BoundArrayIndexExpression): BoundArrayIndexExpression
    {
        let array = this.TraverseExpression(node.array)
        let index = this.TraverseExpression(node.index)
        if (array == node.array && index == node.index)
            return node

        if (!array.type.IsArray())
            throw new Error("Expected array type after rewrite")

        let elemType = array.type.GetInnerType()
        return new BoundArrayIndexExpression(node.syntax, node.symbolTable, elemType, array, index)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    // Literals

    protected TraversePrimitiveLiteral(node: BoundPrimitiveLiteral): BoundExpression
    {
        return node
    }

    protected TraverseArrayLiteral(node: BoundArrayLiteral): BoundExpression
    {
        let newValues: BoundExpression[] = []
        for (let value of node.values) {
            let newValue = this.TraverseExpression(value)
            newValues.push(newValue)
        }

        let changed = false
        for (let index = 0; index < node.values.length; index++) {
            if (node.values[index] != newValues[index]) {
                changed = true
                break
            }
        }

        if (!changed)
            return node

        // TODO: Do we error check here if new array values have the same type??
        return new BoundArrayLiteral(node.syntax, node.symbolTable, Type.Any, newValues)
    }
}
