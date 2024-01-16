// deno-lint-ignore-file prefer-const

import { BoundNodeKind, BoundTypeCastExpression, BoundDoWhileStatement, BoundReturnStatement, BoundContinueStatement, BoundBreakStatement, BoundMissingStatement, BoundEnumDeclaration, BoundFunctionDeclaration, BoundImportDeclaration, BoundStructDeclaration, BoundCompilationUnit, BoundSwitchStatement, BoundCaseStatement, BoundMissingExpression, BoundArrayLiteral, BoundEnumValueLiteral, BoundNameExpression, BoundPrimitiveLiteral, BoundFunctionCallExpression, BoundMemberAccessExpression, BoundArrayIndexExpression, BoundParenthesizedExpression, BoundTernaryConditionalExpression, } from "./boundtree.ts"
import { BoundExpression, BoundBinaryExpression, BoundUnaryExpression, BoundStatement, BoundBlockStatement, BoundExpressionStatement, BoundVariableDeclaration, BoundIfStatement, BoundWhileStatement, BoundForStatement } from "./boundtree.ts"
import { Type } from "./types.ts"

export abstract class BoundTreeRewriter
{
    ////////////////////////////////////////////////////////////////////////////////////////////
    // Compilation Unit

    // Should not be derived
    public RewriteCompilationUnit(node: BoundCompilationUnit): BoundCompilationUnit
    {
        let newDeclarations: BoundStatement[] = []
        for (let declaration of node.globalDeclarations) {
            let newDeclaration = this.RewriteModuleStatement(declaration)
            newDeclarations.push(newDeclaration)
        }

        let changed = false
        for (let index = 0; index < node.globalDeclarations.length; index++) {
            if (node.globalDeclarations[index] != newDeclarations[index]) {
                changed = true
                break
            }
        }

        if (!changed)
            return node

        return new BoundCompilationUnit(node.symbolTable, newDeclarations)
    }

    // Should not be derived
    protected RewriteModuleStatement(node: BoundStatement): BoundStatement
    {
        switch (node.kind) {
            case BoundNodeKind.ImportDeclaration:
                return this.RewriteImportDeclaration(node as BoundImportDeclaration)
            case BoundNodeKind.VariableDeclaration:
                return this.RewriteVariableDeclaration(node as BoundVariableDeclaration)
            case BoundNodeKind.EnumDeclaration:
                return this.RewriteEnumDeclaration(node as BoundEnumDeclaration)
            case BoundNodeKind.StructDeclaration:
                return this.RewriteStructDeclaration(node as BoundStructDeclaration)
            case BoundNodeKind.FunctionDeclaration:
                return this.RewriteFunctionDeclaration(node as BoundFunctionDeclaration)
            default:
                throw new Error(`Unexpected module declaration in rewriter: ${node.kind}`)
        }
    }

    protected RewriteMissingStatement(node: BoundMissingStatement): BoundStatement
    {
        return node
    }

    protected RewriteImportDeclaration(node: BoundImportDeclaration): BoundStatement
    {
        return node
    }

    protected RewriteStructDeclaration(node: BoundStructDeclaration): BoundStatement
    {
        return node
    }

    protected RewriteEnumDeclaration(node: BoundEnumDeclaration): BoundStatement
    {
        return node
    }

    protected RewriteFunctionDeclaration(node: BoundFunctionDeclaration): BoundStatement
    {
        if (node.body == null)
            return node

        let body = this.RewriteBlockStatement(node.body)
        if (body == node.body)
            return node

        if (body instanceof BoundBlockStatement) {
            return new BoundFunctionDeclaration(node.syntax, node.symbolTable, node.symbol, body)
        } else {
            let block = new BoundBlockStatement(body.syntax, body.symbolTable, [body])
            return new BoundFunctionDeclaration(node.syntax, node.symbolTable, node.symbol, block)
        }
    }

    protected RewriteVariableDeclaration(node: BoundVariableDeclaration): BoundStatement
    {
        if (node.initializer == null)
            return node

        let initializer = this.RewriteExpression(node.initializer)
        if (initializer == node.initializer)
            return node

        return new BoundVariableDeclaration(node.syntax, node.symbolTable, node.symbol, initializer)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////
    // Statements

    // Should not be derived
    protected RewriteStatement(node: BoundStatement): BoundStatement
    {
        switch (node.kind) {
            case BoundNodeKind.ExpressionStatement:
                return this.RewriteExpressionStatement(node as BoundExpressionStatement)
            case BoundNodeKind.BlockStatement:
                return this.RewriteBlockStatement(node as BoundBlockStatement)
            case BoundNodeKind.VariableDeclaration:
                return this.RewriteVariableDeclaration(node as BoundVariableDeclaration)
            case BoundNodeKind.IfStatement:
                return this.RewriteIfStatement(node as BoundIfStatement)
            case BoundNodeKind.WhileStatement:
                return this.RewriteWhileStatement(node as BoundWhileStatement)
            case BoundNodeKind.DoWhileStatement:
                return this.RewriteDoWhileStatement(node as BoundDoWhileStatement)
            case BoundNodeKind.ForStatement:
                return this.RewriteForStatement(node as BoundForStatement)
            case BoundNodeKind.ReturnStatement:
                return this.RewriteReturnStatement(node as BoundReturnStatement)
            case BoundNodeKind.BreakStatement:
                return this.RewriteBreakStatement(node as BoundBreakStatement)
            case BoundNodeKind.ContinueStatement:
                return this.RewriteContinueStatement(node as BoundContinueStatement)
            case BoundNodeKind.SwitchStatement:
                return this.RewriteSwitchStatement(node as BoundSwitchStatement)
            default:
                throw new Error(`Unexpected statement in rewriter: ${node.kind}`)
        }
    }

    protected RewriteExpressionStatement(node: BoundExpressionStatement): BoundStatement
    {
        let expression = this.RewriteExpression(node.expression)
        if (expression == node.expression)
            return node

        return new BoundExpressionStatement(node.syntax, node.symbolTable, expression)
    }

    protected RewriteBlockStatement(node: BoundBlockStatement): BoundStatement
    {
        let newStatements: BoundStatement[] = []
        for (let statement of node.statements) {
            let newStatement = this.RewriteStatement(statement)
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

    protected RewriteIfStatement(node: BoundIfStatement): BoundStatement
    {
        let condition = this.RewriteExpression(node.condition)
        let thenStatement = this.RewriteStatement(node.thenStatement)
        let elseStatement = node.elseStatement == null ? null : this.RewriteStatement(node.elseStatement)
        if (condition == node.condition && thenStatement == node.thenStatement && elseStatement == node.elseStatement)
            return node

        return new BoundIfStatement(node.syntax, node.symbolTable, condition, thenStatement, elseStatement)
    }

    protected RewriteWhileStatement(node: BoundWhileStatement): BoundStatement
    {
        let condition = this.RewriteExpression(node.condition)
        let body = this.RewriteStatement(node.body)
        if (condition == node.condition && body == node.body)
            return node

        return new BoundWhileStatement(node.syntax, node.symbolTable, condition, body)
    }

    protected RewriteDoWhileStatement(node: BoundDoWhileStatement): BoundStatement
    {
        let body = this.RewriteStatement(node.body)
        let condition = this.RewriteExpression(node.condition)
        if (condition == node.condition && body == node.body)
            return node

        return new BoundDoWhileStatement(node.syntax, node.symbolTable, condition, body)
    }

    protected RewriteForStatement(node: BoundForStatement): BoundStatement
    {
        let lowerBound = this.RewriteExpression(node.lowerBound)
        let upperBound = this.RewriteExpression(node.upperBound)
        let body = this.RewriteStatement(node.body)
        if (lowerBound == node.lowerBound && upperBound == node.upperBound && body == node.body)
            return node

        return new BoundForStatement(node.syntax, node.symbolTable, node.iteratorSymbol, lowerBound, upperBound, node.upperBoundIsInclusive, body)
    }

    protected RewriteReturnStatement(node: BoundReturnStatement): BoundStatement
    {
        if (node.returnExpression == null)
            return node

        let returnExpression = this.RewriteExpression(node.returnExpression)
        if (returnExpression == node.returnExpression)
            return node

        return new BoundReturnStatement(node.syntax, node.symbolTable, returnExpression)
    }

    protected RewriteBreakStatement(node: BoundBreakStatement): BoundStatement
    {
        return node
    }

    protected RewriteContinueStatement(node: BoundContinueStatement): BoundStatement
    {
        return node
    }

    protected RewriteSwitchStatement(node: BoundSwitchStatement): BoundStatement
    {
        let switchExpression = this.RewriteExpression(node.switchExpression)

        let newCaseStatements: BoundCaseStatement[] = []
        for (let caseStatement of node.caseStatements) {
            let newCaseStatement = this.RewriteCaseStatement(caseStatement)
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

    protected RewriteCaseStatement(node: BoundCaseStatement): BoundCaseStatement
    {
        let caseExpression = node.caseExpression == null ? null : this.RewriteExpression(node.caseExpression)
        let body = node.body == null ? null : this.RewriteBlockStatement(node.body)
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
    protected RewriteExpression(node: BoundExpression): BoundExpression
    {
        switch (node.kind) {
            case BoundNodeKind.MissingExpression:
                return this.RewriteMissingExpression(node as BoundMissingExpression)
            case BoundNodeKind.TypeCastExpression:
                return this.RewriteTypeCastExpression(node as BoundTypeCastExpression)
            case BoundNodeKind.ParenthesizedExpression:
                return this.RewriteParenthesizedExpression(node as BoundParenthesizedExpression)
            case BoundNodeKind.NameExpression:
                return this.RewriteNameExpression(node as BoundNameExpression)
            case BoundNodeKind.UnaryExpression:
                return this.RewriteUnaryExpression(node as BoundUnaryExpression)
            case BoundNodeKind.BinaryExpression:
                return this.RewriteBinaryExpression(node as BoundBinaryExpression)
            case BoundNodeKind.TernaryConditionalExpression:
                return this.RewriteTernaryConditionalExpression(node as BoundTernaryConditionalExpression)
            case BoundNodeKind.FunctionCallExpression:
                return this.RewriteFunctionCallExpression(node as BoundFunctionCallExpression)
            case BoundNodeKind.ArrayIndexExpression:
                return this.RewriteArrayIndexExpression(node as BoundArrayIndexExpression)
            case BoundNodeKind.MemberAccessExpression:
                return this.RewriteMemberAccessExpression(node as BoundMemberAccessExpression)
            case BoundNodeKind.NullLiteral:
            case BoundNodeKind.BoolLiteral:
            case BoundNodeKind.NumberLiteral:
            case BoundNodeKind.StringLiteral:
                return this.RewritePrimitiveLiteral(node as BoundPrimitiveLiteral)
            case BoundNodeKind.EnumValueLiteral:
                return this.RewriteEnumValueLiteral(node as BoundEnumValueLiteral)
            case BoundNodeKind.ArrayLiteral:
                return this.RewriteArrayLiteral(node as BoundArrayLiteral)
            default:
                throw new Error(`Unexpected expression in rewriter: ${node.kind}`)
        }
    }

    protected RewriteMissingExpression(node: BoundMissingExpression): BoundExpression
    {
        return node
    }

    protected RewriteNameExpression(node: BoundNameExpression): BoundExpression
    {
        return node
    }

    protected RewriteParenthesizedExpression(node: BoundParenthesizedExpression): BoundExpression
    {
        let inner = this.RewriteExpression(node.inner)
        if (inner == node.inner)
            return node

        return new BoundParenthesizedExpression(node.syntax, node.symbolTable, inner)
    }

    protected RewriteUnaryExpression(node: BoundUnaryExpression): BoundExpression
    {
        let operand = this.RewriteExpression(node.operand)
        if (operand == node.operand)
            return node

        return new BoundUnaryExpression(node.syntax, node.symbolTable, node.operator, operand, node.resultType, node.resultIsRValue)
    }

    protected RewriteBinaryExpression(node: BoundBinaryExpression): BoundExpression
    {
        let left = this.RewriteExpression(node.left)
        let right = this.RewriteExpression(node.right)
        if (left == node.left && right == node.right)
            return node

        return new BoundBinaryExpression(node.syntax, node.symbolTable, node.operator, left, right, node.resultType, node.resultIsRValue)
    }

    protected RewriteTernaryConditionalExpression(node: BoundTernaryConditionalExpression): BoundExpression
    {
        let condition = this.RewriteExpression(node.condition)
        let thenExpression = this.RewriteExpression(node.thenExpression)
        let elseExpression = this.RewriteExpression(node.elseExpression)
        if (condition == node.condition && thenExpression == node.thenExpression && elseExpression == node.elseExpression)
            return node

        if (!Type.Identical(thenExpression.type, elseExpression.type)) {
            throw new Error("Ternary expression types must be identical")
        }
        return new BoundTernaryConditionalExpression(node.syntax, node.symbolTable, condition, thenExpression, elseExpression)
    }

    protected RewriteFunctionCallExpression(node: BoundFunctionCallExpression): BoundExpression
    {
        let newArgs: BoundExpression[] = []
        for (let arg of node.args) {
            let newArg = this.RewriteExpression(arg)
            newArgs.push(newArg)
        }

        let changed = false
        for (let index = 0; index < node.args.length; index++) {
            if (node.args[index] != newArgs[index]) {
                changed = true
                break
            }
        }

        if (!changed)
            return node

        return new BoundFunctionCallExpression(node.syntax, node.symbolTable, node.symbol!, node.isConstructor, newArgs)
    }

    protected RewriteTypeCastExpression(node: BoundTypeCastExpression): BoundExpression
    {
        let expression = this.RewriteExpression(node.expression)
        if (expression == node.expression)
            return node

        return new BoundTypeCastExpression(node.syntax, node.symbolTable, node.type, expression)
    }

    protected RewriteMemberAccessExpression(node: BoundMemberAccessExpression): BoundMemberAccessExpression
    {
        let container = this.RewriteExpression(node.container)
        if (container == node.container)
            return node
        return new BoundMemberAccessExpression(node.syntax, node.symbolTable, container, node.memberSymbol)
    }

    protected RewriteArrayIndexExpression(node: BoundArrayIndexExpression): BoundArrayIndexExpression
    {
        let array = this.RewriteExpression(node.array)
        let index = this.RewriteExpression(node.index)
        if (array == node.array && index == node.index)
            return node

        if (!array.type.IsArray())
            throw new Error("Expected array type after rewrite")

        let elemType = array.type.GetInnerType()
        return new BoundArrayIndexExpression(node.syntax, node.symbolTable, elemType, array, index)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    // Literals

    protected RewritePrimitiveLiteral(node: BoundPrimitiveLiteral): BoundExpression
    {
        return node
    }

    protected RewriteEnumValueLiteral(node: BoundEnumValueLiteral): BoundExpression
    {
        return node
    }

    protected RewriteArrayLiteral(node: BoundArrayLiteral): BoundExpression
    {
        let newValues: BoundExpression[] = []
        for (let value of node.values) {
            let newValue = this.RewriteExpression(value)
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
