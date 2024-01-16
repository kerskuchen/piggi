// deno-lint-ignore-file prefer-const

import { BoundCompilationUnit, BoundStatement, BoundBlockStatement, BoundVariableDeclaration, BoundIfStatement, BoundBinaryExpression, BoundNameExpression, BoundPrimitiveLiteral, BoundNodeKind, BoundBinaryOperatorKind, BoundExpressionStatement } from "./boundtree.ts"
import { BoundTreeRewriter } from "./boundtree_rewriter.ts"
import { SymbolKind, SymbolScopeKind } from "./symbols.ts"
import { Type } from "./types.ts"

export class LocalPersistTransformer extends BoundTreeRewriter
{
    localpersistNodes: BoundVariableDeclaration[] = []

    constructor()
    {
        super()
    }

    public RewriteCompilationUnit(node: BoundCompilationUnit): BoundCompilationUnit
    {
        let newDeclarations: BoundStatement[] = []
        for (let declaration of node.globalDeclarations) {
            this.localpersistNodes = []
            let newDeclaration = this.RewriteModuleStatement(declaration)
            if (this.localpersistNodes.length != 0)
                newDeclarations = newDeclarations.concat(this.localpersistNodes)
            newDeclarations.push(newDeclaration)
        }

        return new BoundCompilationUnit(node.symbolTable, newDeclarations)
    }

    protected RewriteVariableDeclaration(node: BoundVariableDeclaration): BoundStatement
    {
        if (node.symbol.scopeKind != SymbolScopeKind.LocalPersist)
            return node

        if (node.initializer == null)
            throw new Error("Local persist must have initializer")

        let newSymbolName = `${node.symbol.name}__LocalPersist`
        let initFlagName = `${node.symbol.name}__IsInitialized`

        node.symbol.name = newSymbolName
        this.localpersistNodes.push(node)
        let trueLiteral = new BoundPrimitiveLiteral(BoundNodeKind.BoolLiteral, null, node.symbolTable, Type.Bool, "true")
        let falseLiteral = new BoundPrimitiveLiteral(BoundNodeKind.BoolLiteral, null, node.symbolTable, Type.Bool, "false")
        let initFlagSymbol = node.symbolTable.AddSymbol(initFlagName, SymbolKind.Variable, SymbolScopeKind.Global, Type.Bool)!
        let initFlagDecl = new BoundVariableDeclaration(null, node.symbolTable, initFlagSymbol, falseLiteral)
        this.localpersistNodes.push(initFlagDecl)

        let varNameExpr = new BoundNameExpression(null, node.symbolTable, node.symbol.type, node.symbol)
        let initFlagNameExpr = new BoundNameExpression(null, node.symbolTable, initFlagSymbol.type, initFlagSymbol)
        let condition = new BoundBinaryExpression(null, node.symbolTable, BoundBinaryOperatorKind.Equals, initFlagNameExpr, falseLiteral, initFlagNameExpr.type, true)
        let init = new BoundBinaryExpression(null, node.symbolTable, BoundBinaryOperatorKind.Assignment, varNameExpr, node.initializer, varNameExpr.type, true)
        let initStatement = new BoundExpressionStatement(null, node.symbolTable, init)
        let flag = new BoundBinaryExpression(null, node.symbolTable, BoundBinaryOperatorKind.Assignment, initFlagNameExpr, trueLiteral, initFlagNameExpr.type, true)
        let flagStatement = new BoundExpressionStatement(null, node.symbolTable, flag)
        let thenBlock = new BoundBlockStatement(null, node.symbolTable, [initStatement, flagStatement])
        let ifStatement = new BoundIfStatement(null, node.symbolTable, condition, thenBlock, null)

        node.initializer = null

        return ifStatement
    }
}