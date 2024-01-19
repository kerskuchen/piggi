// deno-lint-ignore-file prefer-const

import { BoundBlockStatement, BoundExpression, BoundFunctionCallExpression } from "./boundtree.ts"
import { BoundTreeTraverser } from "./boundtree_rewriter.ts"
import { Symbol, SymbolKind } from "./symbols.ts"

////////////////////////////////////////////////////////////////////////////////////////////////////
// LocalPersistTransformer 

/*
export class LocalPersistTransformer extends BoundTreeTraverser
{
    localpersistNodes: BoundVariableDeclaration[] = []

    constructor()
    {
        super()
    }

    public TraverseCompilationUnit(node: BoundCompilationUnit): BoundCompilationUnit
    {
        let newDeclarations: BoundStatement[] = []
        for (let declaration of node.globalDeclarations) {
            this.localpersistNodes = []
            let newDeclaration = this.TraverseModuleStatement(declaration)
            if (this.localpersistNodes.length != 0)
                newDeclarations = newDeclarations.concat(this.localpersistNodes)
            newDeclarations.push(newDeclaration)
        }

        return new BoundCompilationUnit(node.symbolTable, newDeclarations, node.sortedGlobalVariables)
    }

    protected TraverseVariableDeclaration(node: BoundVariableDeclaration): BoundStatement
    {
        if (node.symbol.kind != SymbolKind.LocalPersistVariable)
            return node

        if (node.initializer == null)
            throw new Error("Local persist must have initializer")

        let newSymbolName = `${node.symbol.name}__LocalPersist`
        let initFlagName = `${node.symbol.name}__IsInitialized`

        node.symbol.name = newSymbolName
        this.localpersistNodes.push(node)
        let trueLiteral = new BoundPrimitiveLiteral(BoundNodeKind.BoolLiteral, null, node.symbolTable, Type.Bool, "true")
        let falseLiteral = new BoundPrimitiveLiteral(BoundNodeKind.BoolLiteral, null, node.symbolTable, Type.Bool, "false")
        let initFlagSymbol = node.symbolTable.AddSymbol(initFlagName, SymbolKind.GlobalVariable, false, Type.Bool, null)!
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
*/

////////////////////////////////////////////////////////////////////////////////////////////////////
// Symbol Collector

export class GlobalVariableReferenceCollector extends BoundTreeTraverser
{
    collectedGlobalVariableReferences = new Set<Symbol>()
    alreadyTraversedFunctions = new Set<Symbol>()

    constructor(public functionBodies: Map<Symbol, BoundBlockStatement>)
    {
        super()
    }

    public Collect(node: BoundExpression): Set<Symbol>
    {
        this.TraverseExpression(node)
        return this.collectedGlobalVariableReferences
    }

    protected TraverseExpression(node: BoundExpression): BoundExpression
    {
        if (node.symbol != null) {
            if (node.symbol.kind == SymbolKind.GlobalVariable || node.symbol.kind == SymbolKind.MemberVariable)
                this.collectedGlobalVariableReferences.add(node.symbol)
        }
        return super.TraverseExpression(node)
    }

    protected TraverseFunctionCallExpression(node: BoundFunctionCallExpression): BoundExpression
    {
        if (node.symbol == null)
            throw new Error("Symbol of function call expression is null")

        if (!this.alreadyTraversedFunctions.has(node.symbol)) {
            this.alreadyTraversedFunctions.add(node.symbol)
            // NOTE: Body can be undefined i.e. for exterals or constructors
            let body = this.functionBodies.get(node.symbol)
            if (body != undefined) {
                this.TraverseStatement(body)
            }
        }
        return super.TraverseFunctionCallExpression(node)
    }
}