// deno-lint-ignore-file prefer-const

import { BoundArrayIndexExpression, BoundArrayLiteral, BoundBinaryExpression, BoundBlockStatement, BoundBreakStatement, BoundCaseStatement, BoundCompilationUnit, BoundContinueStatement, BoundDoWhileStatement, BoundEnumValueLiteral, BoundExpression, BoundExpressionStatement, BoundForStatement, BoundFunctionCallExpression, BoundFunctionDeclaration, BoundIfStatement, BoundImportDeclaration, BoundMemberAccessExpression, BoundMissingExpression, BoundMissingStatement, BoundNameExpression, BoundNodeKind, BoundParenthesizedExpression, BoundPrimitiveLiteral, BoundReturnStatement, BoundStatement, BoundStructDeclaration, BoundSwitchStatement, BoundTernaryConditionalExpression, BoundTypeCastExpression, BoundUnaryExpression, BoundVariableDeclaration, BoundWhileStatement } from "./boundtree.ts"
import { DiagnosticBag, SourceLocation } from "./common.ts"
import { ArrayIndexExpressionSyntax, ArrayLiteralSyntax, ArrayTypeExpressionSyntax, BaseTypeExpressionSyntax, BinaryExpressionSyntax, BlockStatementSyntax, BoolLiteralSyntax, BreakStatementSyntax, CaseStatementSyntax, ContinueStatementSyntax, DoWhileStatementSyntax, EnumDeclarationSyntax, EnumDefinitionStatementSyntax, ExpressionStatementSyntax, ExpressionSyntax, ForStatementSyntax, FuncCallExpressionSyntax, FunctionDeclarationSyntax, FunctionDefinitionStatementSyntax, GlobalVariableDeclarationSyntax, IfStatementSyntax, ImportDeclarationSyntax, MemberAccessExpressionSyntax, ModuleMemberSyntax, NameExpressionSyntax, NullLiteralSyntax, NullableTypeExpressionSyntax, NumberLiteralSyntax, ParenthesizedExpressionSyntax, ReturnStatementSyntax, StatementSyntax, StringLiteralSyntax, StructDeclarationSyntax, StructDefinitionStatementSyntax, SwitchStatementSyntax, SyntaxTree, TernaryConditionalExpressionSyntax, TypeCastExpressionSyntax, TypeExpressionSyntax, UnaryExpressionSyntax, VariableDeclarationSyntax, WhileStatementSyntax } from "./syntax.ts"
import { SyntaxKind } from "./syntax.ts"
import { ArrayType, BaseType, BaseTypeKind, NullableType, Type } from "./types.ts"
import { Symbol, SymbolTable, SymbolScopeKind, SymbolKind } from "./symbols.ts"
import { BoundEnumDeclaration } from "./boundtree.ts"
import { BoundBinaryOperator, BoundUnaryOperator } from "./operators.ts"

export class Binder
{
    diagnostics = new DiagnosticBag()
    loopLevel = 0
    switchCaseLevel = 0
    currentFunctionSymbol: Symbol | null = null

    constructor(
        public symbolTable: SymbolTable
    )
    {
        Type.Init()
    }

    BindCompilationUnit(trees: SyntaxTree[]): BoundCompilationUnit
    {
        for (let tree of trees) {
            this.diagnostics.Append(tree.diagnostics)
        }

        let globalDeclarations = []
        for (let tree of trees) {
            let module = tree.root
            for (let moduleStatement of module.members) {
                let boundStatement = this.BindModuleStatement(moduleStatement)
                globalDeclarations.push(boundStatement)
            }
        }
        return new BoundCompilationUnit(this.symbolTable, globalDeclarations)
    }

    private BindModuleStatement(syntax: ModuleMemberSyntax): BoundStatement
    {
        switch (syntax.kind) {
            case SyntaxKind.ImportDeclaration:
                return this.BindImportDeclaration(syntax as ImportDeclarationSyntax)
            case SyntaxKind.GlobalVariableDeclaration:
                return this.BindGlobalVariableDefinitionStatement(syntax as GlobalVariableDeclarationSyntax)
            case SyntaxKind.EnumDeclaration:
                return this.BindEnumDefinitionStatement(syntax as EnumDeclarationSyntax)
            case SyntaxKind.EnumDefinitionStatement:
                return this.BindEnumDefinitionStatement(syntax as EnumDefinitionStatementSyntax)
            case SyntaxKind.StructDeclaration:
                return this.BindStructDefinitionStatement(syntax as StructDeclarationSyntax)
            case SyntaxKind.StructDefinitionStatement:
                return this.BindStructDefinitionStatement(syntax as StructDefinitionStatementSyntax)
            case SyntaxKind.FunctionDeclaration:
                return this.BindFunctionDefinitionStatement(syntax as FunctionDeclarationSyntax)
            case SyntaxKind.FunctionDefinitionStatement:
                return this.BindFunctionDefinitionStatement(syntax as FunctionDefinitionStatementSyntax)
            default:
                throw new Error(`Unexpected module statement ${syntax.kind} in binder`)
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    // Module

    private BindImportDeclaration(syntax: ImportDeclarationSyntax): BoundImportDeclaration
    {
        if (this.currentFunctionSymbol != null)
            throw new Error("Unexpected global variable declaration in function")

        let modulename = syntax.modulenameIdent.GetText()
        return new BoundImportDeclaration(syntax, this.symbolTable, modulename)
    }

    private BindGlobalVariableDefinitionStatement(syntax: GlobalVariableDeclarationSyntax): BoundVariableDeclaration | BoundMissingStatement
    {
        if (this.currentFunctionSymbol != null)
            throw new Error("Unexpected global variable declaration in function")

        let isExternal = syntax.externKeyword != null
        let symbolScopeKind = isExternal ? SymbolScopeKind.Extern : SymbolScopeKind.Global
        return this.BindVariableDefinitionStatement(syntax.declaration, symbolScopeKind)
    }

    private BindStructDefinitionStatement(syntax: StructDeclarationSyntax | StructDefinitionStatementSyntax): BoundStructDeclaration | BoundMissingStatement
    {
        if (this.currentFunctionSymbol != null)
            throw new Error("Unexpected struct declaration in function")

        let declarationPart
        if (syntax instanceof StructDeclarationSyntax) {
            declarationPart = syntax as StructDeclarationSyntax
        } else {
            declarationPart = (syntax as StructDefinitionStatementSyntax).structDeclaration
        }

        let isExternal = declarationPart.externKeyword != null
        let symbolScopeKind = isExternal ? SymbolScopeKind.Extern : SymbolScopeKind.Global

        let identifier = declarationPart.identifier
        let typeName = identifier.GetText()
        let type = new BaseType(BaseTypeKind.Struct, typeName)
        let structSymbol = this.GetOrCreateSymbolWithSpecificType(identifier.GetLocation(), typeName, SymbolKind.Struct, symbolScopeKind, type)
        if (structSymbol == null) {
            return new BoundMissingStatement(syntax, this.symbolTable)
        }

        let isForwardDeclaration = true
        if (syntax instanceof StructDefinitionStatementSyntax) {
            isForwardDeclaration = false

            // Push struct member symboltable scope to parse variable declarations as members
            structSymbol.membersSymbolTable = this.PushNewSymbolTable()
            for (let index = 0; index < syntax.membersAndSeparators.length; index += 2) {
                let memberDeclaration = syntax.membersAndSeparators[index] as VariableDeclarationSyntax
                let memberNode = this.BindVariableDefinitionStatement(memberDeclaration, SymbolScopeKind.Local)
                if (memberNode instanceof BoundMissingStatement)
                    continue
                memberNode.symbol.kind = SymbolKind.Member
            }
            this.PopSymbolTable()

            if (structSymbol.membersSymbolTable.symbols.size == 0) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Struct '${typeName}' needs at least one member`,
                )
            }
            if (structSymbol.alreadyDefined) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Duplicate struct definition of '${typeName}'`,
                )
            }
            structSymbol.alreadyDefined = true
        }

        return new BoundStructDeclaration(syntax, this.symbolTable, structSymbol, isForwardDeclaration)
    }

    private BindEnumDefinitionStatement(syntax: EnumDeclarationSyntax | EnumDefinitionStatementSyntax): BoundEnumDeclaration | BoundMissingStatement
    {
        if (this.currentFunctionSymbol != null)
            throw new Error("Unexpected enum declaration in function")

        let declarationPart
        if (syntax instanceof EnumDeclarationSyntax) {
            declarationPart = syntax as EnumDeclarationSyntax
        } else {
            declarationPart = (syntax as EnumDefinitionStatementSyntax).enumDeclaration
        }

        let isExternal = declarationPart.externKeyword != null
        let symbolScopeKind = isExternal ? SymbolScopeKind.Extern : SymbolScopeKind.Global

        let identifier = declarationPart.identifier
        let enumName = identifier.GetText()
        let type = new BaseType(BaseTypeKind.Enum, enumName)
        let enumSymbol = this.GetOrCreateSymbolWithSpecificType(identifier.GetLocation(), enumName, SymbolKind.Enum, symbolScopeKind, type)
        if (enumSymbol == null) {
            return new BoundMissingStatement(syntax, this.symbolTable)
        }

        let isForwardDeclaration = true
        if (syntax instanceof EnumDefinitionStatementSyntax) {

            // Push enum member symboltable scope to parse declarations as members
            enumSymbol.membersSymbolTable = this.PushNewSymbolTable()
            let valueCounter = 0
            for (let clause of syntax.values) {
                let valueName = clause.valueIdentifier.GetText()
                let valueSymbol = this.TryCreateSymbolOrError(identifier.GetLocation(), valueName, SymbolKind.Enumvalue, SymbolScopeKind.Local, enumSymbol.type)
                if (valueSymbol == null) {
                    continue
                }

                if (clause.integerLiteral != null) {
                    let valueToken = clause.integerLiteral
                    if (valueToken.numValue == null)
                        throw new Error("Integer literal cannot be empty in enum value")
                    if (valueToken.numValueIsFloat) {
                        this.diagnostics.ReportError(
                            valueToken.GetLocation(),
                            `Assigned value of enum value literal '${valueName}' must be an integer literal`,
                        )
                    }
                    if (valueToken.numValue < valueCounter) {
                        this.diagnostics.ReportError(
                            valueToken.GetLocation(),
                            `Assigned value of enum value literal '${valueName}' must be chosen such that all enum values of '${enumName}' are unique - chosen value '${valueToken.numValue}' would lead to duplicates`,
                        )
                    }
                    valueCounter = valueToken.numValue
                }
                valueSymbol.kind = SymbolKind.Enumvalue
                valueSymbol.enumValue = valueCounter
                valueCounter += 1
            }
            this.PopSymbolTable()

            if (enumSymbol.membersSymbolTable.symbols.size == 0) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Enum '${enumName}' needs at least one member`,
                )
            }

            if (enumSymbol.alreadyDefined) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Duplicate enum definition of '${enumName}'`,
                )
            }
            enumSymbol.alreadyDefined = true
        }

        return new BoundEnumDeclaration(syntax, this.symbolTable, enumSymbol, isForwardDeclaration)
    }

    private BindFunctionDefinitionStatement(syntax: FunctionDeclarationSyntax | FunctionDefinitionStatementSyntax): BoundFunctionDeclaration | BoundMissingStatement
    {
        if (this.currentFunctionSymbol != null)
            throw new Error("Unexpected function declaration in function")

        let declarationPart
        if (syntax instanceof FunctionDeclarationSyntax) {
            declarationPart = syntax as FunctionDeclarationSyntax
        } else {
            declarationPart = (syntax as FunctionDefinitionStatementSyntax).funcDecl
        }

        let isExternal = declarationPart.externKeyword != null
        let symbolScopeKind = isExternal ? SymbolScopeKind.Extern : SymbolScopeKind.Global

        let identifier = declarationPart.identifier
        let functionName = identifier.GetText()
        let returnType = declarationPart.returnType == null
            ? Type.Void
            : this.BindType(declarationPart.returnType)
        let functionSymbol = this.GetOrCreateSymbolWithSpecificType(identifier.GetLocation(), functionName, SymbolKind.Function, symbolScopeKind, returnType)
        if (functionSymbol == null) {
            return new BoundMissingStatement(syntax, this.symbolTable)
        }

        // Push function symboltable scope to parse variables as function parameters
        let functionParamsSymbolTable = this.PushNewSymbolTable()
        for (let index = 0; index < declarationPart.paramsAndSeparators.length; index += 2) {
            let param = declarationPart.paramsAndSeparators[index]
            if (!(param instanceof VariableDeclarationSyntax))
                throw new Error(`Function parameter syntax has wrong type ${param.kind}`)
            let boundParam = this.BindVariableDefinitionStatement(param, SymbolScopeKind.Local)
            if (boundParam instanceof BoundMissingStatement)
                continue
            boundParam.symbol.kind = SymbolKind.Parameter
        }
        this.PopSymbolTable()

        if (functionSymbol.membersSymbolTable != null) {
            // NOTE: The function was already declared before, we need to make sure its types and 
            // parameters match up with the previous defined type and parameters

            let paramsExisting = Array.from(functionSymbol.membersSymbolTable.symbols.values())
            let paramsNew = Array.from(functionParamsSymbolTable.symbols.values())

            if (paramsExisting.length == paramsNew.length) {
                for (let index = 0; index < paramsExisting.length; index += 1) {
                    let existingType = paramsExisting[index].type
                    let newType = paramsNew[index].type
                    if (!Type.Identical(existingType, newType)) {
                        this.diagnostics.ReportError(
                            declarationPart.GetLocation(),
                            `Previous function '${functionSymbol.name}' parameter ${index + 1} declared type differs from current declared type`,
                        )
                        return new BoundMissingStatement(syntax, this.symbolTable)
                    }
                }
            } else {
                this.diagnostics.ReportError(
                    declarationPart.GetLocation(),
                    `Function '${functionSymbol.name}' was previously declared with ${paramsExisting.length} parameters wheras the current declaration has ${paramsNew.length} parameters`,
                )
                return new BoundMissingStatement(syntax, this.symbolTable)
            }
        }
        functionSymbol.membersSymbolTable = functionParamsSymbolTable

        let body = null
        if (syntax instanceof FunctionDefinitionStatementSyntax) {
            if (isExternal) {
                this.diagnostics.ReportError(
                    declarationPart.externKeyword!.GetLocation(),
                    `Cannot define external function '${functionName}'`,
                )
            }
            if (functionSymbol.alreadyDefined) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Duplicate function definition of '${functionName}'`,
                )
            }
            functionSymbol.alreadyDefined = true

            // Bind function body
            this.PushCustomSymbolTable(functionSymbol.membersSymbolTable)
            this.currentFunctionSymbol = functionSymbol
            body = this.BindBlockStatement(syntax.body)
            // TODO: make sure function returns something if its return type is not void
            this.currentFunctionSymbol = null
            this.PopSymbolTable()
        }

        return new BoundFunctionDeclaration(syntax, this.symbolTable, functionSymbol, body)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    // Statements

    private BindStatement(syntax: StatementSyntax): BoundStatement
    {
        switch (syntax.kind) {
            case SyntaxKind.BlockStatement:
                return this.BindBlockStatement(syntax as BlockStatementSyntax)
            case SyntaxKind.IfStatement:
                return this.BindIfStatement(syntax as IfStatementSyntax)
            case SyntaxKind.DoWhileStatement:
                return this.BindDoWhileStatement(syntax as DoWhileStatementSyntax)
            case SyntaxKind.WhileStatement:
                return this.BindWhileStatement(syntax as WhileStatementSyntax)
            case SyntaxKind.ForStatement:
                return this.BindForStatement(syntax as ForStatementSyntax)
            case SyntaxKind.ReturnStatement:
                return this.BindReturnStatement(syntax as ReturnStatementSyntax)
            case SyntaxKind.BreakStatement:
                return this.BindBreakStatement(syntax as BreakStatementSyntax)
            case SyntaxKind.ContinueStatement:
                return this.BindContinueStatement(syntax as ContinueStatementSyntax)
            case SyntaxKind.SwitchStatement:
                return this.BindSwitchStatement(syntax as SwitchStatementSyntax)
            case SyntaxKind.VariableDeclaration:
                return this.BindVariableDefinitionStatement(syntax as VariableDeclarationSyntax, SymbolScopeKind.Local)
            case SyntaxKind.ExpressionStatement:
                return this.BindExpressionStatement(syntax as ExpressionStatementSyntax)
            default:
                throw new Error(`Unexpected statement in binder: '${syntax.kind}'`)
        }
    }

    private BindExpressionStatement(syntax: ExpressionStatementSyntax): BoundExpressionStatement
    {
        let expression = this.BindExpression(syntax.expression)
        return new BoundExpressionStatement(syntax, this.symbolTable, expression)
    }

    private BindBlockStatement(syntax: BlockStatementSyntax): BoundBlockStatement
    {
        this.PushNewSymbolTable()
        let boundStatements = []
        for (let statement of syntax.statements) {
            let boundStatement = this.BindStatement(statement)
            boundStatements.push(boundStatement)
        }
        this.PopSymbolTable()

        return this.FlattenBlockStatementIfNecessary(new BoundBlockStatement(syntax, this.symbolTable, boundStatements)) as BoundBlockStatement
    }

    private BindVariableDefinitionStatement(syntax: VariableDeclarationSyntax, symbolScopeKind: SymbolScopeKind): BoundVariableDeclaration | BoundMissingStatement
    {
        let identifier = syntax.identifier
        let varName = syntax.identifier.GetText()
        let isLocalPersist = syntax.letKeyword != null && syntax.letKeyword.kind == SyntaxKind.LetLocalPersistKeyword
        if (isLocalPersist) {
            if (symbolScopeKind != SymbolScopeKind.Local) {
                this.diagnostics.ReportError(
                    identifier.GetLocation(),
                    `Cannot mark non-local variable '${varName}' as local persistent`,
                )
            }
            symbolScopeKind = SymbolScopeKind.LocalPersist
        }

        let type = this.BindType(syntax.type)
        let varSymbol = this.TryCreateSymbolOrError(identifier.GetLocation(), varName, SymbolKind.Variable, symbolScopeKind, type)
        if (varSymbol == null) {
            this.diagnostics.ReportError(
                identifier.GetLocation(),
                `Symbol '${varName}' is already declared in current scope`,
            )
            return new BoundMissingStatement(syntax, this.symbolTable)
        }

        let initializer = null
        if (syntax.initializer != null) {
            initializer = this.BindExpression(syntax.initializer)
            this.CanConvertTypeImplicitlyOrError(syntax.initializer.GetLocation(), initializer.type, varSymbol.type)
        }
        return new BoundVariableDeclaration(syntax, this.symbolTable, varSymbol, initializer)
    }

    private BindIfStatement(syntax: IfStatementSyntax): BoundIfStatement
    {
        if (this.currentFunctionSymbol == null)
            throw new Error("Unexpected if statement outside of function")

        let condition = this.BindExpression(syntax.condition)
        this.CanConvertTypeImplicitlyOrError(syntax.condition.GetLocation(), condition.type, Type.Bool)

        let thenStatement = this.BindStatement(syntax.thenStatement)
        thenStatement = this.WrapInBlockStatementIfNecessary(thenStatement)

        let elseStatement = null
        if (syntax.elseStatement != null) {
            elseStatement = this.BindStatement(syntax.elseStatement)
            elseStatement = this.WrapInBlockStatementIfNecessary(elseStatement)
        }

        return new BoundIfStatement(syntax, this.symbolTable, condition, thenStatement, elseStatement)
    }

    private BindWhileStatement(syntax: WhileStatementSyntax): BoundWhileStatement
    {
        if (this.currentFunctionSymbol == null)
            throw new Error("Unexpected while statement outside of function")

        let condition = this.BindExpression(syntax.condition)
        this.CanConvertTypeImplicitlyOrError(syntax.condition.GetLocation(), condition.type, Type.Bool)

        this.loopLevel += 1
        let body = this.BindStatement(syntax.body)
        body = this.WrapInBlockStatementIfNecessary(body)
        this.loopLevel -= 1

        return new BoundWhileStatement(syntax, this.symbolTable, condition, body)
    }

    private BindDoWhileStatement(syntax: DoWhileStatementSyntax): BoundDoWhileStatement
    {
        if (this.currentFunctionSymbol == null)
            throw new Error("Unexpected do while statement outside of function")

        this.loopLevel += 1
        let body = this.BindStatement(syntax.body)
        body = this.WrapInBlockStatementIfNecessary(body)
        this.loopLevel -= 1

        let condition = this.BindExpression(syntax.condition)
        this.CanConvertTypeImplicitlyOrError(syntax.condition.GetLocation(), condition.type, Type.Bool)

        return new BoundDoWhileStatement(syntax, this.symbolTable, condition, body)
    }

    private BindForStatement(syntax: ForStatementSyntax): BoundForStatement | BoundMissingStatement
    {
        if (this.currentFunctionSymbol == null)
            throw new Error("Unexpected for-statement outside of function")

        let lowerBound = this.BindExpression(syntax.lowerBound)
        if (lowerBound.type != Type.Number) {
            this.diagnostics.ReportError(
                syntax.lowerBound.GetLocation(),
                `Expected for statements lower bound to be of type 'number' - actual type ${lowerBound.type.PrettyPrint()}`,
            )
        }
        let upperBound = this.BindExpression(syntax.upperBound)
        if (upperBound.type != Type.Number) {
            this.diagnostics.ReportError(
                syntax.upperBound.GetLocation(),
                `Expected for statements upper bound to be of type 'number' - actual type ${upperBound.type.PrettyPrint()}`,
            )
        }

        this.PushNewSymbolTable()

        let iteratorName = syntax.iteratorIdent.GetText()
        let iteratorSymbol = this.TryCreateSymbolOrError(syntax.iteratorIdent.GetLocation(), iteratorName, SymbolKind.Variable, SymbolScopeKind.Local, Type.Number)
        if (iteratorSymbol == null) {
            // NOTE: Although we created a new scope for our for-statement the varible declaration failed.
            // This means we tried to shadow a function parameter. We already output an error at this point
            // To not break everything too severely we add a dummy symbol and allow it to shadow our parameter
            iteratorSymbol = this.symbolTable.AddSymbol(iteratorName, SymbolKind.Variable, SymbolScopeKind.Local, Type.Number)!
        }

        this.loopLevel += 1
        let body = this.BindStatement(syntax.body)
        body = this.WrapInBlockStatementIfNecessary(body)
        this.loopLevel -= 1

        this.PopSymbolTable()

        let upperBoundIsInclusive = syntax.equals != null
        return new BoundForStatement(syntax, this.symbolTable, iteratorSymbol, lowerBound, upperBound, upperBoundIsInclusive, body)
    }

    private BindReturnStatement(syntax: ReturnStatementSyntax): BoundReturnStatement
    {
        if (this.currentFunctionSymbol == null)
            throw new Error("Unexpected for statement outside of function")

        let functionReturnType = this.currentFunctionSymbol.type
        if (functionReturnType.IsVoid() && syntax.returnExpression != null) {
            this.diagnostics.ReportError(
                syntax.returnExpression.GetLocation(),
                `Invalid return expression in function that does not return anything`
            )
        }
        if (!functionReturnType.IsVoid() && syntax.returnExpression == null) {
            this.diagnostics.ReportError(
                syntax.returnKeyword.GetLocation(),
                `Must expected return expression of type ${functionReturnType.PrettyPrint()} after 'return' keyword in function`
            )
        }

        let returnExpression = null
        if (syntax.returnExpression != null) {
            returnExpression = this.BindExpression(syntax.returnExpression)
            this.CanConvertTypeImplicitlyOrError(syntax.returnExpression.GetLocation(), returnExpression.type, functionReturnType)
        }

        return new BoundReturnStatement(syntax, this.symbolTable, returnExpression)
    }

    private BindBreakStatement(syntax: BreakStatementSyntax): BoundBreakStatement
    {
        if (this.currentFunctionSymbol == null)
            throw new Error("Unexpected break statement outside of function")

        if (this.loopLevel == 0 && this.switchCaseLevel == 0) {
            this.diagnostics.ReportError(
                syntax.breakKeyword.GetLocation(),
                `Invalid 'break' statement found outside of loop or switch-case definition`
            )
        }
        return new BoundBreakStatement(syntax, this.symbolTable)
    }

    private BindContinueStatement(syntax: ContinueStatementSyntax): BoundContinueStatement
    {
        if (this.currentFunctionSymbol == null)
            throw new Error("Unexpected continue statement outside of function")

        if (this.loopLevel == 0) {
            this.diagnostics.ReportError(
                syntax.continueKeyword.GetLocation(),
                `Invalid 'continue' statement found outside of loop definition`
            )
        }
        return new BoundContinueStatement(syntax, this.symbolTable)
    }

    private BindSwitchStatement(syntax: SwitchStatementSyntax): BoundSwitchStatement
    {
        if (this.currentFunctionSymbol == null)
            throw new Error("Unexpected switch statement outside of function")

        let switchExpression = this.BindExpression(syntax.switchExpression)

        this.switchCaseLevel += 1
        let caseStatements = []
        let defaultStatementEncountered = false
        for (let index = 0; index < syntax.caseStatements.length; index += 1) {
            let caseStatement = syntax.caseStatements[index]
            let boundCaseStatement = this.BindCaseStatement(switchExpression, caseStatement)
            if (defaultStatementEncountered) {
                this.diagnostics.ReportError(
                    caseStatement.caseOrDefaultKeyword.GetLocation(),
                    `Unexpected case statement after default statement was already defined`
                )
            }
            if (caseStatement.kind == SyntaxKind.DefaultStatement)
                defaultStatementEncountered = true
            caseStatements.push(boundCaseStatement)
        }
        this.switchCaseLevel -= 1

        if (caseStatements.length == 0) {
            this.diagnostics.ReportError(
                syntax.switchKeyword.GetLocation(),
                `Empty switch statements are not allowed`
            )
        }

        // Check that we don't have any duplicate case labels
        for (let index = 0; index < caseStatements.length; index += 1) {
            let a = caseStatements[index]
            for (let inner = index + 1; inner < caseStatements.length; inner += 1) {
                let b = caseStatements[inner]
                if (a.caseExpression != null && b.caseExpression != null
                    && this.AreLiteralsEqual(a.caseExpression, b.caseExpression)) {
                    this.diagnostics.ReportError(
                        b.caseExpression.syntax!.GetLocation(),
                        `Duplicate switch case literal '${b.caseExpression.syntax!.GetLocation().GetText()}'`,
                    )
                }
            }
        }

        return new BoundSwitchStatement(syntax, this.symbolTable, switchExpression, caseStatements)
    }

    private BindCaseStatement(switchExpression: BoundExpression, syntax: CaseStatementSyntax): BoundCaseStatement
    {
        if (this.switchCaseLevel == 0)
            throw new Error("Unexpected case statement outside of switch statement block")

        let caseExpression = null
        if (syntax.caseExpression != null) {
            caseExpression = this.BindExpression(syntax.caseExpression)
            if (caseExpression.kind != BoundNodeKind.NumberLiteral
                && caseExpression.kind != BoundNodeKind.StringLiteral
                && caseExpression.kind != BoundNodeKind.EnumValueLiteral) {
                this.diagnostics.ReportError(
                    syntax.caseExpression.GetLocation(),
                    `Expected primitive literal after 'case' label but got '${syntax.caseExpression.kind}'`,
                )
            }
            this.CanConvertTypeImplicitlyOrError(syntax.caseExpression.GetLocation(), caseExpression.type, switchExpression.type)
        }

        let body = null
        if (syntax.body != null && syntax.body.statements.length > 0)
            body = this.BindBlockStatement(syntax.body)

        return new BoundCaseStatement(syntax, this.symbolTable, caseExpression, body)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    // Expressions

    private BindExpression(syntax: ExpressionSyntax): BoundExpression
    {
        switch (syntax.kind) {
            // Expressions
            case SyntaxKind.UnaryExpression:
                return this.BindUnaryExpression(syntax as UnaryExpressionSyntax)
            case SyntaxKind.BinaryExpression:
                return this.BindBinaryExpression(syntax as BinaryExpressionSyntax)
            case SyntaxKind.FuncCallExpression:
                return this.BindFunctionCallExpression(syntax as FuncCallExpressionSyntax)
            case SyntaxKind.ArrayIndexExpression:
                return this.BindArrayIndexingExpression(syntax as ArrayIndexExpressionSyntax)
            case SyntaxKind.MemberAccessExpression:
                return this.BindMemberAccessExpression(syntax as MemberAccessExpressionSyntax)
            case SyntaxKind.TypeCastExpression:
                return this.BindTypeCastExpression(syntax as TypeCastExpressionSyntax)
            case SyntaxKind.ParenthesizedExpression:
                return this.BindParenthesizedExpression(syntax as ParenthesizedExpressionSyntax)
            case SyntaxKind.TernaryConditionalExpression:
                return this.BindTernaryConditionalExpression(syntax as TernaryConditionalExpressionSyntax)
            case SyntaxKind.NameExpression:
                return this.BindNameExpression(syntax as NameExpressionSyntax)

            // Literals
            case SyntaxKind.NullLiteral:
                return this.BindNullLiteral(syntax as NullLiteralSyntax)
            case SyntaxKind.BoolLiteral:
                return this.BindBoolLiteral(syntax as BoolLiteralSyntax)
            case SyntaxKind.NumberLiteral:
                return this.BindNumberLiteral(syntax as NumberLiteralSyntax)
            case SyntaxKind.StringLiteral:
                return this.BindStringLiteral(syntax as StringLiteralSyntax)
            case SyntaxKind.ArrayLiteral:
                return this.BindArrayLiteral(syntax as ArrayLiteralSyntax)

            default:
                throw new Error(`Unexpected expression in binder '${syntax.kind}'`)
        }
    }

    private BindParenthesizedExpression(syntax: ParenthesizedExpressionSyntax): BoundParenthesizedExpression
    {
        let inner = this.BindExpression(syntax.inner)
        return new BoundParenthesizedExpression(syntax, this.symbolTable, inner)
    }

    private BindTernaryConditionalExpression(syntax: TernaryConditionalExpressionSyntax): BoundExpression
    {
        let condition = this.BindExpression(syntax.condition)
        let thenExpression = this.BindExpression(syntax.thenExpression)
        let elseExpression = this.BindExpression(syntax.elseExpression)
        if (!Type.Identical(thenExpression.type, elseExpression.type)) {
            this.diagnostics.ReportError(
                syntax.questionmark.GetLocation(),
                `Incompatible expression types in ternary operator - then branch: '${thenExpression.type.PrettyPrint()}', else branch: '${elseExpression.type.PrettyPrint()}'`,
            )
            return new BoundMissingExpression(syntax, this.symbolTable)
        }

        return new BoundTernaryConditionalExpression(syntax, this.symbolTable, condition, thenExpression, elseExpression)
    }

    private BindBinaryExpression(syntax: BinaryExpressionSyntax): BoundExpression
    {
        let left = this.BindExpression(syntax.left)
        let right = this.BindExpression(syntax.right)
        let operatorToken = syntax.operator

        let operator = BoundBinaryOperator.FromTokenAndOperandTypes(this.diagnostics, operatorToken, left.type, right.type)
        if (operator == null)
            return new BoundMissingExpression(syntax, this.symbolTable)

        if (operator.leftMustBeLValue && left.isRValue) {
            this.diagnostics.ReportError(
                operatorToken.GetLocation(),
                `Left argument of operator '${operatorToken.GetText()}' must be an storage location`,
            )
            return new BoundMissingExpression(syntax, this.symbolTable)
        }
        if (operator.rightMustBeLValue && right.isRValue) {
            this.diagnostics.ReportError(
                operatorToken.GetLocation(),
                `Right argument of operator '${operatorToken.GetText()}' must be a storage location`,
            )
            return new BoundMissingExpression(syntax, this.symbolTable)
        }

        return new BoundBinaryExpression(syntax, this.symbolTable, operator, left, right)
    }

    private BindUnaryExpression(syntax: UnaryExpressionSyntax): BoundExpression
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
            return new BoundMissingExpression(syntax, this.symbolTable)
        }
        return new BoundUnaryExpression(syntax, this.symbolTable, operator, operand)
    }


    private BindMemberAccessExpression(syntax: MemberAccessExpressionSyntax): BoundExpression
    {
        let container = this.BindExpression(syntax.container)
        let accessorToken = syntax.dot

        let containerType
        if (container.type.IsNullable())
            containerType = container.type.GetInnerType()
        else
            containerType = container.type

        if (!containerType.IsStruct() && !containerType.IsEnum()) {
            this.diagnostics.ReportError(
                accessorToken.GetLocation(),
                `Attempt to access member of non-container type expression '${container.syntax!.GetLocation().GetText()}'`,
            )
            return new BoundMissingExpression(syntax, this.symbolTable)
        }

        // TODO: this is most probably be wrong but let it guide us with its errors later.
        // Basically we have two distinct concepts:
        // 1) we access a VARIABLE of a certain TYPE like `myvar.member`
        // 2) we access a TYPE directly like `MyEnum.FirstValue`
        let containerName = (containerType as BaseType).name
        let containerSymbol = this.symbolTable.GetSymbol(containerName)
        if (containerSymbol == null)
            throw new Error("Container symbol does not exist")
        if (containerSymbol.kind != SymbolKind.Struct && containerSymbol.kind != SymbolKind.Enum)
            throw new Error("Container symbol has unexpected kind")
        if (!containerSymbol.type.IsStruct() && !containerSymbol.type.IsEnum())
            throw new Error("Container symbol has unexpected type")
        if (!containerSymbol.alreadyDefined) {
            this.diagnostics.ReportError(
                accessorToken.GetLocation(),
                `Attempt to access member of forward declared but undefined symbol '${containerName}'`,
            )
        }
        if (containerSymbol.membersSymbolTable == null)
            throw new Error("Container symbol table is undefined")

        let memberIdentifier = syntax.memberIdentifier
        let identifierText = memberIdentifier.GetText()
        let memberSymbol = containerSymbol.membersSymbolTable.GetSymbolFromLocalScope(identifierText)
        if (memberSymbol == null) {
            this.diagnostics.ReportError(
                memberIdentifier.GetLocation(),
                `Undeclared member '${identifierText}'`,
            )
            return new BoundMissingExpression(syntax, this.symbolTable)
        }

        if (memberSymbol.kind == SymbolKind.Member) {
            return new BoundMemberAccessExpression(syntax, this.symbolTable, container, memberSymbol)
        } else if (memberSymbol.kind == SymbolKind.Enumvalue) {
            return new BoundEnumValueLiteral(syntax, this.symbolTable, containerType, memberSymbol)
        } else {
            throw new Error(`Unexpected member symbol kind ${memberSymbol.kind}`)
        }
    }

    private BindFunctionCallExpression(syntax: FuncCallExpressionSyntax): BoundFunctionCallExpression | BoundMissingExpression
    {
        let left = this.BindExpression(syntax.func)
        let leftParen = syntax.leftParen

        let funcSymbol = left.symbol
        if (funcSymbol == null) {
            this.diagnostics.ReportError(
                syntax.func.GetLocation(),
                `Expression '${syntax.func.GetLocation().GetText()}' is not a known symbol`,
            )
            return new BoundMissingExpression(syntax, this.symbolTable)
        }
        if (funcSymbol.kind != SymbolKind.Function) {
            // TODO: change this to a span that prints the whole left expression
            this.diagnostics.ReportError(
                syntax.func.GetLocation(),
                `Symbol '${funcSymbol.name}' is not a callable function`,
            )
            return new BoundMissingExpression(syntax, this.symbolTable)
        }

        let argumentList = []
        for (let index = 0; index < syntax.argumentsWithSeparators.length; index += 2) {
            let arg = syntax.argumentsWithSeparators[index]
            let boundArg = this.BindExpression(arg as ExpressionSyntax)
            argumentList.push(boundArg)
        }

        if (funcSymbol.membersSymbolTable == null)
            throw new Error("Missing membersSymbolTable")

        let parameterList = Array.from(funcSymbol.membersSymbolTable.symbols.values())
        if (argumentList.length != parameterList.length) {
            this.diagnostics.ReportError(
                leftParen.GetLocation(),
                `Function '${funcSymbol.name}' expects ${parameterList.length} arguments but ${argumentList.length} arguments were provided`,
            )
            return new BoundMissingExpression(syntax, this.symbolTable)
        }

        for (let index = 0; index < argumentList.length; index += 1) {
            let argumentType = argumentList[index].type
            let parameterType = parameterList[index].type
            this.CanConvertTypeImplicitlyOrError(argumentList[index].syntax!.GetLocation(), argumentType, parameterType)
        }

        return new BoundFunctionCallExpression(syntax, this.symbolTable, funcSymbol, argumentList)
    }

    private BindArrayIndexingExpression(syntax: ArrayIndexExpressionSyntax): BoundArrayIndexExpression
    {
        let leftBracket = syntax.leftBracket
        let array = this.BindExpression(syntax.array)
        let elemType
        if (array.type.IsArray()) {
            elemType = array.type.GetInnerType()
        } else {
            this.diagnostics.ReportError(
                leftBracket.GetLocation(),
                `Left hand side of array index operator '${leftBracket.GetText()}' is not of type array`,
            )
            elemType = Type.Any
        }
        let index = this.BindExpression(syntax.indexExpression)
        this.CanConvertTypeImplicitlyOrError(syntax.indexExpression.GetLocation(), index.type, Type.Number)

        return new BoundArrayIndexExpression(syntax, this.symbolTable, elemType, array, index)
    }

    private BindNameExpression(syntax: NameExpressionSyntax): BoundExpression
    {
        let identifier = syntax.identifier
        let identifierText = identifier.GetText()
        let symbol = this.symbolTable.GetSymbol(identifierText)
        if (symbol == null) {
            this.diagnostics.ReportError(
                identifier.GetLocation(),
                `Undeclared identifier '${identifierText}'`,
            )
            return new BoundMissingExpression(syntax, this.symbolTable)
        }
        return new BoundNameExpression(syntax, this.symbolTable, symbol.type, symbol)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    // Literals

    private BindNullLiteral(syntax: NullLiteralSyntax): BoundPrimitiveLiteral
    {
        return new BoundPrimitiveLiteral(BoundNodeKind.NullLiteral, syntax, this.symbolTable, Type.Null, "null")
    }

    private BindBoolLiteral(syntax: BoolLiteralSyntax): BoundPrimitiveLiteral
    {
        let value = syntax.boolLiteral.kind == SyntaxKind.FalseKeyword ? false : true
        let result = new BoundPrimitiveLiteral(BoundNodeKind.BoolLiteral, syntax, this.symbolTable, Type.Bool, value ? "true" : "false")
        result.boolValue = value
        return result
    }

    private BindNumberLiteral(syntax: NumberLiteralSyntax): BoundPrimitiveLiteral
    {
        let tokenText = syntax.numberLiteral.text ?? syntax.numberLiteral.toString()
        let result = new BoundPrimitiveLiteral(BoundNodeKind.NumberLiteral, syntax, this.symbolTable, Type.Number, tokenText)
        result.numValue = syntax.numberLiteral.numValue
        result.numValueIsHex = syntax.numberLiteral.numValueIsHex
        result.numValueIsFloat = syntax.numberLiteral.numValueIsFloat
        return result
    }

    private BindStringLiteral(syntax: StringLiteralSyntax): BoundPrimitiveLiteral
    {
        let tokenText = syntax.stringLiteral.text ?? syntax.stringLiteral.stringValue ?? "'<missing>'"
        let result = new BoundPrimitiveLiteral(BoundNodeKind.StringLiteral, syntax, this.symbolTable, Type.String, tokenText)
        result.stringValue = syntax.stringLiteral.stringValue
        return result
    }

    private BindArrayLiteral(syntax: ArrayLiteralSyntax): BoundArrayLiteral
    {
        let values = []
        for (let index = 0; index < syntax.elemsWithSeparators.length; index += 2) {
            let expression = syntax.elemsWithSeparators[index] as ExpressionSyntax
            let boundExpression = this.BindExpression(expression)
            values.push(boundExpression)
        }

        let elemType = Type.Any
        if (values.length > 0)
            elemType = values[0].type

        // Check that value types somewhat match
        for (let outer = 0; outer < values.length; outer += 1) {
            let a = values[outer]
            for (let inner = outer + 1; inner < values.length; inner += 1) {
                let b = values[inner]
                if (!a.type.CanImplicitlyConvertTo(b.type)) {
                    this.diagnostics.ReportError(
                        b.syntax!.GetLocation(),
                        `Cannot implicitly convert type '${a.type.PrettyPrint()}' of element ${outer + 1} in array initializer to array elements ${inner + 1} type '${b.type.PrettyPrint()}'`,
                    )
                    elemType = Type.Any
                }
                if (!b.type.CanImplicitlyConvertTo(a.type)) {
                    this.diagnostics.ReportError(
                        b.syntax!.GetLocation(),
                        `Cannot implicitly convert type '${b.type.PrettyPrint()}' of element ${inner + 1} in array initializer to array elements ${outer + 1} type '${a.type.PrettyPrint()}'`,
                    )
                    elemType = Type.Any
                }
            }
        }
        let arrayType = new ArrayType(elemType)

        return new BoundArrayLiteral(syntax, this.symbolTable, arrayType, values)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    // Types and specials

    private BindType(syntax: TypeExpressionSyntax): Type
    {
        if (syntax instanceof NullableTypeExpressionSyntax) {
            let baseType = this.BindType(syntax.baseType)
            return new NullableType(baseType)
        }

        if (syntax instanceof ArrayTypeExpressionSyntax) {
            let elemType = this.BindType(syntax.elemType)
            return new ArrayType(elemType)
        }

        if (syntax instanceof BaseTypeExpressionSyntax) {
            switch (syntax.typeIdentifier.kind) {
                case SyntaxKind.AnyKeyword:
                    return Type.Any
                case SyntaxKind.NullKeyword:
                    return Type.Null
                case SyntaxKind.BoolKeyword:
                    return Type.Bool
                case SyntaxKind.NumberKeyword:
                    return Type.Number
                case SyntaxKind.StringKeyword:
                    return Type.String
                case SyntaxKind.IdentifierToken: {
                    let typeName = syntax.typeIdentifier.GetText()
                    let symbol = this.symbolTable.GetSymbol(typeName)
                    if (symbol != null) {
                        if (symbol.kind == SymbolKind.Struct) {
                            return new BaseType(BaseTypeKind.Struct, typeName)
                        } else if (symbol.kind == SymbolKind.Enum) {
                            return new BaseType(BaseTypeKind.Enum, typeName)
                        }
                    }
                    this.diagnostics.ReportError(
                        syntax.typeIdentifier.GetLocation(),
                        `SyntaxToken '${syntax.typeIdentifier.GetText()}' is not a type`
                    )
                    return Type.Any
                }
            }
            throw new Error(`Unexpected syntax in type expression ${syntax.typeIdentifier.kind}`)
        }

        throw new Error(`unreachable`)
    }

    private BindTypeCastExpression(syntax: TypeCastExpressionSyntax): BoundTypeCastExpression
    {
        let expression = this.BindExpression(syntax.expression)
        let targetType = this.BindType(syntax.targetType)
        if (!expression.type.CanExplicitlyConvertTo(targetType)) {
            this.diagnostics.ReportError(
                syntax.asKeyword.GetLocation(),
                `Cast from type '${expression.type.PrettyPrint()}' to type '${targetType.PrettyPrint()}' is impossible`,
            )
        }
        return new BoundTypeCastExpression(syntax, this.symbolTable, targetType, expression)
    }

    private WrapInBlockStatementIfNecessary(node: BoundStatement): BoundBlockStatement
    {
        if (node instanceof BoundBlockStatement)
            return node

        let statements = [node]
        return new BoundBlockStatement(null, this.symbolTable, statements)
    }

    // Turns {{{ a; {b c} d; e; }}} . { a; {b c} d; e; }
    private FlattenBlockStatementIfNecessary(node: BoundStatement): BoundStatement
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
    // Helpers

    private PushNewSymbolTable(): SymbolTable
    {
        this.symbolTable = new SymbolTable(this.symbolTable)
        return this.symbolTable
    }

    private PushCustomSymbolTable(table: SymbolTable): SymbolTable
    {
        if (table.parent != this.symbolTable)
            throw new Error(`Cannot push symboltable - hierarchy would be broken`)

        this.symbolTable = table
        return table
    }

    private PopSymbolTable(): SymbolTable
    {
        if (this.symbolTable.parent == null)
            throw new Error("Cannot pop symboltable further")
        let result = this.symbolTable
        this.symbolTable = this.symbolTable.parent
        return result
    }

    private CanConvertTypeImplicitlyOrError(diagnosticLocation: SourceLocation, from: Type, to: Type): boolean
    {
        if (from.CanImplicitlyConvertTo(to))
            return true

        if (this.CanConvertTypeExplicitlyOrErro(diagnosticLocation, from, to)) {
            this.diagnostics.ReportError(
                diagnosticLocation,
                `Cannot implicitly convert from type '${from.PrettyPrint()}' to '${to.PrettyPrint()} without casting'`,
            )
        }
        return false
    }

    private CanConvertTypeExplicitlyOrErro(diagnosticLocation: SourceLocation, from: Type, to: Type): boolean
    {
        if (from.CanExplicitlyConvertTo(to))
            return true

        this.diagnostics.ReportError(
            diagnosticLocation,
            `Impossible to convert from type '${from.PrettyPrint()}' to '${to.PrettyPrint()}'`,
        )
        return false

    }

    private TryCreateSymbolOrError(diagnosticLocation: SourceLocation, symName: string, kind: SymbolKind, scopeKind: SymbolScopeKind, type: Type): Symbol | null
    {
        let existingLocal = this.symbolTable.GetSymbolFromLocalScope(symName)
        if (existingLocal != null) {
            // We don't allow overwriting symbols from our local scope
            this.diagnostics.ReportError(
                diagnosticLocation,
                `A symbol with the same name '${symName}' already exist in the current scope`,
            )
            return null
        }

        let existing = this.symbolTable.GetSymbol(symName)
        if (existing != null) {
            // We don't allow shadowing parameter type symbols from our surrounding scope
            if (existing.kind == SymbolKind.Parameter) {
                this.diagnostics.ReportError(
                    diagnosticLocation,
                    `A function parameter with the same name '${symName}' already exist in the current scope`,
                )
                return null
            }
        }

        return this.symbolTable.AddSymbol(symName, kind, scopeKind, type)
    }

    private GetOrCreateSymbolWithSpecificType(diagnosticLocation: SourceLocation, symName: string, kind: SymbolKind, scope: SymbolScopeKind, type: Type): Symbol | null
    {
        let existing = this.symbolTable.GetSymbol(symName)
        if (existing != null) {
            if (existing.kind != kind) {
                this.diagnostics.ReportError(
                    diagnosticLocation,
                    `Another symbol with the same name '${symName}' but different kind '${existing.kind}' was already declared in current scope`,
                )
                return null
            }
            if (existing.scopeKind != scope) {
                this.diagnostics.ReportError(
                    diagnosticLocation,
                    `Symbol '${symName} was already previously declared with scope attribute '${existing.scopeKind}' which differs from current scope attribute '${scope}'`,
                )
                return null
            }
            if (!Type.Identical(existing.type, type)) {
                if (kind == SymbolKind.Function) {
                    this.diagnostics.ReportError(
                        diagnosticLocation,
                        `Return type '${type.PrettyPrint()}' of function symbol '${symName}' does not match return type '${existing.type.PrettyPrint()}' of a previous declaration`,
                    )
                } else {
                    this.diagnostics.ReportError(
                        diagnosticLocation,
                        `Type '${type.PrettyPrint()}' of symbol '${symName}' does not match type '${existing.type.PrettyPrint()}' of a previous declaration`,
                    )
                }
                return null
            }
        }
        if (existing == null) {
            return this.symbolTable.AddSymbol(symName, kind, scope, type)!
        }

        return existing
    }

    private AreLiteralsEqual(a: BoundExpression, b: BoundExpression): boolean
    {
        if (a instanceof BoundPrimitiveLiteral && b instanceof BoundPrimitiveLiteral) {
            if (a.kind == b.kind) {
                switch (a.kind) {
                    case BoundNodeKind.NullLiteral:
                        return true
                    case BoundNodeKind.BoolLiteral:
                        return a.boolValue == b.boolValue
                    case BoundNodeKind.NumberLiteral:
                        return a.numValue == b.numValue && a.numValueIsFloat == b.numValueIsFloat
                    case BoundNodeKind.StringLiteral:
                        return a.stringValue == b.stringValue
                    default:
                        throw new Error(`Unexpected literal kind ${a.kind}`)
                }
            }
        }
        return false
    }
}
