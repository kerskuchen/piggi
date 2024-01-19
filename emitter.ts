// deno-lint-ignore-file prefer-const

import { BoundArrayIndexExpression, BoundArrayLiteral, BoundBinaryExpression, BoundBinaryOperatorKindToString, BoundBlockStatement, BoundBreakStatement, BoundCaseStatement, BoundCompilationUnit, BoundContinueStatement, BoundDoWhileStatement, BoundExpression, BoundExpressionStatement, BoundForStatement, BoundFunctionCallExpression, BoundIfStatement, BoundMemberAccessExpression, BoundMissingExpression, BoundNameExpression, BoundNodeKind, BoundParenthesizedExpression, BoundPrimitiveLiteral, BoundReturnStatement, BoundStatement, BoundSwitchStatement, BoundTernaryConditionalExpression, BoundThisExpression, BoundTypeCastExpression, BoundUnaryExpression, BoundUnaryOperatorKindToString, BoundVariableDeclarationStatement, BoundWhileStatement } from "./boundtree.ts"
import { Symbol, SymbolKind } from "./symbols.ts"
import { ArrayType, BaseType, BaseTypeKind, NullableType, Type } from "./types.ts"

export class Emitter
{
    public output = ""
    public indentationLevel = 0
    public currentFunctionSymbol: Symbol | null = null

    constructor(
    ) { }

    EmitCompilationUnit(unit: BoundCompilationUnit): string
    {
        this.EmitPreamble()

        this.EmitHeadline("Global variables")
        this.output += `// NOTE: The following are initialized in the __GlobalVariableInitializer() function`
        this.EmitNewLine()
        for (let varSym of unit.globalVars.values()) {
            this.EmitGlobalVariableDeclaration(varSym)
        }
        this.EmitNewLine()

        this.EmitHeadline("Enums")
        for (let enumSym of unit.enums.values()) {
            this.EmitEnumDeclaration(enumSym)
        }

        this.EmitHeadline("Classes")
        for (let structSym of unit.structs.values()) {
            this.EmitStructDeclaration(structSym)
        }

        this.EmitHeadline("Free Functions")
        for (let funcSym of unit.functions.values()) {
            this.EmitFunctionDeclaration(funcSym)
        }

        // We emit a global variable initialzer function which is called first thing before our Main() function
        this.EmitGlobalVariableInitializerFunction(unit)
        this.EmitPostamble()

        return this.output
    }

    EmitGlobalVariableInitializerFunction(unit: BoundCompilationUnit)
    {
        this.output += `function __GlobalVariableInitializer() {`
        this.indentationLevel += 1
        this.EmitNewLine()

        for (let [index, varSymbol] of unit.resolvedSortedGlobalVariableInitializers.entries()) {
            if (varSymbol.kind == SymbolKind.MemberVariable)
                this.output += `${varSymbol.parent!.name}.${varSymbol.name} = `
            else
                this.output += `${varSymbol.name} = `

            this.EmitExpression(varSymbol.initializer!)
            if (index == unit.resolvedSortedGlobalVariableInitializers.length - 1)
                this.indentationLevel -= 1
            this.EmitNewLine()
        }
        this.output += `}` // constructor
        this.EmitNewLine()
        this.EmitNewLine()
    }

    private EmitPreamble()
    {
        this.output = ""

        this.EmitHeadline("Preamble")
        this.output += "// deno-lint-ignore-file prefer-const"
        this.EmitNewLine()

        this.EmitNewLine()
        this.output += `function Assert(value) { if (!value) throw new Error("assert") }`
        this.EmitNewLine()
        this.output += `function PrintValue(value) { console.log(value) }`
        this.EmitNewLine()
        this.EmitNewLine()
    }

    private EmitPostamble()
    {
        this.EmitHeadline("Postamble")
        this.output += "__GlobalVariableInitializer()"
        this.EmitNewLine()
        this.output += "Main()"
        this.EmitNewLine()
    }

    private EmitEnumDeclaration(symbol: Symbol)
    {
        if (symbol.isExternal)
            return

        this.output += `class ${symbol.name} {`
        this.indentationLevel += 1
        this.EmitNewLine()

        let enumValues =
            Array.from(symbol.membersSymbolTable!.symbols.values())
                .filter((sym) => sym.kind == SymbolKind.EnumValue)
        let firstValueName = null
        for (let [index, enumValue] of enumValues.entries()) {
            if (firstValueName == null)
                firstValueName = `${enumValue.name}`
            this.output += `static ${enumValue.name} = ${enumValue.enumValue}`
            if (index == enumValues.length - 1)
                this.indentationLevel -= 1
            this.EmitNewLine()
        }
        if (firstValueName != null) {
            this.EmitIndentation(this.indentationLevel + 1)
            this.output += `static __Default = ${symbol.name}.${firstValueName}`
            this.EmitNewLine()
        }

        this.output += `}`
        this.EmitNewLine()
        this.EmitNewLine()
    }

    private EmitStructDeclaration(symbol: Symbol)
    {
        if (symbol.isExternal)
            return

        this.output += `class ${symbol.name} {`
        this.indentationLevel += 1
        this.EmitNewLine()

        // Main constructor
        this.output += `constructor(`
        let fields = Array.from(symbol.membersSymbolTable!.symbols.values())
            .filter((sym) => sym.kind == SymbolKind.MemberField)
        for (let [index, field] of fields.entries()) {
            if (field.kind != SymbolKind.MemberField)
                continue
            this.output += field.name
            if (index != fields.length - 1)
                this.output += ', '
        }
        this.output += ") "

        this.output += "{"
        this.indentationLevel += 1
        this.EmitNewLine()
        for (let [index, field] of fields.entries()) {
            if (field.kind != SymbolKind.MemberField)
                continue
            this.output += `this.${field.name} = ${field.name}`
            if (index == fields.length - 1)
                this.indentationLevel -= 1
            this.EmitNewLine()
        }
        this.output += `}` // constructor
        this.EmitNewLine()

        // Empty constructor
        this.output += `static Default() {`
        this.indentationLevel += 1
        this.EmitNewLine()
        this.output += `return new ${symbol.name}(`
        this.indentationLevel += 1
        this.EmitNewLine()
        for (let [index, field] of fields.entries()) {
            if (field.kind != SymbolKind.MemberField)
                continue
            this.EmitDefaultValueForType(field.type)
            this.output += ','
            this.output += ` // ${field.name}`
            if (index == fields.length - 1)
                this.indentationLevel -= 1
            this.EmitNewLine()
        }
        this.output += `)`
        this.indentationLevel -= 1
        this.EmitNewLine()
        this.output += `}` // static Default()
        this.EmitNewLine()

        // Associated static variables
        let staticVars = Array.from(symbol.membersSymbolTable!.symbols.values())
            .filter((sym) => sym.kind == SymbolKind.MemberVariable)
        if (staticVars.length > 0) {
            this.output += `// NOTE: The following are initialized in the __GlobalVariableInitializer() function`
            this.EmitNewLine()
            for (let staticVar of staticVars) {
                this.EmitGlobalVariableDeclaration(staticVar)
            }
            this.EmitNewLine()
        }

        // Associated static functions
        let staticFuncs = Array.from(symbol.membersSymbolTable!.symbols.values())
            .filter((sym) => sym.kind == SymbolKind.MemberFunction)
        for (let staticFunc of staticFuncs) {
            this.EmitFunctionDeclaration(staticFunc)
        }

        // Associated member methods
        let memberFuncs = Array.from(symbol.membersSymbolTable!.symbols.values())
            .filter((sym) => sym.kind == SymbolKind.MemberMethod)
        for (let memberFunc of memberFuncs) {
            this.EmitFunctionDeclaration(memberFunc)
        }

        this.indentationLevel -= 1
        this.EmitNewLine()
        this.output += `}` // class
        this.EmitNewLine()
        this.EmitNewLine()

        return symbol
    }

    private EmitFunctionDeclaration(symbol: Symbol)
    {
        if (symbol.isExternal)
            return

        if (symbol.kind == SymbolKind.MemberFunction)
            this.output += `static `
        else if (symbol.kind == SymbolKind.Function)
            this.output += `function `

        this.output += `${symbol.name}(`

        let params = Array.from(symbol.membersSymbolTable!.symbols.values())
        for (let [index, param] of params.entries()) {
            this.output += param.name
            this.EmitTypeComment(param.type)
            if (index != params.length - 1)
                this.output += ', '
        }
        this.output += `) `
        if (symbol.type != Type.Void)
            this.EmitTypeComment(symbol.type)

        this.currentFunctionSymbol = symbol
        this.EmitBlockStatement(symbol.functionBody!)
        this.currentFunctionSymbol = null
        this.EmitNewLine()
        this.EmitNewLine()
    }

    private EmitGlobalVariableDeclaration(symbol: Symbol)
    {
        if (symbol.isExternal)
            return

        if (symbol.kind == SymbolKind.MemberVariable)
            this.output += `static `
        else
            this.output += `let `

        this.output += `${symbol.name}`
        // NOTE: We don't emit the initializer here, this happens later in a special globalinitializer function
        this.output += ` /*: ${symbol.type.PrettyPrint()} = `
        this.EmitExpression(symbol.initializer!)
        this.output += ` */`

        this.EmitNewLine()
    }

    ////////////////////////////////////////////////////////////////////////////////////////////
    // Statements

    EmitStatement(node: BoundStatement)
    {
        // TODO: add leading trivia here
        // for this we need some kind of node.syntax.GetLeadingTrivia()

        switch (node.kind) {
            case BoundNodeKind.ExpressionStatement:
                this.EmitExpressionStatement(node as BoundExpressionStatement)
                break
            case BoundNodeKind.BlockStatement:
                this.EmitBlockStatement(node as BoundBlockStatement)
                break
            case BoundNodeKind.VariableDeclarationStatement:
                this.EmitVariableDeclaration(node as BoundVariableDeclarationStatement)
                break
            case BoundNodeKind.IfStatement:
                this.EmitIfStatement(node as BoundIfStatement)
                break
            case BoundNodeKind.WhileStatement:
                this.EmitWhileStatement(node as BoundWhileStatement)
                break
            case BoundNodeKind.DoWhileStatement:
                this.EmitDoWhileStatement(node as BoundDoWhileStatement)
                break
            case BoundNodeKind.ForStatement:
                this.EmitForStatement(node as BoundForStatement)
                break
            case BoundNodeKind.ReturnStatement:
                this.EmitReturnStatement(node as BoundReturnStatement)
                break
            case BoundNodeKind.BreakStatement:
                this.EmitBreakStatement(node as BoundBreakStatement)
                break
            case BoundNodeKind.ContinueStatement:
                this.EmitContinueStatement(node as BoundContinueStatement)
                break
            case BoundNodeKind.SwitchStatement:
                this.EmitSwitchStatement(node as BoundSwitchStatement)
                break
            default:
                throw new Error(`Unexpected statement in rewriter: ${node.kind}`)
        }

        // TODO: add leading trivia here
        // for this we need some kind of node.syntax.GetTrailingTrivia()
    }

    private EmitExpressionStatement(node: BoundExpressionStatement)
    {
        this.EmitExpression(node.expression)
    }

    private EmitBlockStatement(node: BoundBlockStatement)
    {
        if (node.statements.length == 0) {
            this.output += "{}"
        } else {
            this.output += "{"

            this.indentationLevel += 1
            this.EmitNewLine()

            for (let [index, statement] of node.statements.entries()) {
                this.EmitStatement(statement)
                if (index == node.statements.length - 1)
                    this.indentationLevel -= 1
                this.EmitNewLine()
            }
            this.output += "}"
        }
        return node
    }

    private EmitVariableDeclaration(node: BoundVariableDeclarationStatement)
    {
        if (node.symbol.kind == SymbolKind.LocalPersistVariable) {
            if (this.currentFunctionSymbol == null)
                throw new Error("Local persist variable must be contained in a function")
            if (node.initializer == null)
                throw new Error("Local persist variable must have initializer in emitter")

            this.output += `if (typeof ${this.currentFunctionSymbol.name}.${node.symbol.name} == 'undefined') {`
            this.indentationLevel += 1
            this.EmitNewLine()

            this.output += `${this.currentFunctionSymbol.name}.${node.symbol.name} = `
            this.EmitExpression(node.initializer)

            this.indentationLevel -= 1
            this.EmitNewLine()
            this.output += "}"
        } else {
            this.output += `let ${node.symbol.name}`

            if (node.initializer != null) {
                this.output += ` = `
                this.EmitExpression(node.initializer)
            } else {
                this.EmitTypeComment(node.symbol.type)
            }
        }
    }

    private EmitIfStatement(node: BoundIfStatement)
    {
        this.output += "if ("
        this.EmitExpression(node.condition)
        this.output += ") "

        this.EmitStatement(node.thenStatement)
        if (node.elseStatement != null) {
            this.output += " else "
            this.EmitStatement(node.elseStatement)
        }
        return node
    }

    private EmitWhileStatement(node: BoundWhileStatement)
    {
        this.output += "while ("
        this.EmitExpression(node.condition)
        this.output += ")"
        this.EmitStatement(node.body)
        return node
    }

    private EmitDoWhileStatement(node: BoundDoWhileStatement)
    {
        this.output += "do "
        this.EmitStatement(node.body)
        this.output += "while ("
        this.EmitExpression(node.condition)
        this.output += ")"
        return node
    }

    private EmitForStatement(node: BoundForStatement)
    {
        this.output += `for (let ${node.iteratorSymbol.name} = `
        this.EmitExpression(node.lowerBound)
        this.output += `; ${node.iteratorSymbol.name} <`
        if (node.upperBoundIsInclusive)
            this.output += "= "
        else
            this.output += " "
        this.EmitExpression(node.upperBound)
        this.output += `; ${node.iteratorSymbol.name} += 1) `
        this.EmitStatement(node.body)
        return node
    }

    private EmitReturnStatement(node: BoundReturnStatement)
    {
        this.output += "return "
        if (node.returnExpression != null) {
            this.EmitExpression(node.returnExpression)
        }
        return node
    }

    private EmitBreakStatement(node: BoundBreakStatement)
    {
        this.output += "break"
        return node
    }

    private EmitContinueStatement(node: BoundContinueStatement)
    {
        this.output += "continue"
        return node
    }

    private EmitSwitchStatement(node: BoundSwitchStatement)
    {
        this.output += "switch ("
        this.EmitExpression(node.switchExpression)
        this.output += ") {"

        this.indentationLevel += 1
        this.EmitNewLine()

        for (let [index, statement] of node.caseStatements.entries()) {
            this.EmitCaseStatement(statement)
            if (index == node.caseStatements.length - 1)
                this.indentationLevel -= 1
            this.EmitNewLine()
        }

        this.output += "}"
    }

    private EmitCaseStatement(node: BoundCaseStatement)
    {
        if (node.caseExpression == null) {
            this.output += "default: "
        } else {
            this.output += "case "
            this.EmitExpression(node.caseExpression)
            this.output += ": "
        }
        if (node.body != null)
            this.EmitStatement(node.body)
        else
            this.output += "// Fallthrough"
    }

    ////////////////////////////////////////////////////////////////////////////////////////////
    // Expressions

    private EmitExpression(node: BoundExpression)
    {
        switch (node.kind) {
            case BoundNodeKind.MissingExpression:
                this.EmitMissingExpression(node as BoundMissingExpression)
                break
            case BoundNodeKind.TypeCastExpression:
                this.EmitTypeCastExpression(node as BoundTypeCastExpression)
                break
            case BoundNodeKind.ParenthesizedExpression:
                this.EmitParenthesizedExpression(node as BoundParenthesizedExpression)
                break
            case BoundNodeKind.NameExpression:
                this.EmitNameExpression(node as BoundNameExpression)
                break
            case BoundNodeKind.ThisExpression:
                this.EmitThisExpression(node as BoundThisExpression)
                break
            case BoundNodeKind.UnaryExpression:
                this.EmitUnaryExpression(node as BoundUnaryExpression)
                break
            case BoundNodeKind.BinaryExpression:
                this.EmitBinaryExpression(node as BoundBinaryExpression)
                break
            case BoundNodeKind.TernaryConditionalExpression:
                this.EmitTernaryConditionalExpression(node as BoundTernaryConditionalExpression)
                break
            case BoundNodeKind.FunctionCallExpression:
                this.EmitFunctionCallExpression(node as BoundFunctionCallExpression)
                break
            case BoundNodeKind.ArrayIndexExpression:
                this.EmitArrayIndexExpression(node as BoundArrayIndexExpression)
                break
            case BoundNodeKind.MemberAccessExpression:
                this.EmitMemberAccessExpression(node as BoundMemberAccessExpression)
                break
            case BoundNodeKind.NullLiteral:
            case BoundNodeKind.BoolLiteral:
            case BoundNodeKind.NumberLiteral:
            case BoundNodeKind.StringLiteral:
                this.EmitPrimitiveLiteral(node as BoundPrimitiveLiteral)
                break
            case BoundNodeKind.ArrayLiteral:
                this.EmitArrayLiteral(node as BoundArrayLiteral)
                break
            default:
                throw new Error(`Unexpected expression in rewriter: ${node.kind}`)
        }
    }

    private EmitMissingExpression(node: BoundMissingExpression)
    {
        this.output += `/* missing expression '${node.syntax!.GetLocation().GetText()}' */`
    }

    private EmitNameExpression(node: BoundNameExpression)
    {
        if (node.symbol == null)
            throw new Error("Symbol is null in name expression in emitter")

        if (node.symbol.kind == SymbolKind.LocalPersistVariable) {
            if (this.currentFunctionSymbol == null)
                throw new Error("Local persist variable must be contained in a function in emitter")

            this.output += `${this.currentFunctionSymbol.name}.${node.symbol.name}`
        } else {
            this.output += node.symbol.name
        }
    }

    private EmitThisExpression(_node: BoundThisExpression)
    {
        this.output += "this"
    }


    private EmitParenthesizedExpression(node: BoundParenthesizedExpression)
    {
        this.output += '('
        this.EmitExpression(node.inner)
        this.output += ')'
    }

    private EmitUnaryExpression(node: BoundUnaryExpression)
    {
        this.output += `${BoundUnaryOperatorKindToString(node.operator)} `
        this.EmitExpression(node.operand)
    }

    private EmitBinaryExpression(node: BoundBinaryExpression)
    {
        this.EmitExpression(node.left)
        this.output += ` ${BoundBinaryOperatorKindToString(node.operator)} `
        this.EmitExpression(node.right)
    }

    private EmitTernaryConditionalExpression(node: BoundTernaryConditionalExpression)
    {
        this.EmitExpression(node.condition)
        this.output += ' ? '
        this.EmitExpression(node.thenExpression)
        this.output += ' : '
        this.EmitExpression(node.elseExpression)
    }

    private EmitFunctionCallExpression(node: BoundFunctionCallExpression)
    {
        if (node.isConstructor && node.args.length > 0) {
            this.output += "new "
        }

        this.EmitExpression(node.left)

        if (node.isConstructor && node.args.length == 0) {
            this.output += ".Default"
        }

        this.output += '('
        for (let [index, arg] of node.args.entries()) {
            this.EmitExpression(arg)
            if (index != node.args.length - 1)
                this.output += ', '
        }
        this.output += ')'
    }

    private EmitTypeCastExpression(node: BoundTypeCastExpression)
    {
        this.EmitExpression(node.expression)
    }

    private EmitMemberAccessExpression(node: BoundMemberAccessExpression)
    {
        this.EmitExpression(node.container)
        this.output += '.'
        this.output += node.memberSymbol.name
    }

    private EmitArrayIndexExpression(node: BoundArrayIndexExpression)
    {
        this.EmitExpression(node.array)
        this.output += '['
        this.EmitExpression(node.index)
        this.output += ']'
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    // Literals

    private EmitPrimitiveLiteral(node: BoundPrimitiveLiteral)
    {
        this.output += node.tokenText
    }

    private EmitEnumValueLiteral(node: BoundMemberAccessExpression)
    {
        this.output += `${node.container.type.PrettyPrint()}_${node.memberSymbol.name}`
    }

    private EmitArrayLiteral(node: BoundArrayLiteral)
    {
        this.output += '['
        for (let [index, value] of node.values.entries()) {
            this.EmitExpression(value)
            if (index != node.values.length - 1)
                this.output += ', '
        }
        this.output += ']'
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // Helpers

    EmitDefaultValueForType(type: Type)
    {
        if (type instanceof BaseType) {
            switch (type.baseKind) {
                case BaseTypeKind.Void:
                    throw new Error("Unexpected void type")
                case BaseTypeKind.Null:
                    throw new Error("Unexpected null type")
                case BaseTypeKind.Any:
                    this.output += "null"
                    break
                case BaseTypeKind.Bool:
                    this.output += "false"
                    break
                case BaseTypeKind.Number:
                    this.output += "0"
                    break
                case BaseTypeKind.String:
                    this.output += ""
                    break
                case BaseTypeKind.Struct:
                    this.output += `${type.name}`
                    break
                case BaseTypeKind.Enum:
                    this.output += `${type.name}.__Default`
                    break
                default:
                    throw new Error("Unreachable")
            }

        } else if (type instanceof NullableType) {
            this.output += "null"
        } else if (type instanceof ArrayType) {
            this.output += "[]"
        } else {
            throw new Error("Unreachable")
        }
    }
    private EmitTypeComment(type: Type)
    {
        this.output += ` /*: ${type.PrettyPrint()}*/ `
    }

    private EmitIndentation(level: number)
    {
        for (let index = 0; index < level; index += 1) {
            this.output += "    "
        }
    }
    private EmitNewLine()
    {
        this.output += "\n"
        this.EmitIndentation(this.indentationLevel)
    }

    private EmitHeadline(name: string)
    {
        this.output += `////////////////////////////////////////////////////////////////////////////////////////////////////
// ${name}`
        this.EmitNewLine()
        this.EmitNewLine()
    }
}