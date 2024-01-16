// deno-lint-ignore-file prefer-const

import { BoundArrayIndexExpression, BoundArrayLiteral, BoundBinaryExpression, BoundBinaryOperatorKindToString, BoundBlockStatement, BoundBreakStatement, BoundCaseStatement, BoundCompilationUnit, BoundContinueStatement, BoundDoWhileStatement, BoundEnumDeclaration, BoundEnumValueLiteral, BoundExpression, BoundExpressionStatement, BoundForStatement, BoundFunctionCallExpression, BoundFunctionDeclaration, BoundIfStatement, BoundImportDeclaration, BoundMemberAccessExpression, BoundMissingExpression, BoundMissingStatement, BoundNameExpression, BoundNodeKind, BoundParenthesizedExpression, BoundPrimitiveLiteral, BoundReturnStatement, BoundStatement, BoundStructDeclaration, BoundSwitchStatement, BoundTernaryConditionalExpression, BoundTypeCastExpression, BoundUnaryExpression, BoundUnaryOperatorKindToString, BoundVariableDeclaration, BoundWhileStatement } from "./boundtree.ts"
import { SymbolScopeKind } from "./symbols.ts"
import { SyntaxFacts } from "./syntax.ts"
import { ArrayType, BaseType, BaseTypeKind, NullableType, Type } from "./types.ts"

export class Emitter
{
    constructor(
        public output: string = "",
        public indentationLevel = 0,
    ) { }

    EmitCompilationUnit(unit: BoundCompilationUnit): string
    {
        this.EmitPreamble()
        for (let declaration of unit.globalDeclarations) {
            this.EmitModuleStatement(declaration)
        }
        this.EmitPostamble()

        return this.output
    }

    private EmitPreamble()
    {
        this.output = ""
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
        this.output += "Main()"
    }

    private EmitModuleStatement(node: BoundStatement)
    {
        // TODO: add leading trivia here
        // for this we need some kind of node.syntax.GetLeadingTrivia()

        switch (node.kind) {
            case BoundNodeKind.ImportDeclaration:
                this.EmitImportDeclaration(node as BoundImportDeclaration)
                break
            case BoundNodeKind.VariableDeclaration:
                this.EmitGlobalVariableDeclaration(node as BoundVariableDeclaration)
                break
            case BoundNodeKind.EnumDeclaration:
                this.EmitEnumDeclaration(node as BoundEnumDeclaration)
                break
            case BoundNodeKind.StructDeclaration:
                this.EmitStructDeclaration(node as BoundStructDeclaration)
                break
            case BoundNodeKind.FunctionDeclaration:
                this.EmitFunctionDeclaration(node as BoundFunctionDeclaration)
                break
            default:
                throw new Error(`Unexpected module declaration in rewriter: ${node.kind}`)
        }

        // TODO: add leading trivia here
        // for this we need some kind of node.syntax.GetTrailingTrivia()
    }

    private EmitMissingStatement(node: BoundMissingStatement)
    {
        this.output += `/* missing statement '${node.syntax!.GetLocation().GetText()}' */`
        this.EmitNewLine()
        return node
    }

    private EmitImportDeclaration(node: BoundImportDeclaration)
    {
        // this.output += `/* import ${node.modulename} */`
        // this.EmitNewLine()
        return node
    }

    private EmitStructDeclaration(node: BoundStructDeclaration)
    {
        if (node.isForwardDeclaration)
            return

        this.output += `class ${node.symbol.name} {`
        this.indentationLevel += 1
        this.EmitNewLine()

        this.output += `constructor(`
        let members = Array.from(node.symbol.membersSymbolTable!.symbols.values())
        for (let [index, member] of members.entries()) {
            this.output += member.name
            if (index != members.length - 1)
                this.output += ', '
        }
        this.output += ") "

        this.output += "{"
        this.indentationLevel += 1
        this.EmitNewLine()
        for (let [index, member] of members.entries()) {
            this.output += `this.${member.name} = ${member.name}`
            if (index == members.length - 1)
                this.indentationLevel -= 1
            this.EmitNewLine()
        }
        this.output += `}` // constructor
        this.EmitNewLine()
        this.EmitNewLine()

        this.output += `static Default() {`
        this.indentationLevel += 1
        this.EmitNewLine()
        this.output += `return new ${node.symbol.name}(`
        this.indentationLevel += 1
        this.EmitNewLine()
        for (let [index, member] of members.entries()) {
            this.EmitDefaultValueForType(member.type)
            this.output += ','
            this.output += ` // ${member.name}`
            if (index == members.length - 1)
                this.indentationLevel -= 1
            this.EmitNewLine()
        }
        this.output += `)`
        this.indentationLevel -= 1
        this.EmitNewLine()
        this.output += `}` // static Default()
        this.indentationLevel -= 1
        this.EmitNewLine()

        this.output += `}` // class
        this.EmitNewLine()
        this.EmitNewLine()

        return node
    }

    private EmitEnumDeclaration(node: BoundEnumDeclaration)
    {
        if (node.isForwardDeclaration)
            return

        this.output += `// enum ${node.symbol.name} {`
        this.indentationLevel += 1
        this.EmitNewLine()

        let enumValues = Array.from(node.symbol.membersSymbolTable!.symbols.values())
        let firstValueName = null
        for (let [index, enumValue] of enumValues.entries()) {
            if (firstValueName == null)
                firstValueName = `${node.symbol.name}_${enumValue.name}`
            this.output += `const ${node.symbol.name}_${enumValue.name} = ${enumValue.enumValue}`
            if (index == enumValues.length - 1)
                this.indentationLevel -= 1
            this.EmitNewLine()
        }
        if (firstValueName != null) {
            this.EmitIndentation(this.indentationLevel + 1)
            this.output += `const ${node.symbol.name}__Default = ${firstValueName}`
            this.EmitNewLine()
        }

        this.output += `// }`
        this.EmitNewLine()
        this.EmitNewLine()
    }

    private EmitFunctionDeclaration(node: BoundFunctionDeclaration)
    {
        if (node.isForwardDeclaration)
            return

        this.output += `function ${node.symbol.name}(`

        let params = Array.from(node.symbol.membersSymbolTable!.symbols.values())
        for (let [index, param] of params.entries()) {
            this.output += param.name
            if (index != params.length - 1)
                this.output += ', '
        }
        this.output += ") "
        this.EmitBlockStatement(node.body!)
        this.EmitNewLine()
        this.EmitNewLine()
    }

    private EmitGlobalVariableDeclaration(node: BoundVariableDeclaration)
    {
        if (node.symbol.scopeKind == SymbolScopeKind.Extern)
            return

        this.output += `let ${node.symbol.name}`
        if (node.initializer != null) {
            this.output += ` = `
            this.EmitExpression(node.initializer)
        }
        this.EmitNewLine()
    }

    ////////////////////////////////////////////////////////////////////////////////////////////
    // Statements

    EmitStatement(node: BoundStatement)
    {
        // TODO: add leading trivia here
        // for this we need some kind of node.syntax.GetLeadingTrivia()

        switch (node.kind) {
            case BoundNodeKind.MissingStatement:
                this.EmitMissingStatement(node as BoundMissingStatement)
                break
            case BoundNodeKind.ExpressionStatement:
                this.EmitExpressionStatement(node as BoundExpressionStatement)
                break
            case BoundNodeKind.BlockStatement:
                this.EmitBlockStatement(node as BoundBlockStatement)
                break
            case BoundNodeKind.VariableDeclaration:
                this.EmitVariableDeclaration(node as BoundVariableDeclaration)
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

    private EmitVariableDeclaration(node: BoundVariableDeclaration)
    {
        this.output += `let ${node.symbol.name}`

        if (node.initializer != null) {
            this.output += ` = `
            this.EmitExpression(node.initializer)
        }

        this.EmitNewLine()
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
            case BoundNodeKind.EnumValueLiteral:
                this.EmitEnumValueLiteral(node as BoundEnumValueLiteral)
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
        this.output += node.symbol!.name
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

        this.output += node.symbol!.name

        if (node.isConstructor && node.args.length == 0) {
            this.output += ".Default"
        }

        this.output += '('
        for (let [index, arg] of node.args.entries()) {
            this.EmitExpression(arg)
            if (index != node.args.length - 1)
                this.output += ','
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

    private EmitEnumValueLiteral(node: BoundEnumValueLiteral)
    {
        this.output += `${node.enumType.PrettyPrint()}_${node.enumValueSymbol.name}`
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
                    this.output += `${type.name}__Default`
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
}