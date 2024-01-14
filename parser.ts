// deno-lint-ignore-file prefer-const

import { DiagnosticBag, Source, SourceLocation } from "./common.ts"
import { Scanner } from "./scanner.ts"
import { SyntaxKind, SyntaxToken, SyntaxTrivia, SyntaxTree, BinaryExpressionSyntax, BlockStatementSyntax, BreakStatementSyntax, ContinueStatementSyntax, DoWhileStatementSyntax, ExpressionStatementSyntax, ExpressionSyntax, ForStatementSyntax, IfStatementSyntax, ModuleMemberSyntax, ModuleSyntax, NameExpressionSyntax, ParenthesizedExpressionSyntax, ReturnStatementSyntax, StatementSyntax, TypeCastExpressionSyntax, UnaryExpressionSyntax, VariableDeclarationStatementSyntax, WhileStatementSyntax, ImportDeclarationStatementSyntax, GlobalVariableDeclarationStatementSyntax, StructDeclarationStatementSyntax, StructDefinitionStatementSyntax, FunctionDeclarationStatementSyntax, FunctionDefinitionStatementSyntax, TypeExpressionSyntax, EnumDeclarationStatementSyntax, EnumDefinitionStatementSyntax, EnumValueClauseSyntax, SwitchStatementSyntax, CaseStatementSyntax, TernaryConditionalExpressionSyntax, FuncCallExpressionSyntax, MemberAccessExpressionSyntax, ArrayIndexExpressionSyntax, ArrayLiteralSyntax, NumberLiteralSyntax, StringLiteralSyntax, BoolLiteralSyntax, NullLiteralSyntax, ArrayTypeExpressionSyntax, BaseTypeExpressionSyntax, NullableTypeExpressionSyntax } from "./syntax.ts"
import { SyntaxFacts } from "./syntax.ts"

////////////////////////////////////////////////////////////////////////////////////////////////////
// Parser

export class Parser
{
    diagnostics: DiagnosticBag = new DiagnosticBag()
    tokens: SyntaxToken[] = []
    position = 0
    tree: SyntaxTree

    loopLevel = 0
    switchCaseLevel = 0
    functionLevel = 0

    debugToParse: SyntaxToken[] = []
    debugAlreadyParsed: SyntaxToken[] = []

    constructor(public source: Source)
    {
        this.tree = new SyntaxTree(source)
    }

    Parse(): SyntaxTree
    {
        let scanner = new Scanner(this.tree)

        let badTokens: SyntaxToken[] = []
        let token: SyntaxToken
        do {
            token = scanner.NextToken()

            if (token.kind == SyntaxKind.BadToken) {
                badTokens.push(token)
            } else {
                // Prepend previously produced bad tokens as leading trivia of the next valid token
                if (badTokens.length > 0) {
                    let leadingTrivia = token.leadingTrivia
                    let index = 0

                    for (let badToken of badTokens) {
                        for (let lt of badToken.leadingTrivia)
                            leadingTrivia.splice(index++, 0, lt)

                        let trivia = new SyntaxTrivia(SyntaxKind.SkippedTextTrivia, this.tree, badToken.location)
                        leadingTrivia.splice(index++, 0, trivia)

                        for (let tt of badToken.trailingTrivia)
                            leadingTrivia.splice(index++, 0, tt)
                    }

                    badTokens.length = 0
                    token = new SyntaxToken(token.kind, token.tree, token.location, token.text, leadingTrivia, token.trailingTrivia)
                }

                this.tokens.push(token)
            }
        } while (token.kind != SyntaxKind.EndOfFileToken)

        // Some verifications
        let tokenTextLengthSum = 0
        for (let token of this.tokens) {
            tokenTextLengthSum += token.GetLocationIncludingTrivia().length
        }
        if (tokenTextLengthSum != this.source.content.length) {
            throw new Error(`All tokens text length = ${tokenTextLengthSum} != ${this.source.content.length} = source text length`)
        }

        // for (let token of this.tokens)
        //     console.log(token.PrettyPrint())

        this.debugToParse = this.tokens
        this.debugAlreadyParsed = []

        let root = this.ParseModule(this.tree.source.modulename)

        this.tree.AssignRoot(root)
        return this.tree
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // Module

    private ParseModule(moduleName: string): ModuleSyntax
    {
        let members = this.ParseModuleMembers()
        let endOfFileToken = this.MatchAndAdvanceToken(SyntaxKind.EndOfFileToken)
        return new ModuleSyntax(this.tree, moduleName, members, endOfFileToken)
    }

    private ParseModuleMembers(): ModuleMemberSyntax[]
    {
        let members = []

        while (this.Current().kind != SyntaxKind.EndOfFileToken) {
            let startToken = this.Current()

            let member = this.ParseModuleMember()
            if (member != null)
                members.push(member)

            // If ParseModuleMember() did not consume any tokens,
            // we need to skip the current token and continue
            // in order to avoid an infinite loop.
            //
            // We don't need to report an error, because we'll
            // already tried to parse an expression statement
            // and reported one.
            if (this.Current() == startToken)
                this.AdvanceToken()
        }

        return members
    }

    private ParseModuleMember(): ModuleMemberSyntax | null
    {
        if (this.Current().kind == SyntaxKind.ImportKeyword)
            return this.ParseImportStatement()

        let externKeyword: SyntaxToken | null = null
        if (this.Current().kind == SyntaxKind.ExternKeyword)
            externKeyword = this.MatchAndAdvanceToken(SyntaxKind.ExternKeyword)

        if (this.Current().kind == SyntaxKind.LetKeyword)
            return this.ParseGlobalVariableDeclaration(externKeyword)
        if (this.Current().kind == SyntaxKind.StructKeyword)
            return this.ParseStructDefinition(externKeyword)
        if (this.Current().kind == SyntaxKind.FunKeyword)
            return this.ParseFunctionDefinition(externKeyword)
        if (this.Current().kind == SyntaxKind.EnumKeyword)
            return this.ParseEnumDefinition(externKeyword)

        this.tree.diagnostics.ReportError(
            this.Current().GetLocation(),
            `Expected global module definition got unexpected token '${this.Current().GetText()}' instead`
        )
        return null
    }

    private ParseImportStatement(): ModuleMemberSyntax
    {
        let keyword = this.MatchAndAdvanceToken(SyntaxKind.ImportKeyword)
        let moduleName = this.MatchAndAdvanceToken(SyntaxKind.IdentifierToken)
        return new ImportDeclarationStatementSyntax(this.tree, keyword, moduleName)
    }


    private ParseGlobalVariableDeclaration(externKeyword: SyntaxToken | null): GlobalVariableDeclarationStatementSyntax
    {
        let statement = this.ParseVariableDeclarationStatement(false, true)
        return new GlobalVariableDeclarationStatementSyntax(this.tree, externKeyword, statement)
    }

    private ParseStructDefinition(externKeyword: SyntaxToken | null): StructDeclarationStatementSyntax | StructDefinitionStatementSyntax
    {
        let structKeyword = this.MatchAndAdvanceToken(SyntaxKind.StructKeyword)
        let identifier = this.MatchAndAdvanceToken(SyntaxKind.IdentifierToken)
        let declaration = new StructDeclarationStatementSyntax(this.tree, externKeyword, structKeyword, identifier)
        if (this.Current().kind != SyntaxKind.LeftBraceToken) {
            return declaration
        }

        let leftBrace = this.MatchAndAdvanceToken(SyntaxKind.LeftBraceToken)
        let membersAndSeparators = []
        while (this.Current().kind != SyntaxKind.RightBraceToken && this.Current().kind != SyntaxKind.EndOfFileToken) {
            let member = this.ParseVariableDeclarationStatement(true, false)
            membersAndSeparators.push(member)

            if (this.Current().kind == SyntaxKind.CommaToken as SyntaxKind) {
                let comma = this.AdvanceToken()
                membersAndSeparators.push(comma)
            } else {
                break
            }
        }
        let rightBrace = this.MatchAndAdvanceToken(SyntaxKind.RightBraceToken)
        return new StructDefinitionStatementSyntax(this.tree, declaration, leftBrace, membersAndSeparators, rightBrace)
    }

    private ParseFunctionDefinition(externKeyword: SyntaxToken | null): FunctionDeclarationStatementSyntax | FunctionDefinitionStatementSyntax
    {
        let funKeyword = this.MatchAndAdvanceToken(SyntaxKind.FunKeyword)
        let identifier = this.MatchAndAdvanceToken(SyntaxKind.IdentifierToken)

        let leftParen = this.MatchAndAdvanceToken(SyntaxKind.LeftParenToken)
        let paramsAndSeparators = []
        while (this.Current().kind != SyntaxKind.RightParenToken && this.Current().kind != SyntaxKind.EndOfFileToken) {
            let param = this.ParseVariableDeclarationStatement(true, false)
            paramsAndSeparators.push(param)

            if (this.Current().kind == SyntaxKind.CommaToken) {
                let comma = this.MatchAndAdvanceToken(SyntaxKind.CommaToken)
                paramsAndSeparators.push(comma)
            } else {
                break
            }
        }
        let rightParen = this.MatchAndAdvanceToken(SyntaxKind.RightParenToken)

        let colon: SyntaxToken | null = null
        let type: TypeExpressionSyntax | null = null
        if (this.Current().kind == SyntaxKind.ColonToken) {
            colon = this.MatchAndAdvanceToken(SyntaxKind.ColonToken)
            type = this.ParseTypeExpression()
        }

        let declaration = new FunctionDeclarationStatementSyntax(this.tree, externKeyword, funKeyword, identifier, leftParen, paramsAndSeparators, rightParen, colon, type)
        if (this.Current().kind != SyntaxKind.LeftBraceToken) {
            return declaration
        }

        this.functionLevel += 1
        let body = this.ParseBlockStatement(false)
        this.functionLevel -= 1

        return new FunctionDefinitionStatementSyntax(this.tree, declaration, body)
    }

    private ParseEnumDefinition(externKeyword: SyntaxToken | null): EnumDeclarationStatementSyntax | EnumDefinitionStatementSyntax
    {
        let enumKeyword = this.MatchAndAdvanceToken(SyntaxKind.EnumKeyword)
        let identifier = this.MatchAndAdvanceToken(SyntaxKind.IdentifierToken)
        let declaration = new EnumDeclarationStatementSyntax(this.tree, externKeyword, enumKeyword, identifier)
        if (this.Current().kind != SyntaxKind.LeftBraceToken) {
            return declaration
        }

        let leftBrace = this.MatchAndAdvanceToken(SyntaxKind.LeftBraceToken)
        let values = []
        while (this.Current().kind != SyntaxKind.RightBraceToken && this.Current().kind != SyntaxKind.EndOfFileToken) {
            let valueClause = this.ParseEnumValueClause()
            values.push(valueClause)
        }
        let rightBrace = this.MatchAndAdvanceToken(SyntaxKind.RightBraceToken)
        return new EnumDefinitionStatementSyntax(this.tree, declaration, leftBrace, values, rightBrace)
    }

    private ParseEnumValueClause(): EnumValueClauseSyntax
    {
        let identifier = this.MatchAndAdvanceToken(SyntaxKind.IdentifierToken)

        let equals: SyntaxToken | null = null
        let integerLiteral: SyntaxToken | null = null
        if (this.Current().kind == SyntaxKind.EqualsToken) {
            equals = this.MatchAndAdvanceToken(SyntaxKind.EqualsToken)
            integerLiteral = this.MatchAndAdvanceToken(SyntaxKind.NumberLiteralToken)
        }

        let comma: SyntaxToken | null = null
        if (this.Current().kind == SyntaxKind.CommaToken) {
            comma = this.MatchAndAdvanceToken(SyntaxKind.CommaToken)
        }

        return new EnumValueClauseSyntax(this.tree, identifier, equals, integerLiteral, comma)
    }


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // Statements

    private ParseStatement(): StatementSyntax
    {
        if (this.functionLevel == 0)
            throw new Error("Statements cannot be outside functions")

        switch (this.Current().kind) {
            case SyntaxKind.LeftBraceToken:
                return this.ParseBlockStatement(false)
            case SyntaxKind.IfKeyword:
                return this.ParseIfStatement()
            case SyntaxKind.DoKeyword:
                return this.ParseDoWhileStatement()
            case SyntaxKind.WhileKeyword:
                return this.ParseWhileStatement()
            case SyntaxKind.ForKeyword:
                return this.ParseForStatement()
            case SyntaxKind.ReturnKeyword:
                return this.ParseReturnStatement()
            case SyntaxKind.BreakKeyword:
                return this.ParseBreakStatement()
            case SyntaxKind.ContinueKeyword:
                return this.ParseContinueStatement()
            case SyntaxKind.SwitchKeyword:
                return this.ParseSwitchStatement()
            case SyntaxKind.LetKeyword:
            case SyntaxKind.LetLocalPersistKeyword:
                return this.ParseVariableDeclarationStatement(false, true)
            default:
                return this.ParseExpressionStatement()
        }
    }

    private ParseSwitchStatement(): SwitchStatementSyntax
    {
        let switchKeyword = this.MatchAndAdvanceToken(SyntaxKind.SwitchKeyword)
        let switchExpression = this.ParseExpression()

        let leftBrace = this.MatchAndAdvanceToken(SyntaxKind.LeftBraceToken)

        this.switchCaseLevel += 1
        let caseStatements = []
        while (this.Current().kind != SyntaxKind.EndOfFileToken && this.Current().kind != SyntaxKind.RightBraceToken) {
            let caseStatement = this.ParseCaseStatement()
            caseStatements.push(caseStatement)
        }
        this.switchCaseLevel -= 1

        let rightBrace = this.MatchAndAdvanceToken(SyntaxKind.RightBraceToken)

        return new SwitchStatementSyntax(this.tree, switchKeyword, switchExpression, leftBrace, caseStatements, rightBrace)
    }

    private ParseCaseStatement(): CaseStatementSyntax 
    {
        let caseOrDefaultKeyword = this.Current().kind == SyntaxKind.CaseKeyword
            ? this.MatchAndAdvanceToken(SyntaxKind.CaseKeyword)
            : this.MatchAndAdvanceToken(SyntaxKind.DefaultKeyword)

        if (this.switchCaseLevel == 0) {
            this.tree.diagnostics.ReportError(
                caseOrDefaultKeyword.GetLocation(),
                `Unexpected '${caseOrDefaultKeyword.GetText}' label keyword outside of switch statement`
            )
        }

        let caseExpression: ExpressionSyntax | null = null
        if (caseOrDefaultKeyword.kind == SyntaxKind.CaseKeyword) {
            caseExpression = this.ParseExpression()
            switch (caseExpression.kind) {
                case SyntaxKind.NumberLiteral:
                case SyntaxKind.StringLiteral:
                case SyntaxKind.MemberAccessExpression:
                    break
                default:
                    this.tree.diagnostics.ReportError(
                        caseExpression.GetLocation(),
                        `Expected literal token in case label but got '${caseExpression.GetLocation().GetText()}' instead`,
                    )
            }
        }

        let colon = this.MatchAndAdvanceToken(SyntaxKind.ColonToken)
        let body: BlockStatementSyntax | null = this.ParseBlockStatement(true)
        if (body.leftBrace == null && body.righBrace == null && body.statements.length == 0)
            body = null

        return new CaseStatementSyntax(this.tree, caseOrDefaultKeyword, caseExpression, colon, body)
    }

    private ParseBlockStatement(inSwitchCaseStatement: boolean): BlockStatementSyntax
    {
        let leftBrace: SyntaxToken | null = null
        if (this.Current().kind == SyntaxKind.LeftBraceToken || !inSwitchCaseStatement) {
            leftBrace = this.MatchAndAdvanceToken(SyntaxKind.LeftBraceToken)
        } else {
            // We allow omitting the braces in switch statements
            leftBrace = null
        }

        let statements = []
        while (this.Current().kind != SyntaxKind.RightBraceToken) {
            let startToken = this.Current()
            if (inSwitchCaseStatement && this.Current().kind == SyntaxKind.CaseKeyword)
                break
            if (inSwitchCaseStatement && this.Current().kind == SyntaxKind.DefaultKeyword)
                break
            let statement = this.ParseStatement()
            statements.push(statement)

            // If ParseStatement() did not consume any tokens, we need to skip the current token 
            // and continue in order to avoid an infinite loop. We don't need to report an error, 
            // because we already tried to parse an expression statement and reported one.
            if (this.Current() == startToken)
                this.AdvanceToken()
        }

        let rightBrace: SyntaxToken | null = null
        if (leftBrace != null) {
            // If we start with a brace we also must end with a brace
            rightBrace = this.MatchAndAdvanceToken(SyntaxKind.RightBraceToken)
        }

        return new BlockStatementSyntax(this.tree, leftBrace, statements, rightBrace)
    }

    private ParseBreakStatement(): StatementSyntax 
    {
        let breakKeyword = this.MatchAndAdvanceToken(SyntaxKind.BreakKeyword)
        return new BreakStatementSyntax(this.tree, breakKeyword)
    }

    private ParseContinueStatement(): StatementSyntax 
    {
        let continueKeyword = this.MatchAndAdvanceToken(SyntaxKind.ContinueKeyword)
        return new ContinueStatementSyntax(this.tree, continueKeyword)
    }

    private ParseReturnStatement(): StatementSyntax 
    {
        let returnKeyword = this.MatchAndAdvanceToken(SyntaxKind.ReturnKeyword)
        let returnExpression: ExpressionSyntax | null = null
        if (this.Current().kind != SyntaxKind.EndOfFileToken) {
            // NOTE: The return keyword and an expression coming after it both belong to the return 
            // statement if they are both on the same line
            let keywordLine = this.source.GetLineColumnIndex(returnKeyword.location.start).line
            let currentLine = this.source.GetLineColumnIndex(this.Current().location.start).line
            let sameLine = keywordLine == currentLine
            returnExpression = sameLine ? this.ParseExpression() : null
        }
        return new ReturnStatementSyntax(this.tree, returnKeyword, returnExpression)
    }

    private ParseForStatement(): ForStatementSyntax
    {
        let keyword = this.MatchAndAdvanceToken(SyntaxKind.ForKeyword)
        let iteratorIdent = this.MatchAndAdvanceToken(SyntaxKind.IdentifierToken)
        let inKeyword = this.MatchAndAdvanceToken(SyntaxKind.InKeyword)
        let lowerBound = this.ParseExpression()
        let dotdot = this.MatchAndAdvanceToken(SyntaxKind.DotDotToken)
        let equals: SyntaxToken | null = null
        if (this.Current().kind == SyntaxKind.EqualsToken)
            equals = this.MatchAndAdvanceToken(SyntaxKind.EqualsToken)
        let upperBound = this.ParseExpression()
        this.loopLevel += 1
        let body = this.ParseStatement()
        this.loopLevel -= 1
        return new ForStatementSyntax(this.tree, keyword, iteratorIdent, inKeyword, lowerBound, dotdot, equals, upperBound, body)
    }

    private ParseDoWhileStatement(): DoWhileStatementSyntax
    {
        let doKeyword = this.MatchAndAdvanceToken(SyntaxKind.DoKeyword)
        let body = this.ParseStatement()
        let whileKeyword = this.MatchAndAdvanceToken(SyntaxKind.WhileKeyword)
        this.loopLevel += 1
        let condition = this.ParseExpression()
        this.loopLevel -= 1
        return new DoWhileStatementSyntax(this.tree, doKeyword, body, whileKeyword, condition)
    }

    private ParseWhileStatement(): WhileStatementSyntax
    {
        let keyword = this.MatchAndAdvanceToken(SyntaxKind.WhileKeyword)
        let condition = this.ParseExpression()
        this.loopLevel += 1
        let body = this.ParseStatement()
        this.loopLevel -= 1
        return new WhileStatementSyntax(this.tree, keyword, condition, body)
    }

    private ParseIfStatement(): IfStatementSyntax
    {
        let ifKeyword = this.MatchAndAdvanceToken(SyntaxKind.IfKeyword)
        let condition = this.ParseExpression()
        let thenStatement = this.ParseStatement()
        if (this.Current().kind == SyntaxKind.ElseKeyword) {
            let elseKeyword = this.AdvanceToken()
            let elseStatement = this.ParseStatement()
            return new IfStatementSyntax(this.tree, ifKeyword, condition, thenStatement, elseKeyword, elseStatement)
        }
        else {
            return new IfStatementSyntax(this.tree, ifKeyword, condition, thenStatement, null, null)
        }
    }

    private ParseVariableDeclarationStatement(skipLet: boolean, allowInitilizer: boolean): VariableDeclarationStatementSyntax
    {
        let letKeyword: SyntaxToken | null = null
        if (!skipLet) {
            if (this.Current().kind == SyntaxKind.LetLocalPersistKeyword)
                letKeyword = this.MatchAndAdvanceToken(SyntaxKind.LetLocalPersistKeyword)
            else
                letKeyword = this.MatchAndAdvanceToken(SyntaxKind.LetKeyword)
        }

        let identifier = this.MatchAndAdvanceToken(SyntaxKind.IdentifierToken)
        let colon = this.MatchAndAdvanceToken(SyntaxKind.ColonToken)
        let type = this.ParseTypeExpression()

        if (allowInitilizer && this.Current().kind == SyntaxKind.EqualsToken) {
            let equals = this.MatchAndAdvanceToken(SyntaxKind.EqualsToken)
            let initializer = this.ParseExpression()
            return new VariableDeclarationStatementSyntax(this.tree, letKeyword, identifier, colon, type, equals, initializer)
        }
        else {
            return new VariableDeclarationStatementSyntax(this.tree, letKeyword, identifier, colon, type, null, null)
        }
    }

    private ParseExpressionStatement(): ExpressionStatementSyntax
    {
        let expression = this.ParseExpression()
        return new ExpressionStatementSyntax(this.tree, expression)
    }


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // Expressions

    private ParseExpression(): ExpressionSyntax
    {
        return this.ParseAssignmentExpression()
    }

    private ParseAssignmentExpression(): ExpressionSyntax
    {
        let left = this.ParseTernaryConditionalExpression()
        switch (this.Current().kind) {
            case SyntaxKind.EqualsToken:
            case SyntaxKind.PlusEqualsToken:
            case SyntaxKind.MinusEqualsToken:
            case SyntaxKind.StarEqualsToken:
            case SyntaxKind.SlashEqualsToken:
            case SyntaxKind.PercentEqualsToken:
            case SyntaxKind.HatEqualsToken:
            case SyntaxKind.AmpersandEqualsToken:
            case SyntaxKind.PipeEqualsToken:
            case SyntaxKind.LessLessEqualsToken:
            case SyntaxKind.GreaterGreaterEqualsToken:
                {
                    let operator = this.AdvanceToken()
                    let right = this.ParseAssignmentExpression()
                    return new BinaryExpressionSyntax(this.tree, left, operator, right)
                }
        }
        return left
    }

    private ParseTernaryConditionalExpression(): ExpressionSyntax
    {
        let left = this.ParseBinaryExpression(0)
        if (this.Current().kind == SyntaxKind.QuestionmarkToken) {
            let condition = left
            let questionmark = this.MatchAndAdvanceToken(SyntaxKind.QuestionmarkToken)
            let thenExpression = this.ParseTernaryConditionalExpression()
            let colon = this.MatchAndAdvanceToken(SyntaxKind.ColonToken)
            let elseExpression = this.ParseTernaryConditionalExpression()
            return new TernaryConditionalExpressionSyntax(this.tree, condition, questionmark, thenExpression, colon, elseExpression)
        }
        return left
    }

    private ParseBinaryExpression(parentPrecedence = 0): ExpressionSyntax
    {
        let left = this.ParseUnaryExpression(parentPrecedence)
        while (true) {
            let precedence = SyntaxFacts.GetBinaryOperatorPrecedence(this.Current().kind)
            if (precedence == 0
                || precedence < parentPrecedence
                || precedence == parentPrecedence && !SyntaxFacts.IsBinaryOperatorRightAssociative(this.Current().kind)) {
                break
            }
            let operator = this.AdvanceToken()
            let right = this.ParseBinaryExpression(precedence)
            left = new BinaryExpressionSyntax(this.tree, left, operator, right)
        }
        return left
    }

    private ParseUnaryExpression(parentPrecedence = 0): ExpressionSyntax
    {
        let unaryOperatorPrecedence = SyntaxFacts.GetUnaryOperatorPrecedence(this.Current().kind)
        if ((unaryOperatorPrecedence != 0) && (unaryOperatorPrecedence >= parentPrecedence)) {
            let operator = this.AdvanceToken()
            let operand = this.ParseBinaryExpression(unaryOperatorPrecedence)
            return new UnaryExpressionSyntax(this.tree, operator, operand)
        } else {
            return this.ParsePostFixExpression()
        }
    }

    private ParsePostFixExpression(): ExpressionSyntax
    {
        let left = this.ParsePrimaryExpression()

        let foundPostfix = false
        do {
            foundPostfix = false
            if (this.Current().kind == SyntaxKind.AsKeyword) {
                foundPostfix = true
                left = this.ParseTypeCastExpression(left)
            }
            if (this.Current().kind == SyntaxKind.LeftParenToken) {
                foundPostfix = true
                left = this.ParseFunctionCallExpression(left)
            }
            if (this.Current().kind == SyntaxKind.DotToken) {
                foundPostfix = true
                left = this.ParseMemberAccess(left)
            }
            if (this.Current().kind == SyntaxKind.LeftBracketToken) {
                foundPostfix = true
                left = this.ParseArrayIndexingExpression(left)
            }
        } while (foundPostfix)

        return left
    }


    private ParseTypeCastExpression(left: ExpressionSyntax): ExpressionSyntax
    {
        let expression = left
        let asKeyword = this.MatchAndAdvanceToken(SyntaxKind.AsKeyword)
        let targetType = this.ParseTypeExpression()
        return new TypeCastExpressionSyntax(this.tree, expression, asKeyword, targetType)
    }


    private ParseFunctionCallExpression(left: ExpressionSyntax)
    {
        let func = left
        let leftParen = this.MatchAndAdvanceToken(SyntaxKind.LeftParenToken)

        let argumentsWithSeparators = []
        while (this.Current().kind != SyntaxKind.RightParenToken) {
            let argument = this.ParseExpression()
            argumentsWithSeparators.push(argument)

            if (this.Current().kind == SyntaxKind.CommaToken) {
                let comma = this.MatchAndAdvanceToken(SyntaxKind.CommaToken)
                argumentsWithSeparators.push(comma)
            } else {
                break
            }
        }
        let rightParen = this.MatchAndAdvanceToken(SyntaxKind.RightParenToken)
        return new FuncCallExpressionSyntax(this.tree, func, leftParen, argumentsWithSeparators, rightParen)
    }

    private ParseMemberAccess(left: ExpressionSyntax): ExpressionSyntax
    {
        let container = left
        let dot = this.MatchAndAdvanceToken(SyntaxKind.DotToken)
        let memberIdentifier = this.MatchAndAdvanceToken(SyntaxKind.IdentifierToken)
        return new MemberAccessExpressionSyntax(this.tree, container, dot, memberIdentifier)
    }

    private ParseArrayIndexingExpression(left: ExpressionSyntax): ExpressionSyntax
    {
        let array = left
        let leftBracket = this.MatchAndAdvanceToken(SyntaxKind.LeftBracketToken)
        let indexExpression = this.ParseExpression()
        let rightBracket = this.MatchAndAdvanceToken(SyntaxKind.RightBracketToken)
        return new ArrayIndexExpressionSyntax(this.tree, array, leftBracket, indexExpression, rightBracket)
    }


    private ParsePrimaryExpression(): ExpressionSyntax
    {
        switch (this.Current().kind) {

            case SyntaxKind.LeftParenToken:
                return this.ParseParenthesizedExpression()
            case SyntaxKind.IdentifierToken:
                return this.ParseNameExpression()
            case SyntaxKind.LeftBracketToken:
                return this.ParseArrayLiteral()
            case SyntaxKind.NullKeyword:
                return this.ParseNullLiteral()
            case SyntaxKind.FalseKeyword:
            case SyntaxKind.TrueKeyword:
                return this.ParseBoolLiteral()
            case SyntaxKind.NumberLiteralToken:
                return this.ParseNumberLiteral()
            case SyntaxKind.StringLiteralToken:
                return this.ParseStringLiteral()
            default:
                return this.ParseNameExpression()
        }
    }

    private ParseParenthesizedExpression(): ParenthesizedExpressionSyntax
    {
        let leftParen = this.MatchAndAdvanceToken(SyntaxKind.LeftParenToken)
        let expression = this.ParseExpression()
        let rightParen = this.MatchAndAdvanceToken(SyntaxKind.RightParenToken)
        return new ParenthesizedExpressionSyntax(this.tree, leftParen, expression, rightParen)
    }

    private ParseNameExpression(): NameExpressionSyntax
    {
        let identifierToken = this.MatchAndAdvanceToken(SyntaxKind.IdentifierToken)
        return new NameExpressionSyntax(this.tree, identifierToken)
    }

    private ParseArrayLiteral(): ArrayLiteralSyntax
    {
        let leftBracket = this.MatchAndAdvanceToken(SyntaxKind.LeftBracketToken)
        let elemsWithSeparators = []
        while (this.Current().kind != SyntaxKind.RightBracketToken) {
            let expression = this.ParseExpression()
            elemsWithSeparators.push(expression)

            if (this.Current().kind == SyntaxKind.CommaToken) {
                let comma = this.MatchAndAdvanceToken(SyntaxKind.CommaToken)
                elemsWithSeparators.push(comma)
            } else {
                break
            }
        }
        let rightBracket = this.MatchAndAdvanceToken(SyntaxKind.RightBracketToken)
        return new ArrayLiteralSyntax(this.tree, leftBracket, elemsWithSeparators, rightBracket)
    }

    private ParseNullLiteral(): NullLiteralSyntax
    {
        let nullLiteral = this.MatchAndAdvanceToken(SyntaxKind.NullKeyword)
        return new NullLiteralSyntax(this.tree, nullLiteral)
    }

    private ParseBoolLiteral(): BoolLiteralSyntax
    {
        let boolLiteral
        if (this.Current().kind == SyntaxKind.TrueKeyword)
            boolLiteral = this.MatchAndAdvanceToken(SyntaxKind.TrueKeyword)
        else
            boolLiteral = this.MatchAndAdvanceToken(SyntaxKind.FalseKeyword)

        return new BoolLiteralSyntax(this.tree, boolLiteral)
    }

    private ParseStringLiteral(): StringLiteralSyntax
    {
        let stringLiteral = this.MatchAndAdvanceToken(SyntaxKind.StringLiteralToken)
        return new StringLiteralSyntax(this.tree, stringLiteral)
    }

    private ParseNumberLiteral(): NumberLiteralSyntax
    {
        let numberLiteral = this.MatchAndAdvanceToken(SyntaxKind.NumberLiteralToken)
        return new NumberLiteralSyntax(this.tree, numberLiteral)
    }

    private ParseTypeExpression(): TypeExpressionSyntax
    {
        let typeIdentifier
        switch (this.Current().kind) {
            case SyntaxKind.AnyKeyword:
            case SyntaxKind.BoolKeyword:
            case SyntaxKind.NumberKeyword:
            case SyntaxKind.StringKeyword:
            case SyntaxKind.IdentifierToken:
                typeIdentifier = this.AdvanceToken()
                break
            default:
                this.tree.diagnostics.ReportError(
                    this.Current().GetLocation(),
                    `Expected primitive type or identifier token - got '${this.Current().GetLocation().GetText()}' instead`
                )
                typeIdentifier = this.MatchAndAdvanceToken(SyntaxKind.AnyKeyword)
                break
        }

        let result: TypeExpressionSyntax = new BaseTypeExpressionSyntax(this.tree, typeIdentifier)
        while (this.Current().kind == SyntaxKind.QuestionmarkToken
            || this.Current().kind == SyntaxKind.LeftBracketToken) {
            if (this.Current().kind == SyntaxKind.QuestionmarkToken) {
                let questionMark = this.MatchAndAdvanceToken(SyntaxKind.QuestionmarkToken)
                result = new NullableTypeExpressionSyntax(this.tree, result, questionMark)
            }
            if (this.Current().kind == SyntaxKind.LeftBracketToken) {
                let leftBracket = this.MatchAndAdvanceToken(SyntaxKind.LeftBracketToken)
                let righBracket = this.MatchAndAdvanceToken(SyntaxKind.RightBracketToken)
                result = new ArrayTypeExpressionSyntax(this.tree, result, leftBracket, righBracket)
            }
        }
        return result
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // Helpers

    private Current(): SyntaxToken { return this.Peek(0) }
    private LookAhead(): SyntaxToken { return this.Peek(1) }

    private Peek(offset: number): SyntaxToken
    {
        let index = this.position + offset
        if (index >= this.tokens.length)
            return this.tokens[this.tokens.length - 1]
        return this.tokens[index]
    }

    private AdvanceToken(): SyntaxToken
    {
        let token = this.Current()
        if (this.position < this.tokens.length)
            this.position++

        this.debugToParse = this.tokens.slice(0, this.position)
        this.debugAlreadyParsed = this.tokens.slice(this.position)

        return token
    }

    private MatchAndAdvanceToken(kind: SyntaxKind): SyntaxToken
    {
        if (this.Current().kind == kind)
            return this.AdvanceToken()

        this.diagnostics.ReportError(
            this.Current().GetLocation(),
            `Expected token '${kind}' but got token '${this.Current().kind}'`
        )

        // Return dummy token to avoid having holes in the syntax tree
        let location = SourceLocation.FromStartLength(this.source, this.Current().GetLocation().start, 0)
        return new SyntaxToken(kind, this.tree, location, null, [], [])
    }

}