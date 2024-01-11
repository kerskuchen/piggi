// deno-lint-ignore-file prefer-const

import { Source } from "./common.ts"
import { Scanner } from "./scanner.ts"
import { SyntaxKind, SyntaxToken, SyntaxTree, SyntaxTrivia } from "./syntax.ts"

function LoadSource(moduleName: string): Source
{
    // TODO: Actually do some searching first if the filepath cannot be found
    // For example we may want to search "math.pig" also in subfolders called "math"
    let modulePath = moduleName
    if (!modulePath.endsWith(".pig")) {
        modulePath = modulePath + ".pig"
    }
    let content = Deno.readTextFileSync(modulePath)
    return new Source(moduleName, modulePath, content)
}

function Main()
{
    let source = LoadSource("test")
    console.log(source)

    let tree = new SyntaxTree(source)
    let scanner = new Scanner(tree)

    let tokens: SyntaxToken[] = []
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

                    let trivia = new SyntaxTrivia(SyntaxKind.SkippedTextTrivia, tree, badToken.location)
                    leadingTrivia.splice(index++, 0, trivia)

                    for (let tt of badToken.trailingTrivia)
                        leadingTrivia.splice(index++, 0, tt)
                }

                badTokens.length = 0
                token = new SyntaxToken(token.kind, token.tree, token.location, token.text, leadingTrivia, token.trailingTrivia)
            }

            tokens.push(token)
        }
    } while (token.kind != SyntaxKind.EndOfFileToken)

    // Some verifications
    let tokenTextLengthSum = 0
    for (let token of tokens) {
        tokenTextLengthSum += token.GetLocationIncludingTrivia().length
    }
    if (tokenTextLengthSum != tree.source.content.length) {
        throw new Error(`All tokens text length = ${tokenTextLengthSum} != ${tree.source.content.length} = source text length`)
    }


    for (let token of tokens)
        console.log(token.PrettyPrint())

    if (tree.diagnostics.hasErrors) {
        tree.diagnostics.Print()
    }
}

Main()