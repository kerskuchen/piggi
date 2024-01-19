// deno-lint-ignore-file prefer-const

import { Binder } from "./binder.ts"
import { Source } from "./common.ts"
import { Emitter } from "./emitter.ts"
import { Parser } from "./parser.ts"
import { SymbolTable } from "./symbols.ts"
import { ImportDeclarationSyntax, SyntaxKind, SyntaxTree } from "./syntax.ts"

function LoadSource(moduleName: string): Source
{
    // TODO: Actually do some searching first if the filepath cannot be found
    // For example we may want to search "math.pig" also in subfolders called "math"
    // TODO: Also we need error reporting and graceful handling of missing files
    let modulePath = moduleName
    if (!modulePath.endsWith(".pig")) {
        modulePath = modulePath + ".pig"
    }
    let content = Deno.readTextFileSync(modulePath)
    return new Source(moduleName, modulePath, content)
}

function CollectSyntaxTreesRecursive(result: SyntaxTree[], visited: string[], modulename: string)
{
    if (visited.includes(modulename))
        return
    else
        visited.push(modulename)

    let source = LoadSource(modulename)
    let parser = new Parser(source)
    let tree = parser.Parse()

    for (let member of tree.root.members) {
        if (member.kind == SyntaxKind.ImportDeclaration) {
            let importStatement = member as ImportDeclarationSyntax
            if (importStatement.modulenameIdent.GetText() == null) {
                // We already reported a parsing error
                continue
            }
            let importmodulename = importStatement.modulenameIdent.GetText()
            CollectSyntaxTreesRecursive(result, visited, importmodulename)
        }
    }
    result.push(tree)
}


function CollectSyntaxTrees(rootModuleName: string): SyntaxTree[]
{
    let result: SyntaxTree[] = []
    let visited: string[] = []
    CollectSyntaxTreesRecursive(result, visited, rootModuleName)
    return result
}

function Main()
{
    console.log("COMPLILATION STARTED")

    let rootModuleName = "test"
    let trees = CollectSyntaxTrees(rootModuleName)
    for (let tree of trees) {
        Deno.writeTextFileSync("bin/" + tree.source.modulename + "_syntaxdump.txt", tree.root.DumpTree())
    }

    let symbolTable = new SymbolTable(null)
    let binder = new Binder(symbolTable)
    let compilation = binder.BindCompilationUnit(trees)

    Deno.writeTextFileSync("bin/" + rootModuleName + "_boundtreedump.txt", compilation.DumpTree())

    if (binder.diagnostics.hasErrors) {
        binder.diagnostics.Print()
        return
    }

    // let localPersistTransformer = new LocalPersistTransformer()
    // compilation = localPersistTransformer.RewriteCompilationUnit(compilation)

    let emitter = new Emitter()
    let output = emitter.EmitCompilationUnit(compilation)

    Deno.writeTextFileSync("bin/" + rootModuleName + ".js", output)

    console.log("COMPLILATION FINISHED")
}

Main()