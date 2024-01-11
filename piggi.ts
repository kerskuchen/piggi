// deno-lint-ignore-file prefer-const

import { Source } from "./common.ts"

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
}

Main()