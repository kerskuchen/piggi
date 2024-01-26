// deno-lint-ignore-file prefer-const

import { BoundBlockStatement, BoundExpression } from "./boundtree.ts"
import { SyntaxNode } from "./syntax.ts"
import { Type } from "./types.ts"

export enum SymbolKind
{
    Function = "Function",
    TemplateFunction = "TemplateFunction",
    Struct = "Struct",
    Enum = "Enum",
    Variable = "Variable",
    GlobalVariable = "GlobalVariable",
    LocalPersistVariable = "LocalPersistVariable",

    Parameter = "Parameter", // Valid in functions only
    EnumValue = "EnumValue", // For enums only

    // Valid in impl blocks only
    MemberField = "MemberField",
    MemberFunction = "MemberFunction",
    MemberMethod = "MemberMethod",
    MemberVariable = "MemberVariable",
};

export class Symbol
{
    constructor(
        public name: string,
        public kind: SymbolKind,
        public isExternal: boolean,
        public type: Type,
        public table: SymbolTable,
    ) { }

    public syntax: SyntaxNode | null = null
    public parent: Symbol | null = null // for members to determine their container/namespace symbols
    public membersSymbolTable: SymbolTable | null = null // Holds function parameters, enum/union/struct fields/members
    public templateTypeParamNames: string[] | null = null // For template functions and structs
    public functionBody: BoundBlockStatement | null = null // For functions
    public initializer: BoundExpression | null = null // For variables
    public enumValue = 0  // For enum values
}

export class SymbolTable
{
    public symbols: Map<string, Symbol> = new Map()
    public childTables: SymbolTable[] = []

    constructor(
        public parent: SymbolTable | null
    )
    {
        if (parent != null) {
            parent.childTables.push(this)
        }
    }


    GetSymbolFromLocalScope(name: string): Symbol | null
    {
        let result = this.symbols.get(name)
        if (result == undefined)
            return null
        return result
    }

    GetSymbol(name: string): Symbol | null
    {
        let result = this.GetSymbolFromLocalScope(name)
        if (result != null)
            return result
        if (this.parent != null)
            return this.parent.GetSymbol(name)
        return null
    }

    AddSymbol(
        name: string,
        kind: SymbolKind,
        isExtern: boolean,
        type: Type,
    ): Symbol | null
    {
        let existingLocal = this.GetSymbolFromLocalScope(name)
        if (existingLocal != null) {
            throw new Error("Cannot overwrite exiting symbol in the same scope")
        }

        let newSymbol = new Symbol(name, kind, isExtern, type, this)
        this.symbols.set(name, newSymbol)
        return newSymbol
    }
}


