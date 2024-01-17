// deno-lint-ignore-file prefer-const

import { Type } from "./types.ts"

export enum SymbolKind
{
    Invalid = "Invalid", // Only used if the binder encounters an error
    Function = "Function",
    Struct = "Struct",
    Enum = "Enum",
    Variable = "Variable",
    Parameter = "Parameter",  // Valid in functions only
    Member = "Member",     // Valid in struct/union members only
    Enumvalue = "Enumvalue",  // For enums only
};

export enum SymbolScopeKind
{
    Extern = "Extern",
    Global = "Global",
    Local = "Local",
    LocalPersist = "LocalPersist",
};

export class Symbol
{
    constructor(
        public name: string,
        public kind: SymbolKind,
        public scopeKind: SymbolScopeKind,
        public type: Type,
    ) { }

    // For structs/unions/functions/enums
    public membersSymbolTable: SymbolTable | null = null// Holds function parameters, enum/union/struct members
    public enumValue = 0  // For enum values
}

export class SymbolTable
{
    public symbols: Map<string, Symbol> = new Map()

    constructor(
        public parent: SymbolTable | null
    ) { }


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

    AddSymbol(name: string, kind: SymbolKind, scopeKind: SymbolScopeKind, type: Type): Symbol | null
    {
        let existingLocal = this.GetSymbolFromLocalScope(name)
        if (existingLocal != null) {
            throw new Error("Cannot overwrite exiting symbol in the same scope")
        }

        let newSymbol = new Symbol(name, kind, scopeKind, type)
        this.symbols.set(name, newSymbol)
        return newSymbol
    }
}


