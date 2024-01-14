// deno-lint-ignore-file prefer-const

export enum BaseTypeKind
{
    Void = "Void",
    Any = "Any",
    Null = "Null",
    Bool = "Bool",
    Number = "Number",
    String = "String",

    Struct = "Struct",
    Enum = "Enum",
};

function IsBaseTypeKindUserDefined(baseKind: BaseTypeKind): boolean
{
    switch (baseKind) {
        case BaseTypeKind.Struct:
        case BaseTypeKind.Enum:
            return true
        case BaseTypeKind.Void:
        case BaseTypeKind.Any:
        case BaseTypeKind.Null:
        case BaseTypeKind.Bool:
        case BaseTypeKind.Number:
        case BaseTypeKind.String:
            return false
        default:
            throw new Error(`Unexpected type kind ${baseKind}`)
    }
}

function IsBaseTypeKindPrimitive(baseKind: BaseTypeKind): boolean
{
    switch (baseKind) {
        case BaseTypeKind.Bool:
        case BaseTypeKind.Number:
        case BaseTypeKind.String:
            return true
        case BaseTypeKind.Void:
        case BaseTypeKind.Any:
        case BaseTypeKind.Null:
        case BaseTypeKind.Struct:
        case BaseTypeKind.Enum:
            return false
        default:
            throw new Error(`Unexpected type kind ${baseKind}`)
    }
}

export enum TypeConversionResult
{
    NonConvertible = "NonConvertible",
    Identical = "Identical",
    ImplictlyConvertible = "ImplictlyConvertible",
    ExplicitlyConvertible = "ExplicitlyConvertible",
};

export abstract class Type
{
    static builtinTypes: Map<BaseTypeKind, Type> = new Map()

    protected constructor()
    {
    }

    static Init()
    {
        if (Type.builtinTypes.size == 0) {
            Type.builtinTypes.set(BaseTypeKind.Void, new BaseType(BaseTypeKind.Void, null))
            Type.builtinTypes.set(BaseTypeKind.Any, new BaseType(BaseTypeKind.Any, null))
            Type.builtinTypes.set(BaseTypeKind.Null, new BaseType(BaseTypeKind.Null, null))
            Type.builtinTypes.set(BaseTypeKind.Bool, new BaseType(BaseTypeKind.Bool, null))
            Type.builtinTypes.set(BaseTypeKind.Number, new BaseType(BaseTypeKind.Number, null))
            Type.builtinTypes.set(BaseTypeKind.String, new BaseType(BaseTypeKind.String, null))
        }
    }

    static get Void() { return Type.builtinTypes.get(BaseTypeKind.Void)! }
    static get Any() { return Type.builtinTypes.get(BaseTypeKind.Any)! }
    static get Null() { return Type.builtinTypes.get(BaseTypeKind.Null)! }
    static get Bool() { return Type.builtinTypes.get(BaseTypeKind.Bool)! }
    static get Number() { return Type.builtinTypes.get(BaseTypeKind.Number)! }
    static get String() { return Type.builtinTypes.get(BaseTypeKind.String)! }

    static Identical(a: Type, b: Type): boolean
    {
        if (a instanceof BaseType && b instanceof BaseType) {
            return a.baseKind == b.baseKind
                && a.name == b.name
        } else if (a instanceof ArrayType && b instanceof ArrayType) {
            return this.Identical(a.elementType, b.elementType)
        } else if (a instanceof NullableType && b instanceof NullableType) {
            return this.Identical(a.elementType, b.elementType)
        }
        return false
    }

    IsBaseType(): boolean { return this instanceof BaseType }
    IsArray(): boolean { return this instanceof ArrayType }
    IsNullable(): boolean { return this instanceof NullableType }
    IsVoid(): boolean { return this == Type.Void }
    IsAny(): boolean { return this == Type.Any }
    IsNull(): boolean { return this == Type.Null }
    IsBool(): boolean { return this == Type.Bool }
    IsNumber(): boolean { return this == Type.Number }
    IsString(): boolean { return this == Type.String }

    IsPrimitive(): boolean
    {
        if (this instanceof BaseType) {
            return IsBaseTypeKindPrimitive(this.baseKind)
        }
        return false
    }
    IsUserDefined(): boolean
    {
        if (this instanceof BaseType) {
            return IsBaseTypeKindUserDefined(this.baseKind)
        }
        return false
    }
    IsEnum(): boolean
    {
        if (this instanceof BaseType) {
            return this.baseKind == BaseTypeKind.Enum
        }
        return false
    }
    IsStruct(): boolean
    {
        if (this instanceof BaseType) {
            return this.baseKind == BaseTypeKind.Struct
        }
        return false
    }

    PrettyPrint(): string
    {
        if (this instanceof ArrayType) {
            return `${this.elementType.PrettyPrint()}[]`
        } else if (this instanceof BaseType) {
            return this.name
        } else if (this instanceof NullableType) {
            return `${this.elementType.PrettyPrint()}?`
        }
        throw new Error("Unreachable - Unknown type given")
    }

    CanImplicitlyConvertTo(target: Type): boolean
    {
        let conversion = this.ConvertTo(target)
        return conversion == TypeConversionResult.Identical || conversion == TypeConversionResult.ImplictlyConvertible
    }

    CanExplicitlyConvertTo(target: Type): boolean
    {
        let conversion = this.ConvertTo(target)
        return conversion != TypeConversionResult.NonConvertible
    }

    ConvertTo(target: Type): TypeConversionResult
    {
        if (Type.Identical(this, target))
            return TypeConversionResult.Identical

        if (target.IsNullable()) {
            if (this.IsNull())
                return TypeConversionResult.ImplictlyConvertible
            if (Type.Identical(this, target.GetInnerType()))
                return TypeConversionResult.ImplictlyConvertible
        }

        if (target.IsAny())
            return TypeConversionResult.ImplictlyConvertible

        if (this instanceof BaseType && target instanceof BaseType) {
            if (this.IsPrimitive() && target.IsString())
                return TypeConversionResult.ExplicitlyConvertible

            if (this.IsBool() && target.IsNumber())
                return TypeConversionResult.ExplicitlyConvertible

            if (this.IsEnum() && target.IsNumber()) {
                return TypeConversionResult.ExplicitlyConvertible
            }
        }

        return TypeConversionResult.NonConvertible
    }

    GetInnerType(): Type
    {
        if (this instanceof ArrayType) {
            return this.elementType
        } else if (this instanceof NullableType) {
            return this.elementType
        } else {
            throw new Error("Base type has no inner type")
        }
    }
}

export class ArrayType extends Type
{
    constructor(
        public elementType: Type,
    ) { super() }
}

export class NullableType extends Type
{
    constructor(
        public elementType: Type,
    ) { super() }
}

export class BaseType extends Type
{
    public name: string

    constructor(
        public baseKind: BaseTypeKind,
        name: string | null,
    )
    {
        super()

        if (IsBaseTypeKindUserDefined(baseKind)) {
            if (name == null)
                throw new Error(`Invalid user defined type '${baseKind}' with missing name`)
        } else {
            if (name != null)
                throw new Error(`Unexpected user defined name '${name}' in primitive type ${baseKind}`)
        }

        switch (baseKind) {
            case BaseTypeKind.Void:
                this.name = "void"
                break
            case BaseTypeKind.Any:
                this.name = "any"
                break
            case BaseTypeKind.Null:
                this.name = "null"
                break
            case BaseTypeKind.Bool:
                this.name = "bool"
                break
            case BaseTypeKind.Number:
                this.name = "number"
                break
            case BaseTypeKind.String:
                this.name = "string"
                break
            default:
                this.name = name!
        }
    }
};




