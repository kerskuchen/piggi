// deno-lint-ignore-file prefer-const

export enum TypeKind
{
    Basic = "Base",
    Indirection = "Indirection",
    Array = "Array",
}

export enum BasicTypeKind
{
    Void = "Void",
    Bool = "Bool",
    Char = "Char",
    Byte = "Byte",
    Short = "Short",
    Int = "Int",
    Long = "Long",
    CString = "CString",

    Struct = "Struct",
    Union = "Union",
    Enum = "Enum",
};

function IsBaseTypeKindNumber(basicKind: BasicTypeKind): boolean
{
    switch (basicKind) {
        case BasicTypeKind.Bool:
        case BasicTypeKind.Byte:
        case BasicTypeKind.Short:
        case BasicTypeKind.Int:
        case BasicTypeKind.Long:
            return true
    }
    return false
}

function IsBaseTypeKindPrimitive(basicKind: BasicTypeKind): boolean
{
    switch (basicKind) {
        case BasicTypeKind.Void:
        case BasicTypeKind.Bool:
        case BasicTypeKind.Char:
        case BasicTypeKind.Byte:
        case BasicTypeKind.Short:
        case BasicTypeKind.Int:
        case BasicTypeKind.Long:
        case BasicTypeKind.CString:
            return true
        case BasicTypeKind.Struct:
        case BasicTypeKind.Union:
        case BasicTypeKind.Enum:
            return false
        default:
            throw new Error(`Unexpected type kind ${basicKind}`)
    }
}

function BytesCountOfNumberType(basicKind: BasicTypeKind)
{
    switch (basicKind) {
        case BasicTypeKind.Byte:
            return 1
        case BasicTypeKind.Short:
            return 2
        case BasicTypeKind.Int:
            return 4
        case BasicTypeKind.Long:
            return 8
    }
    throw new Error("Not a number type")
}

export enum TypeConversionResult
{
    NonConvertible,
    Identical,
    ImplictlyConvertible,
    ExplicitlyConvertible,
};

export abstract class Type
{
    protected constructor(
        public kind: TypeKind
    ) { }

    static FromPrimitive(basicKind: BasicTypeKind): Type
    {
        if (!IsBaseTypeKindPrimitive(basicKind))
            throw new Error(`Unexpected non-primitive type ${basicKind}`)
        return new BasicType(basicKind, null)
    }

    static FromPrimitivePointer(basicKind: BasicTypeKind, indirectionLevel: number): Type
    {
        if (!IsBaseTypeKindPrimitive(basicKind))
            throw new Error(`Unexpected non-primitive type ${basicKind}`)
        return new IndirectionType(new BasicType(basicKind, null), indirectionLevel)
    }


    static TypesIdentical(a: Type, b: Type): boolean
    {
        if (a instanceof BasicType && b instanceof BasicType) {
            return a.basicKind == b.basicKind
                && a.name == b.name
        } else if (a instanceof ArrayType && b instanceof ArrayType) {
            return a.elementCount == b.elementCount && this.TypesIdentical(a.elementType, b.elementType)
        } else if (a instanceof IndirectionType && b instanceof IndirectionType) {
            return a.indirectionLevel == b.indirectionLevel && this.TypesIdentical(a.elementType, b.elementType)
        }
        return false
    }

    GetIndirectionLevel(): number
    {
        if (this instanceof ArrayType) {
            return 1
        } else if (this instanceof BasicType) {
            if (this.basicKind == BasicTypeKind.CString)
                return 1
            else
                return 0
        } else if (this instanceof IndirectionType) {
            return this.indirectionLevel
        }
        throw new Error("unreachable")
    }

    IsVoidPointer(): boolean
    {
        if (this instanceof IndirectionType) {
            if (this.indirectionLevel != 0)
                return false
            if (this.elementType instanceof BasicType)
                return this.elementType.basicKind == BasicTypeKind.Void
        }
        return false
    }

    IsVoidType()
    {
        if (this instanceof BasicType) {
            return this.basicKind == BasicTypeKind.Void
        }
        return false
    }

    IsEnumType(): boolean
    {
        if (this instanceof BasicType) {
            return this.basicKind == BasicTypeKind.Enum
        }
        return false
    }

    IsStructType(): boolean
    {
        if (this instanceof BasicType) {
            return this.basicKind == BasicTypeKind.Struct
        }
        return false
    }


    IsCharType(): boolean
    {
        if (this instanceof BasicType) {
            return this.basicKind == BasicTypeKind.Char
        }
        return false
    }

    IsNumberType(): boolean
    {
        if (this instanceof BasicType) {
            return IsBaseTypeKindNumber(this.basicKind)
        }
        return false
    }

    IsPointerType()
    {
        if (this instanceof IndirectionType) {
            if (this.indirectionLevel > 0)
                return true
        } else if (this instanceof BasicType) {
            return this.basicKind == BasicTypeKind.CString
        }
        return false
    }

    PrettyPrint(): string
    {
        if (this instanceof ArrayType) {
            return `[${this.elementType.PrettyPrint()}, ${this.elementCount}}]`
        } else if (this instanceof BasicType) {
            return this.name
        } else if (this instanceof IndirectionType) {
            let result = this.elementType.PrettyPrint()
            for (let index = 0; index < this.indirectionLevel; index += 1) {
                result += "*"
            }
            return result
        }
        throw new Error("unreachable")
    }

    IncreaseIndirection(): Type 
    {
        return new IndirectionType(this, 1)
    }

    DecreaseIndirection(): Type
    {
        if (this instanceof IndirectionType) {
            if (this.indirectionLevel == 1)
                return this.elementType
            else
                return new IndirectionType(this.elementType, this.indirectionLevel - 1)
        } else {
            throw new Error(`Cannot decrease indirection on type ${this.PrettyPrint()}`)
        }
    }

    static GetTypeThatFitsBothTypes(a: Type, b: Type): Type | null
    {
        if (a.IsVoidType() || b.IsVoidType())
            return null

        if (Type.TypesIdentical(a, b))
            return a

        if (a.IsNumberType() && b.IsNumberType()) {
            if (BytesCountOfNumberType((a as BasicType).basicKind) <= BytesCountOfNumberType((b as BasicType).basicKind))
                return b
            else
                return a
        }

        if (a.IsPointerType() && b.IsVoidPointer())
            return a
        if (a.IsVoidPointer() && b.IsPointerType())
            return b

        return null
    }

    CanConvertTo(target: Type): TypeConversionResult
    {
        if (this.IsVoidType() || target.IsVoidType())
            return TypeConversionResult.NonConvertible

        if (Type.TypesIdentical(this, target))
            return TypeConversionResult.Identical

        if (this instanceof BasicType && target instanceof BasicType) {
            // We can convert numbers implicitly if they are smaller
            if (this.IsNumberType() && target.IsNumberType()) {
                if (BytesCountOfNumberType(this.basicKind) <= BytesCountOfNumberType(target.basicKind))
                    return TypeConversionResult.ImplictlyConvertible
                else
                    return TypeConversionResult.ExplicitlyConvertible
            }

            // We can convert chars explicitly
            if (this.basicKind == BasicTypeKind.Char && target.IsNumberType()) {
                return TypeConversionResult.ExplicitlyConvertible
            }
            if (this.IsNumberType() && target.basicKind == BasicTypeKind.Char) {
                return TypeConversionResult.ExplicitlyConvertible
            }

            // We can convert enums explicitly
            if (this.basicKind == BasicTypeKind.Enum && target.IsNumberType()) {
                return TypeConversionResult.ExplicitlyConvertible
            }
            if (this.IsNumberType() && target.basicKind == BasicTypeKind.Enum) {
                return TypeConversionResult.ExplicitlyConvertible
            }
        }

        // Void pointers are always implicitly convertible to other pointers
        if (this.IsPointerType() && target.IsVoidPointer())
            return TypeConversionResult.ImplictlyConvertible
        if (this.IsVoidPointer() && target.IsPointerType())
            return TypeConversionResult.ImplictlyConvertible

        // [type; _] -> type* implicitly
        if (this.GetIndirectionLevel() == target.GetIndirectionLevel()) {
            if (this instanceof ArrayType && target instanceof IndirectionType) {
                if (Type.TypesIdentical(this.elementType, target.elementType))
                    return TypeConversionResult.ImplictlyConvertible
            }
        }

        // char* -> const cha* implicitly
        if (this instanceof IndirectionType && target instanceof BasicType) {
            if (target.basicKind == BasicTypeKind.CString) {
                if (this.indirectionLevel == 1 && this.elementType instanceof BasicType) {
                    if (this.elementType.basicKind == BasicTypeKind.Char)
                        return TypeConversionResult.ImplictlyConvertible
                }
            }
        }

        return TypeConversionResult.NonConvertible
    }

}

export class ArrayType extends Type
{
    constructor(
        public elementType: Type,
        public elementCount: number,
    ) { super(TypeKind.Array) }
}

export class IndirectionType extends Type
{
    constructor(
        public elementType: Type,
        public indirectionLevel: number, // How many * come after type: int**** -> indirectionLevel == 4
    ) { super(TypeKind.Indirection) }
}

export class BasicType extends Type
{
    public name: string

    constructor(
        public basicKind: BasicTypeKind,
        name: string | null,
    )
    {
        super(TypeKind.Basic)

        if (IsBaseTypeKindPrimitive(basicKind)) {
            if (name != null)
                throw new Error(`Unexpected custom name ${name} in primitive type ${basicKind}`)
        } else {
            if (name == null)
                throw new Error(`Invalid custom type ${basicKind} with missing name`)
        }

        switch (basicKind) {
            case BasicTypeKind.Void:
                this.name = "void"
                break
            case BasicTypeKind.Bool:
                this.name = "bool"
                break
            case BasicTypeKind.Char:
                this.name = "char"
                break
            case BasicTypeKind.Byte:
                this.name = "byte"
                break
            case BasicTypeKind.Short:
                this.name = "short"
                break
            case BasicTypeKind.Int:
                this.name = "int"
                break
            case BasicTypeKind.Long:
                this.name = "long"
                break
            case BasicTypeKind.CString:
                this.name = "cstring"
                break
            default:
                this.name = name!
        }
    }
};




