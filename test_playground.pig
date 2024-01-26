
fun EchoValue<ValueType>(value: ValueType) : ValueType {
    let resultArray: ValueType[] = [value, value]
    let resultArrayInferred = [value, value, value]
    let resultArrayCombined = [resultArray[0], resultArrayInferred[0]]
    return resultArrayCombined[0]
}

// TODO
// fun EchoFirst<FirstType, SecondType>(first: FirstType, second: SecondType) : FirstType {
//     return first
// }
// 
// fun EchoSecond<FirstType, SecondType>(first: FirstType, second: SecondType) : FirstType {
//     return second
// }
// 
// fun ReturnDefaultForType<InputType>() : InputType {
//     return InputType()
// }

fun TestTemplateFunctions() {
    // let num = ReturnDefaultForType::<number>()
    // Assert(num == 0)
    let num2 = EchoValue<number>(123)
    Assert(num2 == 123)
    // let num3 = EchoValue(321)
    // Assert(num3 == 321)

    // let str = ReturnDefaultForType::<string>()
    // Assert(str == "")
    let str2 = EchoValue<string>("hello")
    Assert(str2 == "hello")
    // let str3 = EchoValue("olleh")
    // Assert(str3 == "olleh")

    // let bul = ReturnDefaultForType::<bool>()
    // Assert(bul == false)
    let bul2 = EchoValue<bool>(true)
    Assert(bul2 == true)
    // let bul3 = EchoValue(false)
    // Assert(bul3 == false)

    // let nil = ReturnDefaultForType<any>()
    // Assert(nil == null)
    let nil2 = EchoValue<any>(null)
    Assert(nil2 == null)
    // let nil3 = EchoValue(null)
    // Assert(nil3 == null)
}

/*
TODO
struct Container<FirstType, SecondType> 
{
    first: FirstType
    second: SecondType
}

impl Container<FirstType, SecondType> 
{
    fun CreateFirstDefault() : FirstType {
        return FirstType()
    }

    fun CreateSecondDefault() : SecondType {
        return SecondType()
    }

    met GetFirst() : FirstType 
    {
        return this.first
    }

    met GetSecond() : SecondType 
    {
        return this.second
    }
}

fun TestTemplateStructs() {
    let stringnumContainer = Container<string, number>()
    Assert(numstringContainer.first == "")
    Assert(numstringContainer.second == 0)

    let numstringContainer = Container(123, "hello")
    Assert(numstringContainer.first == 123)
    Assert(numstringContainer.second == "hello")
}

*/



fun TestPlayground() {
    TestTemplateFunctions()
    // TestTemplateStructs()
}