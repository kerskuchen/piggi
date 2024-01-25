import prelude

enum MyEnum {
    One = 1,
    Three = 3,
    Five = 5,
}

fun TestEnums()
{
    let enummi: MyEnum = MyEnum.Three
    Assert(enummi == MyEnum.Three)
    Assert(enummi != MyEnum.One)
    Assert(enummi > MyEnum.One)
    Assert(MyEnum.One < MyEnum.Three)
    Assert(MyEnum.One < MyEnum.Five)
    Assert(MyEnum.One < MyEnum.Five)

    let arr: MyEnum[] = [ MyEnum.One, MyEnum.Three, MyEnum.Five ]
    Assert(arr[0] == MyEnum.One)
    Assert(arr[1] == MyEnum.Three)
    Assert(arr[2] == MyEnum.Five)

    let numnum: MyEnum = MyEnum.Five
    switch (numnum) {
        case MyEnum.One: { {
                Assert(false)
                break
            }
        }
        case MyEnum.Three:
            Assert(false)
            break
        case MyEnum.Five:
            Assert(true)
            break
        default:
            Assert(false)
            break
    }
}
struct MyOtherStruct {
    lala: number,
    color: Color,
    eee: MyEnum,
}
struct Color {
    r: number,
    g: number,
    b: number,
}
let black: number = 123
fun GetSum(a : number, b : number): number 
{
    return a + b
}
impl Color {
    let specialMembers: Color[] = [
        Color.black,
        Color.white,
        Color.green,
        Color(0, 255, 0), 
    ]
    let black: Color = Color()
    let white: Color = Color.FromGrey(255)
    let blue: Color = Color.specialMembers[3]
    let green: Color = Color(0, 0, 255)
    let yellow: Color = Color(0, Color.blue.b, Color.green.g)

    met GetSum() : number {
        return this.r + this.g + this.b
    }

    met GetMax() : number {
        let max = 0
        if (this.r > max)
            max = this.r
        if (this.g > max)
            max = this.g
        if (this.b > max)
            max = this.b
        return max
    }

    fun FromGrey(grey: number) : Color {
        return Color(grey, grey, grey)
    }
}
fun TestStructImpl()
{
    let blue1 = Color.blue
    let blue2 = Color.specialMembers[3]
    Assert(blue1.b == blue2.b)

    let max = blue1.GetMax()
    Assert(max == 255)

    let sum = GetSum(1,1)
    Assert(sum == 2)

    let magenta = Color(Color.white.r, Color.black.g, Color.white.b)
    Assert(magenta.r == 255)
    Assert(magenta.g == 0)
    Assert(magenta.b == 255)
    Assert(magenta.GetSum() == 2 * 255)
}

struct SelfReferential {
    value: number[],
    next: SelfReferential?,
    previous: SelfReferential?,
}
fun TestStructs()
{
    let col = Color()
    col.r = 255
    Assert(col.r == 255)
    Assert(col.g == 0)
    Assert(col.b == 0)

    let r: number = col.r + 5
    Assert(r == 260)

    let mymy = MyOtherStruct()
    mymy.color = col
    Assert(mymy.color.r == 255)
    Assert(mymy.color.g == 0)
    Assert(mymy.color.b == 0)

    let b = mymy.color.r + 5
    Assert(b == 260)

    let one = SelfReferential()
    one.value = [1, 1, 1]
    let two = SelfReferential()
    two.value = [2, 2, 2]
    let tre = SelfReferential()
    tre.value = [3, 3, 3]

    one.previous = null
    one.next = two

    two.previous = one
    two.next = tre

    tre.previous = two
    tre.next = null

    Assert(one.value[0] == 1)
    Assert(one.value[1] == 1)
    Assert(one.value[2] == 1)

    Assert(two.value[0] == 2)
    Assert(two.value[1] == 2)
    Assert(two.value[2] == 2)

    Assert(tre.value[0] == 3)
    Assert(tre.value[1] == 3)
    Assert(tre.value[2] == 3)

    Assert(one.next.value[0] == 2)
    Assert(two.next.value[0] == 3)
    Assert(tre.next == null)

    Assert(one.previous == null)
    Assert(two.previous.value[0] == 1)
    Assert(tre.previous.value[0] == 2)

    Assert(one.next.next.value[0] == 3)
    Assert(tre.previous.previous.value[0] == 1)

    Assert(one.next.next.previous.previous.value[0] == 1)
    Assert(one.next.next.previous.previous.value[0] + 4 == 5)
}

struct Shape {
    area: number,
}
struct Square {
    area: number,
    width: number,
}

fun TestCasting()
{
    let a: number = true as number
    Assert(a == 1)
    Assert(false as number == 0)

    let b: number = MyEnum.Five as number
    Assert(b == 5)

    let square: Square = Square()
    square.area = 25
    square.width = 5
    let shape: Shape? = null
    Assert(shape == null)
    // TODO
    // shape = square as Shape
    // Assert(shape.area == 25)
}

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
    // let num = ReturnDefaultForType<number>()
    // Assert(num == 0)
    let num2 = EchoValue(123)
    Assert(num2 == 123)

    // / let str = ReturnDefaultForType<string>()
    // / Assert(str == "")
    let str2 = EchoValue("hello")
    Assert(str2 == "hello")

    // / let bul = ReturnDefaultForType<bool>()
    // / Assert(bul == false)
    let bul2 = EchoValue(true)
    Assert(bul2 == true)

    // / let nil = ReturnDefaultForType<any>()
    // / Assert(nil == null)
    let nil2 = EchoValue(null)
    Assert(nil2 == null)
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



fun TestTypes(){
    TestEnums()
    TestStructs()
    TestStructImpl()
    TestCasting()
    TestTemplateFunctions()
    // TestTemplateStructs()
}