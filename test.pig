import prelude

fun Main() {
    PrintValue("=====================================")
    PrintValue("RUNNING TESTS")

    TestBasics()
    TestComparisons()
    TestForLoops()
    TestWhileLoops()
    TestIf()
    TestFunctionCalls()
    TestGlobals()
    TestParenthesis()
    TestArrays()
    TestStrings()
    TestOperators()
    TestFunctionOrdering()
    TestScopeShadowing()
    TestEnums()
    TestParams()
    TestStructs()
    TestStructImpl()
    TestSwitchStatements()
    TestCasting()
    TestLocalPersist()
    TestPrecedence()

    PrintValue("=====================================")
    PrintValue("ALL TESTS PASSED")
}

fun TestBasics()
{
    let a = -1 + 2 * 3 - 4 / +2 + 2
    Assert(a == 5)

    let b = 123
    Assert(b == 123)

    let c = a + b

    Assert(c == a + b)
    Assert(c == 5 + 123)
    Assert(c == 128)
    Assert(c == (123 + (-1 + 2 * 3 - 4 / +2 + 2)))

    let d = 0xfe + 1
    Assert(d == 255)
    Assert(d == 0xff)

    let ch: string = '\x41'
    Assert(ch == 'A')

    let math = 1
    math += 1 + 2 + 2
    Assert(math == 6)
    math /= 1 + 1
    Assert(math == 3)
    math *= 1 + 1
    Assert(math == 6)
    math %= 2 + 2
    Assert(math == 2)
    math -= 2 - 1
    Assert(math == 1)

    let ternary = 4 < 3 
        ? math
        : 3 < 3 
            ? 1 + 2 + 3
            : 5
    Assert(ternary == 5)
}

fun TestComparisons()
{
    Assert(1 == 1)
    Assert(1 != 2)
    Assert(1 <  2)
    Assert(1 <= 2)
    Assert(1 >= 1)
    Assert(1 <= 1)
    Assert(1 >= 1)

    Assert(!(1 != 1))
    Assert(!(1 == 2))
    Assert(!(1 >= 2))
    Assert(!(1 >  2))
    Assert(!(1 <  1))
    Assert(!(1 >  1))
    Assert(!(1 <  1))
}

fun TestWhileLoops()
{
    let index = 0
    Assert(index == 0)
    while index < 10 {
        index = index + 1
        if (index == 5)
            break
    }
    Assert(index == 5)
}

fun TestForLoops()
{
    let value = 0 
    Assert(value == 0)
    for index in 2..5 { 
        value = index
    }
    Assert(value == 4)
    for index in 2..=5 { 
        value = index
    }
    Assert(value == 5)
}

fun TestIf()
{
    let i = 1
    let j = 2
    if (i < j) 
        Assert(true)
    else 
        Assert(false)
    
    if (2 > 1) {
        Assert(true)
        Assert(true)
        Assert(true)
    } else {
        Assert(false)
    }

    if (1==0)
        Assert(false)
    else
        if (1==1)
            Assert(true)
        else
            Assert(false)
}

struct ResultType
{
    first: number,
    second: number,
}
fun EmptyFunc()
{
}
fun Foo(): number {
  return 100
}
fun EarlyReturn()
{
    return
    Assert(false)
}
fun MustNotBeCalled(): bool {
    Assert(false)
    return true
}
fun ReturnsTrue(): bool {
    return true
}
fun ReturnsFalse(): bool {
    return false
}
fun ReturnComplexResult(): ResultType {
    let result: ResultType = ResultType()
    result.first = 1
    result.second = 2
    return result
}
fun TestFunctionCalls()
{
    let result = Foo()
    Assert(result == 100)
    Assert((Foo() + 100) == 200)
    EmptyFunc()
    EarlyReturn()
    let mymy: ResultType = ReturnComplexResult()
    Assert(mymy.first == 1)
    Assert(mymy.second == 2)

    let dummy: bool = ReturnsTrue() && ReturnsFalse() && MustNotBeCalled()
    Assert(dummy == false)
}

fun TestGlobalFunctionB() : number{
    return glob_b + 1 + Color.specialMembers[1].r
}
let glob_a: number = TestGlobalFunctionB()
let glob_b: number = 100 + 20 + 3 // + glob_a
let glob_c: string = "hello"
let glob_d: string = glob_c
let glob_e: string = "test"
fun TestGlobals()
{
    Assert(glob_a == glob_b + 1 + 255)
    Assert(glob_b == 123)
    Assert(glob_c == "hello")
    Assert(glob_d == glob_c)
    Assert(glob_e == "test")
}

fun TestParenthesis()
{
    Assert((1 + 2) * 3 == 9)
    Assert(1 + 2 * 3 == 1 + (2 * 3))
}

fun TestArrays()
{
    let a = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    let b = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    let c = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    let d: number

    b[5] = 123 
    Assert(b[5] == 123)
    d = 2
    c[5] = 2
    d = b[d + c[5] + 1]
    Assert(d == 123)

    let fill: number[] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    fill[5] = 500
    for index in 0..10 {
        if (index == 5)
            continue
        fill[index] = index
    }
    Assert(fill[0] == 0)
    Assert(fill[1] == 1)
    Assert(fill[2] == 2)
    Assert(fill[3] == 3)
    Assert(fill[4] == 4)
    Assert(fill[5] == 500)
    Assert(fill[6] == 6)
    Assert(fill[7] == 7)
    Assert(fill[8] == 8)
    Assert(fill[9] == 9)

    let five: number = 5
    let prefilled: number[] = [0, 1, 2, 3, 4, five, 6, 7, 8, 9]
    for index in 0..10 {
        Assert(prefilled[index] == index)
    }

    let charses: string[] = [ 'a', 'b', 'c' ]
    Assert(charses[0] == 'a')
    Assert(charses[1] == 'b')
    Assert(charses[2] == 'c')
}

fun TestStrings()
{
    let str: string = "Hellou\"uu\tworld!!\n"
    let cur: string = str

    // TODO
    // let index: number = 0
    // cur = str
    // while (*cur) {
    //     Assert(*cur == str[index])
    //     index += 1
    //     cur += 1
    // }
}

fun TestOperators()
{
    Assert((0 ^ 1) == 1)
    Assert(1 << 1 == 2)
    Assert(2 >> 1 == 1)
    Assert(~0 == -1)
    Assert(!false == true)
    Assert(!true == false)
    Assert((false || false) == false)
    Assert((false || true)  == true)
    Assert((false && true)  == false)
    Assert((true  && true)  == true)
    Assert((0 | 0) == 0)
    Assert((0 | 1) == 1)
    Assert((0 & 1) == 0)
    Assert((1 & 1) == 1)
}

fun TestScopeShadowing()
{
    let scope_test = 1
    Assert(scope_test == 1)
    {
        let scope_test = 2
        Assert(scope_test == 2)
        if (true) {
            let scope_test = 3
            Assert(scope_test == 3)
        } 
        Assert(scope_test == 2)
    }
    Assert(scope_test == 1)
}

fun Params1(a: number)
{
    Assert(a == 1)
}

fun Params2(a: number, b: number)
{
    Assert(a == 1)
    Assert(b == 2)
}

fun Params3(a: number, b: number, c: number)
{
    Assert(a == 1)
    Assert(b == 2)
    Assert(c == 3)
}

fun TestParams()
{
    Params1(1)
    Params2(1, 2)
    Params3(1, 2, 3)
}

fun TestFunctionOrdering()
{
    let result: number = ForwardDeclaredFunction(1, 2, 3)
    Assert(result == 6)
}
fun ForwardDeclaredFunction(a: number, b: number, c: number): number
{
    return a + b + c
}

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

fun TestSwitchStatements()
{
    let a = 5
    switch (a) {
        case 1: // Fallthrough
        case 3:
            Assert(false)
            break
        case 5:
            Assert(true)
            break
        default:
            Assert(false)
            break
    }

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

fun LocalPersistFunc(): number
{
    letpersist s_counter: number = 0
    let result: number = s_counter
    s_counter += 1
    {
        letpersist s_counterAnother: number = 0
        result += s_counterAnother
        s_counterAnother += 1
    }
    return result
}

fun TestLocalPersist()
{
    Assert(LocalPersistFunc() == 0)
    Assert(LocalPersistFunc() == 2)
    Assert(LocalPersistFunc() == 4)
    Assert(LocalPersistFunc() == 6)
}

fun TestPrecedence()
{
    let a: number? = null
    let b: number? = null
    let c: number? = null
    if (a == null || b == null || c == null) {
        Assert(true)
    } else {
        Assert(false)
    }
    if (a == null && b == null && c == null) {
        Assert(true)
    } else {
        Assert(false)
    }
}