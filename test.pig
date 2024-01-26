import prelude
import test_playground
import test_types

fun Main() {
    PrintValue("=====================================")
    PrintValue("RUNNING TESTS")

    TestPlayground()

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
    TestParams()
    TestSwitchStatements()
    TestLocalPersist()
    TestPrecedence()
    TestTypes()

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