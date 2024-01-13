import prelude

fun PrintNumber(value: long)
{
    fprintf(stdout, "%lld\n", value)
}

fun PrintString(value: char*)
{
    fprintf(stdout, "%s\n", value)
}

fun PrintChar(value: char)
{
    fprintf(stdout, "%c\n", value)
}

fun TestBasics()
{
    let a: int = -1 + 2 * 3 - 4 / +5
    assert(a == 5)

    let b: int = 123
    assert(b == 123)

    let c: int = a + b

    assert(c == a + b)
    assert(c == 5 + 123)
    assert(c == 128)
    assert(c == (123 + (-1 + 2 * 3 - 4 / +5)))

    let d: int = 0xfe + 1
    assert(d == 255)
    assert(d == 0xff)

    let ch: char = '\xff'
    assert(ch == 255 as char)

    let math: int = 1
    math += 1 + 2 + 2
    assert(math == 6)
    math /= 1 + 1
    assert(math == 3)
    math *= 1 + 1
    assert(math == 6)
    math %= 2 + 2
    assert(math == 2)
    math -= 2 - 1
    assert(math == 1)

    let ternary: int = 4 < 3 
        ? math
        : 3 < 3 
            ? 1 + 2 + 3
            : 5
    assert(ternary == 5)
}

fun TestComparisons()
{
    assert(1 == 1)
    assert(1 != 2)
    assert(1 <  2)
    assert(1 <= 2)
    assert(1 >= 1)
    assert(1 <= 1)
    assert(1 >= 1)

    assert(!(1 != 1))
    assert(!(1 == 2))
    assert(!(1 >= 2))
    assert(!(1 >  2))
    assert(!(1 <  1))
    assert(!(1 >  1))
    assert(!(1 <  1))
}

fun TestWhileLoops()
{
    let index: int = 0
    assert(index == 0)
    while (index < 10) {
        index = index + 1
        if (index == 5)
            break
    }
    assert(index == 5)

    let j: byte = 120
    assert(j == 120)
    while (j != 5) {
        j += 1
    }
    assert(j == 5)
}

fun TestForLoops()
{
    let i: int = 0 
    assert(i == 0)
    for i in 2..5 { }
    assert(i == 5)
}

fun TestIf()
{
    let i: int = 1
    let j: int = 2
    if (i < j) 
        assert(1)
    else 
        assert(0)
    
    if (2 > 1) {
        assert(1)
        assert(2)
        assert(3)
    } else {
        assert(0)
    }

    if (1==0)
        assert(0)
    else
        if (1==1)
            assert(1)
        else
            assert(0)
}

struct ResultType
{
    first: int,
    second: int,
}
fun EmptyFunc()
{
}
fun Foo(): int {
  return 100
}
fun EarlyReturn()
{
    return
    assert(0)
}
fun MustNotBeCalled(): int {
    assert(0)
    return 1
}
fun ReturnsOne(): int {
    return 1
}
fun ReturnsZero(): int {
    return 0
}
fun ReturnComplexResult(): ResultType {
    let result: ResultType
    result.first = 1
    result.second = 2
    return result
}
fun TestFunctionCalls()
{
    let result: int = Foo()
    assert(result == 100)
    assert((Foo() + 100) == 200)
    EmptyFunc()
    EarlyReturn()
    let mymy: ResultType = ReturnComplexResult()
    assert(mymy.first == 1)
    assert(mymy.second == 2)

    let dummy: int = ReturnsOne() && ReturnsZero() && MustNotBeCalled()
    assert(dummy == 0)
}

fun TestPointers()
{
    let a: short  = 123 
    let b: short* = &a 
    let c: short  = *b
    assert(c == 123)

    let d: int  = 321 
    let e: int* = &d 
    let f: int  = *e
    assert(f == 321)

    let zero: int* = null
    if (zero)
        assert(0)
    else
        assert(1)

    let nonzero: int* = &d
    if (nonzero && *nonzero > 0)
        assert(1)
    else
        assert(0)
}

let glob_d: int = 100 + 20 + 3 
let glob_e: int*
let glob_f: int
fun TestGlobals()
{
    glob_e = &glob_d 
    glob_f = *glob_e 
    assert(glob_d == 123)
    assert(glob_f == 123)
}

fun TestPointerMath()
{
    let a: int = 10
    let b: int = 20
    let c: int = 30

    let ap: int* = &a
    let bp: int* = &b
    let cp: int* = &c

    // Seems we cannot cannot assert these because of address layout changes?
    PrintNumber(cp - ap) // assert(cp - ap == 2)
    PrintNumber(cp - bp) // assert(cp - bp == 1)
    PrintNumber(bp - ap) // assert(bp - ap == 1)

    let cp2: int* = ap + 2
    PrintNumber(*cp2) // assert(*cp2 == c)

    let ap2: int* = cp - 2
    PrintNumber(*ap2) // assert(*ap2 == a)
}

fun TestLValues()
{
    let a:  int = 10
    let ap: int* = &a
    assert(*ap == a)
    assert(*ap == 10)
    *ap = 123
    assert(a == 123)
}

fun TestParenthesis()
{
    assert((1 + 2) * 3 == 9)
    assert(1 + 2 * 3 == 1 + (2 * 3))
}

fun IndexArray(arrPointer: int* , index: int): int {
    return *(arrPointer + index)
}
fun TestArrays()
{
    let a: [char; 100]
    let b: [int; 100]
    let c: [long; 100]
    let d: int

    b[15] = 123 
    d = 5
    c[10] = 5
    d = b[d + c[10] + 5]
    assert(d == 123)

    // let index: int
    let fill: [int; 10]
    fill[5] = 500
    for index in 0..10 {
        if (index == 5)
            continue
        fill[index] = index
    }
    assert(fill[0] == 0)
    assert(fill[1] == 1)
    assert(fill[2] == 2)
    assert(fill[3] == 3)
    assert(fill[4] == 4)
    assert(fill[5] == 500)
    assert(fill[6] == 6)
    assert(fill[7] == 7)
    assert(fill[8] == 8)
    assert(fill[9] == 9)

    let five: int = 5
    let prefilled: [int] = { 0, 1, 2, 3, 4, five, 6, 7, 8, 9 }
    for index in 0..10 {
        assert(prefilled[index] == index)
    }
    let prefilledPointer: int* = prefilled
    for index in 0..10 {
        assert(*prefilledPointer == index)
        prefilledPointer += 1
    }
    let fivePointer: int* = &prefilled[5]
    assert(*fivePointer == 5)
    let result: int = IndexArray(prefilled, 5)
    assert(result == 5)

    let charses: [char] = { 'a', 'b', 'c' }
    assert(charses[0] == 'a')
    assert(charses[1] == 'b')
    assert(charses[2] == 'c')
    PrintChar(charses[0])
    PrintChar(charses[1])
    PrintChar(charses[2])
}

fun TestStrings()
{
    let str: char* = "Hellou" "\"uu\tworld!!\n"
    PrintString(str)

    let cur: char* = str
    assert(*cur == 'H')  cur += 1
    assert(*cur == 'e')  cur += 1
    assert(*cur == 'l')  cur += 1
    assert(*cur == 'l')  cur += 1
    assert(*cur == 'o')  cur += 1
    assert(*cur == 'u')  cur += 1
    assert(*cur == '"')  cur += 1
    assert(*cur == 'u')  cur += 1
    assert(*cur == 'u')  cur += 1
    assert(*cur == '\t') cur += 1
    assert(*cur == 'w')  cur += 1
    assert(*cur == 'o')  cur += 1
    assert(*cur == 'r')  cur += 1
    assert(*cur == 'l')  cur += 1
    assert(*cur == 'd')  cur += 1
    assert(*cur == '!')  cur += 1
    assert(*cur == '!')

    assert(*cur == '!')  cur -= 1
    assert(*cur == '!')  cur -= 1
    assert(*cur == 'd')  cur -= 1
    assert(*cur == 'l')  cur -= 1
    assert(*cur == 'r')  cur -= 1
    assert(*cur == 'o')  cur -= 1
    assert(*cur == 'w')  cur -= 1
    assert(*cur == '\t') cur -= 1
    assert(*cur == 'u')  cur -= 1
    assert(*cur == 'u')  cur -= 1
    assert(*cur == '"')  cur -= 1
    assert(*cur == 'u')  cur -= 1
    assert(*cur == 'o')  cur -= 1
    assert(*cur == 'l')  cur -= 1
    assert(*cur == 'l')  cur -= 1
    assert(*cur == 'e')  cur -= 1
    assert(*cur == 'H')  cur -= 1

    let index: int = 0
    cur = str
    while (*cur) {
        assert(*cur == str[index])
        index += 1
        cur += 1
    }
}

fun TestOperators()
{
    assert(0 ^ 1 == 1)
    assert(1 << 1 == 2)
    assert(2 >> 1 == 1)
    assert(~0 == -1)
    assert(!0 == 1)
    assert(!1 == 0)
    assert((0 || 0) == 0)
    assert((0 || 1) == 1)
    assert((0 && 1) == 0)
    assert((1 && 1) == 1)
    assert((0 | 0) == 0)
    assert((0 | 1) == 1)
    assert((0 & 1) == 0)
    assert((1 & 1) == 1)
}

let scope_test: int = 1
fun TestScopeShadowing()
{
    assert(scope_test == 1)

    let scope_test: int = 2
    assert(scope_test == 2)
    {
        let scope_test: int = 3
        assert(scope_test == 3)
        if (1) {
            let scope_test: int = 4
            assert(scope_test == 4)
        } 
        assert(scope_test == 3)
    }
    assert(scope_test == 2)
}

fun Params1(a: short)
{
    assert(a == 1)
}

fun Params2(a: short, b: int)
{
    assert(a == 1)
    assert(b == 2)
}

fun Params3(a: short, b: int, c: long)
{
    assert(a == 1)
    assert(b == 2)
    assert(c == 3)
}

fun TestParams()
{
    Params1(1)
    Params2(1, 2)
    Params3(1, 2, 3)
}

fun ForwardDeclaredFunction(a: short, b: int, c: long): long
fun TestForwardDeclaredFunction()
{
    let result: long = ForwardDeclaredFunction(1, 2, 3)
    assert(result == 6)
}
fun ForwardDeclaredFunction(a: short , b: int , c: long): long
{
    return a + b + c
}

fun TestMultipointer()
{
    let a: int = 5
    let ap: int* = &a
    let app: int** = &ap
    assert(a == 5)
    assert(*ap == 5)
    assert(**app == 5)
}

struct MyStruct // Forward declaration
struct MyOtherStruct {
    lala: int,
    other: MyStruct*,
}
struct MyStruct {
    a: short,
    b: int,
    c: long,
    d: int*,
}
struct SelfReferential {
    value: [int; 3],
    next: SelfReferential*,
    previous: SelfReferential*,
}
fun TestStructs()
{
    let my: MyStruct
    my.a = 5

    let myp: MyStruct* = &my
    assert(myp.a == 5)

    let a: int = myp.a + 5
    assert(a == 10)

    let mymy: MyOtherStruct
    mymy.other = myp
    assert(mymy.other.a == 5)

    let b: int = mymy.other.a + 5
    assert(b == 10)

    let one: SelfReferential
    one.value[0] = 1
    one.value[1] = 1
    one.value[2] = 1
    let two: SelfReferential
    two.value[0] = 2
    two.value[1] = 2
    two.value[2] = 2
    let tre: SelfReferential
    tre.value[0] = 3
    tre.value[1] = 3
    tre.value[2] = 3

    one.previous = null
    one.next = &two

    two.previous = &one
    two.next = &tre

    tre.previous = &two
    tre.next = null

    assert(one.value[0] == 1)
    assert(one.value[1] == 1)
    assert(one.value[2] == 1)

    assert(two.value[0] == 2)
    assert(two.value[1] == 2)
    assert(two.value[2] == 2)

    assert(tre.value[0] == 3)
    assert(tre.value[1] == 3)
    assert(tre.value[2] == 3)

    assert(one.next.value[0] == 2)
    assert(two.next.value[0] == 3)
    assert(tre.next == null)

    assert(one.previous == null)
    assert(two.previous.value[0] == 1)
    assert(tre.previous.value[0] == 2)

    assert(one.next.next.value[0] == 3)
    assert(tre.previous.previous.value[0] == 1)

    assert(one.next.next.previous.previous.value[0] == 1)
    assert(one.next.next.previous.previous.value[0] + 4 == 5)
}

struct MyUnion // Forward declaration
struct MyOtherUnion {
    lala: int,
    other: MyUnion*,
}
struct MyUnion {
    a: short,
    b: int,
    c: long,
    d: int*,
}
fun TestUnions()
{
    let my: MyUnion
    my.a = 5

    let myp: MyUnion* = &my
    assert(myp.a == 5)

    let a: int = myp.a + 5
    assert(a == 10)

    let mymy: MyOtherUnion
    mymy.other = myp
    assert(mymy.other.a == 5)

    let b: int = mymy.other.a + 5
    assert(b == 10)
}

enum MyEnum {
    One = 1,
    Three = 3,
    Five = 5,
}

fun EnumTest()
{
    let enummi: MyEnum = MyEnum.Three
    assert(enummi == MyEnum.Three)
    assert(enummi != MyEnum.One)
    assert(enummi > MyEnum.One)
    assert(MyEnum.One < MyEnum.Three)
    assert(MyEnum.One < MyEnum.Five)
    assert(MyEnum.One < MyEnum.Five)

    let enummip: MyEnum* = &enummi
    assert(*enummip == MyEnum.Three)

    let arr: [MyEnum] = { MyEnum.One, MyEnum.Three, MyEnum.Five }
    assert(arr[0] == MyEnum.One)
    assert(arr[1] == MyEnum.Three)
    assert(arr[2] == MyEnum.Five)
}

fun TestSwitchStatements()
{
    let a: int = 5
    switch (a) {
        case 1: // Fallthrough
        case 3:
            assert(0)
            break
        case 5:
            assert(1)
            break
        default:
            assert(0)
            break
    }

    let numnum: MyEnum = MyEnum.Five
    switch (numnum) {
        case MyEnum.One: { {
                assert(0)
                break
            }
        }
        case MyEnum.Three:
            assert(0)
            break
        case MyEnum.Five:
            assert(1)
            break
        default:
            assert(0)
            break
    }

}

struct Shape {
    area: int,
}
struct Square {
    area: int,
    width: int,
}

fun TestCasting()
{
    let a: byte = (127 + 4) as byte
    assert(a == 4)

    let b: int = MyEnum.Five as int
    assert(b == 5)

    let square: Square
    square.area = 25
    square.width = 5
    let shape: Shape* = null
    assert(shape == null)
    shape = &square as Shape*
    assert(shape.area == 25)
    assert(&square as Shape* == shape)
}

fun TestSizeof()
{
    assert(sizeof(char) == 1)
    assert(sizeof(byte) == 1)
    assert(sizeof(short) == 2)
    assert(sizeof(int) == 4)
    assert(sizeof(long) == 8)
    assert(sizeof(Shape) == sizeof(int))
    assert(sizeof(Square) == 2 * sizeof(int))
    assert(sizeof(Shape*) == sizeof(Square*))
}

fun LocalPersistFunc(): int
{
    letpersist s_counter: int = 0
    let result: int = s_counter
    s_counter += 1
    return result
}

fun TestLocalPersist()
{
    assert(LocalPersistFunc() == 0)
    assert(LocalPersistFunc() == 1)
    assert(LocalPersistFunc() == 2)
    assert(LocalPersistFunc() == 3)
}

fun TestPrecedence()
{
    let a: int* = null
    let b: int* = null
    let c: int* = null
    if (a == null || b == null || c == null) {
        assert(1)
    } else {
        assert(0)
    }
    if (a == null && b == null && c == null) {
        assert(1)
    } else {
        assert(0)
    }
}

fun main() {
    TestBasics()
    TestComparisons()
    TestForLoops()
    TestWhileLoops()
    TestIf()
    TestFunctionCalls()
    TestPointers()
    TestGlobals()
    TestPointerMath()
    TestLValues()
    TestParenthesis()
    TestArrays()
    TestStrings()
    TestOperators()
    TestScopeShadowing()
    TestParams()
    TestMultipointer()
    TestStructs()
    TestUnions()
    TestSwitchStatements()
    TestCasting()
    TestSizeof()
    TestLocalPersist()

    PrintString("=====================================")
    PrintString("ALL TESTS PASSED")
    PrintString("=====================================")
}