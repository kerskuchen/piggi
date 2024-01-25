////////////////////////////////////////////////////////////////////////////////////////////////////
// Preamble

// deno-lint-ignore-file prefer-const

function Assert(value) { if (!value) throw new Error("assert") }
function PrintValue(value) { console.log(value) }

////////////////////////////////////////////////////////////////////////////////////////////////////
// Global variables

// NOTE: The following are initialized in the __GlobalVariableInitializer() function
let black /*: number = 123 */
let glob_a /*: number = TestGlobalFunctionB() */
let glob_b /*: number = 100 + 20 + 3 */
let glob_c /*: string = "hello" */
let glob_d /*: string = glob_c */
let glob_e /*: string = "test" */

////////////////////////////////////////////////////////////////////////////////////////////////////
// Enums

class MyEnum {
    static One = 1
    static Three = 3
    static Five = 5
    static __Default = MyEnum.One
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Classes

class MyOtherStruct {
    constructor(lala, color, eee) {
        this.lala = lala
        this.color = color
        this.eee = eee
    }
    static Default() {
        return new MyOtherStruct(
            0, // lala
            Color, // color
            MyEnum.__Default, // eee
        )
    }
    
}

class Color {
    constructor(r, g, b) {
        this.r = r
        this.g = g
        this.b = b
    }
    static Default() {
        return new Color(
            0, // r
            0, // g
            0, // b
        )
    }
    // NOTE: The following are initialized in the __GlobalVariableInitializer() function
    static specialMembers /*: Color[] = [Color.black, Color.white, Color.green, new Color(0, 255, 0)] */
    static black /*: Color = Color.Default() */
    static white /*: Color = Color.FromGrey(255) */
    static blue /*: Color = Color.specialMembers[3] */
    static green /*: Color = new Color(0, 0, 255) */
    static yellow /*: Color = new Color(0, Color.blue.b, Color.green.g) */
    
    static FromGrey(grey /*: number*/ )  /*: Color*/ {
        return new Color(grey, grey, grey)
    }
    
    GetSum()  /*: number*/ {
        return this.r + this.g + this.b
    }
    
    GetMax()  /*: number*/ {
        let max = 0
        if ((this.r > max)) {
            max = this.r
        }
        if ((this.g > max)) {
            max = this.g
        }
        if ((this.b > max)) {
            max = this.b
        }
        return max
    }
    
    
}

class SelfReferential {
    constructor(value, next, previous) {
        this.value = value
        this.next = next
        this.previous = previous
    }
    static Default() {
        return new SelfReferential(
            [], // value
            null, // next
            null, // previous
        )
    }
    
}

class Shape {
    constructor(area) {
        this.area = area
    }
    static Default() {
        return new Shape(
            0, // area
        )
    }
    
}

class Square {
    constructor(area, width) {
        this.area = area
        this.width = width
    }
    static Default() {
        return new Square(
            0, // area
            0, // width
        )
    }
    
}

class ResultType {
    constructor(first, second) {
        this.first = first
        this.second = second
    }
    static Default() {
        return new ResultType(
            0, // first
            0, // second
        )
    }
    
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Free Functions

function TestEnums() {
    let enummi = MyEnum.Three
    Assert(enummi == MyEnum.Three)
    Assert(enummi != MyEnum.One)
    Assert(enummi > MyEnum.One)
    Assert(MyEnum.One < MyEnum.Three)
    Assert(MyEnum.One < MyEnum.Five)
    Assert(MyEnum.One < MyEnum.Five)
    let arr = [MyEnum.One, MyEnum.Three, MyEnum.Five]
    Assert(arr[0] == MyEnum.One)
    Assert(arr[1] == MyEnum.Three)
    Assert(arr[2] == MyEnum.Five)
    let numnum = MyEnum.Five
    switch ((numnum)) {
        case MyEnum.One: {
            Assert(false)
            break
        }
        case MyEnum.Three: {
            Assert(false)
            break
        }
        case MyEnum.Five: {
            Assert(true)
            break
        }
        default: {
            Assert(false)
            break
        }
    }
}

function GetSum(a /*: number*/ , b /*: number*/ )  /*: number*/ {
    return a + b
}

function TestStructImpl() {
    let blue1 = Color.blue
    let blue2 = Color.specialMembers[3]
    Assert(blue1.b == blue2.b)
    let max = blue1.GetMax()
    Assert(max == 255)
    let sum = GetSum(1, 1)
    Assert(sum == 2)
    let magenta = new Color(Color.white.r, Color.black.g, Color.white.b)
    Assert(magenta.r == 255)
    Assert(magenta.g == 0)
    Assert(magenta.b == 255)
    Assert(magenta.GetSum() == 2 * 255)
}

function TestStructs() {
    let col = Color.Default()
    col.r = 255
    Assert(col.r == 255)
    Assert(col.g == 0)
    Assert(col.b == 0)
    let r = col.r + 5
    Assert(r == 260)
    let mymy = MyOtherStruct.Default()
    mymy.color = col
    Assert(mymy.color.r == 255)
    Assert(mymy.color.g == 0)
    Assert(mymy.color.b == 0)
    let b = mymy.color.r + 5
    Assert(b == 260)
    let one = SelfReferential.Default()
    one.value = [1, 1, 1]
    let two = SelfReferential.Default()
    two.value = [2, 2, 2]
    let tre = SelfReferential.Default()
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

function TestCasting() {
    let a = true
    Assert(a == 1)
    Assert(false == 0)
    let b = MyEnum.Five
    Assert(b == 5)
    let square = Square.Default()
    square.area = 25
    square.width = 5
    let shape = null
    Assert(shape == null)
}

function TestTypes() {
    TestEnums()
    TestStructs()
    TestStructImpl()
    TestCasting()
}

function Main() {
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
    TestParams()
    TestSwitchStatements()
    TestLocalPersist()
    TestPrecedence()
    TestTypes()
    PrintValue("=====================================")
    PrintValue("ALL TESTS PASSED")
}

function TestBasics() {
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
    let ch = '\x41'
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
    let ternary = 4 < 3 ? math : 3 < 3 ? 1 + 2 + 3 : 5
    Assert(ternary == 5)
}

function TestComparisons() {
    Assert(1 == 1)
    Assert(1 != 2)
    Assert(1 < 2)
    Assert(1 <= 2)
    Assert(1 >= 1)
    Assert(1 <= 1)
    Assert(1 >= 1)
    Assert(! (1 != 1))
    Assert(! (1 == 2))
    Assert(! (1 >= 2))
    Assert(! (1 > 2))
    Assert(! (1 < 1))
    Assert(! (1 > 1))
    Assert(! (1 < 1))
}

function TestWhileLoops() {
    let index = 0
    Assert(index == 0)
    while (index < 10){
        index = index + 1
        if ((index == 5)) {
            break
        }
    }
    Assert(index == 5)
}

function TestForLoops() {
    let value = 0
    Assert(value == 0)
    for (let index = 2; index < 5; index += 1) {
        value = index
    }
    Assert(value == 4)
    for (let index = 2; index <= 5; index += 1) {
        value = index
    }
    Assert(value == 5)
}

function TestIf() {
    let i = 1
    let j = 2
    if ((i < j)) {
        Assert(true)
    } else {
        Assert(false)
    }
    if ((2 > 1)) {
        Assert(true)
        Assert(true)
        Assert(true)
    } else {
        Assert(false)
    }
    if ((1 == 0)) {
        Assert(false)
    } else {
        if ((1 == 1)) {
            Assert(true)
        } else {
            Assert(false)
        }
    }
}

function EmptyFunc() {}

function Foo()  /*: number*/ {
    return 100
}

function EarlyReturn() {
    return 
    Assert(false)
}

function MustNotBeCalled()  /*: bool*/ {
    Assert(false)
    return true
}

function ReturnsTrue()  /*: bool*/ {
    return true
}

function ReturnsFalse()  /*: bool*/ {
    return false
}

function ReturnComplexResult()  /*: ResultType*/ {
    let result = ResultType.Default()
    result.first = 1
    result.second = 2
    return result
}

function TestFunctionCalls() {
    let result = Foo()
    Assert(result == 100)
    Assert((Foo() + 100) == 200)
    EmptyFunc()
    EarlyReturn()
    let mymy = ReturnComplexResult()
    Assert(mymy.first == 1)
    Assert(mymy.second == 2)
    let dummy = ReturnsTrue() && ReturnsFalse() && MustNotBeCalled()
    Assert(dummy == false)
}

function TestGlobalFunctionB()  /*: number*/ {
    return glob_b + 1 + Color.specialMembers[1].r
}

function TestGlobals() {
    Assert(glob_a == glob_b + 1 + 255)
    Assert(glob_b == 123)
    Assert(glob_c == "hello")
    Assert(glob_d == glob_c)
    Assert(glob_e == "test")
}

function TestParenthesis() {
    Assert((1 + 2) * 3 == 9)
    Assert(1 + 2 * 3 == 1 + (2 * 3))
}

function TestArrays() {
    let a = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    let b = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    let c = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    let d /*: number*/ 
    b[5] = 123
    Assert(b[5] == 123)
    d = 2
    c[5] = 2
    d = b[d + c[5] + 1]
    Assert(d == 123)
    let fill = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    fill[5] = 500
    for (let index = 0; index < 10; index += 1) {
        if ((index == 5)) {
            continue
        }
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
    let five = 5
    let prefilled = [0, 1, 2, 3, 4, five, 6, 7, 8, 9]
    for (let index = 0; index < 10; index += 1) {
        Assert(prefilled[index] == index)
    }
    let charses = ['a', 'b', 'c']
    Assert(charses[0] == 'a')
    Assert(charses[1] == 'b')
    Assert(charses[2] == 'c')
}

function TestStrings() {
    let str = "Hellou\"uu\tworld!!\n"
    let cur = str
}

function TestOperators() {
    Assert((0 ^ 1) == 1)
    Assert(1 << 1 == 2)
    Assert(2 >> 1 == 1)
    Assert(~ 0 == -1)
    Assert(! false == true)
    Assert(! true == false)
    Assert((false || false) == false)
    Assert((false || true) == true)
    Assert((false && true) == false)
    Assert((true && true) == true)
    Assert((0 | 0) == 0)
    Assert((0 | 1) == 1)
    Assert((0 & 1) == 0)
    Assert((1 & 1) == 1)
}

function TestScopeShadowing() {
    let scope_test = 1
    Assert(scope_test == 1)
    {
        let scope_test = 2
        Assert(scope_test == 2)
        if ((true)) {
            let scope_test = 3
            Assert(scope_test == 3)
        }
        Assert(scope_test == 2)
    }
    Assert(scope_test == 1)
}

function Params1(a /*: number*/ ) {
    Assert(a == 1)
}

function Params2(a /*: number*/ , b /*: number*/ ) {
    Assert(a == 1)
    Assert(b == 2)
}

function Params3(a /*: number*/ , b /*: number*/ , c /*: number*/ ) {
    Assert(a == 1)
    Assert(b == 2)
    Assert(c == 3)
}

function TestParams() {
    Params1(1)
    Params2(1, 2)
    Params3(1, 2, 3)
}

function TestFunctionOrdering() {
    let result = ForwardDeclaredFunction(1, 2, 3)
    Assert(result == 6)
}

function ForwardDeclaredFunction(a /*: number*/ , b /*: number*/ , c /*: number*/ )  /*: number*/ {
    return a + b + c
}

function TestSwitchStatements() {
    let a = 5
    switch ((a)) {
        case 1: // Fallthrough
        case 3: {
            Assert(false)
            break
        }
        case 5: {
            Assert(true)
            break
        }
        default: {
            Assert(false)
            break
        }
    }
}

function LocalPersistFunc()  /*: number*/ {
    if (typeof LocalPersistFunc.s_counter == 'undefined') {
        LocalPersistFunc.s_counter = 0
    }
    let result = LocalPersistFunc.s_counter
    LocalPersistFunc.s_counter += 1
    {
        if (typeof LocalPersistFunc.s_counterAnother == 'undefined') {
            LocalPersistFunc.s_counterAnother = 0
        }
        result += LocalPersistFunc.s_counterAnother
        LocalPersistFunc.s_counterAnother += 1
    }
    return result
}

function TestLocalPersist() {
    Assert(LocalPersistFunc() == 0)
    Assert(LocalPersistFunc() == 2)
    Assert(LocalPersistFunc() == 4)
    Assert(LocalPersistFunc() == 6)
}

function TestPrecedence() {
    let a = null
    let b = null
    let c = null
    if ((a == null || b == null || c == null)) {
        Assert(true)
    } else {
        Assert(false)
    }
    if ((a == null && b == null && c == null)) {
        Assert(true)
    } else {
        Assert(false)
    }
}

function __GlobalVariableInitializer() {
    black = 123
    glob_b = 100 + 20 + 3
    glob_c = "hello"
    glob_e = "test"
    Color.black = Color.Default()
    Color.white = Color.FromGrey(255)
    Color.green = new Color(0, 0, 255)
    glob_d = glob_c
    Color.specialMembers = [Color.black, Color.white, Color.green, new Color(0, 255, 0)]
    glob_a = TestGlobalFunctionB()
    Color.blue = Color.specialMembers[3]
    Color.yellow = new Color(0, Color.blue.b, Color.green.g)
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Postamble

__GlobalVariableInitializer()
Main()
