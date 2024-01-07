#include "prelude.hpp"

fun void PrintNumber(longint value) {
    fprintf(stdout, "%lld\n", value);
}

fun void PrintString(char* value) {
    fprintf(stdout, "%s\n", value);
}

fun void PrintChar(char value) {
    fprintf(stdout, "%c\n", value);
}

fun void TestBasics() {
    let int a = -1 + 2 * 3 - 4 / +5;
    assert(a == 5);

    let int b = 123;
    assert(b == 123);

    let int c = a + b;

    assert(c == a + b);
    assert(c == 5 + 123);
    assert(c == 128);
    assert(c == (123 + (-1 + 2 * 3 - 4 / +5)));

    let int d = 0xfe + 1;
    assert(d == 255);
    assert(d == 0xff);

    let char ch = '\xff';
    assert(ch == (as char)255);

    let int math = 1;
    math += 1 + 2 + 2;
    assert(math == 6);
    math /= 1 + 1;
    assert(math == 3);
    math *= 1 + 1;
    assert(math == 6);
    math %= 2 + 2;
    assert(math == 2);
    math -= 2 - 1;
    assert(math == 1);

    let int ternary = 4 < 3 
        ? math
        : 3 < 3 
            ? 1 + 2 + 3
            : 5;
    assert(ternary == 5);
}

fun void TestComparisons() {
    assert(1 == 1);
    assert(1 != 2);
    assert(1 <  2);
    assert(1 <= 2);
    assert(1 >= 1);
    assert(1 <= 1);
    assert(1 >= 1);

    assert(!(1 != 1));
    assert(!(1 == 2));
    assert(!(1 >= 2));
    assert(!(1 >  2));
    assert(!(1 <  1));
    assert(!(1 >  1));
    assert(!(1 <  1));
}

fun void TestWhileLoops() {
    let int index = 0;
    assert(index == 0);
    while (index < 10) {
        index = index + 1;
        if (index == 5)
            break;
    }
    assert(index == 5);
}

fun void TestForLoops() {
    let int  i = 0; 
    let byte j = 0;
    assert(i == 0);
    assert(j == 0);
    for (i = 2;   i < 5;  i = i + 1) { }
    for (j = 120; j != 5; j = j + 1) { }
    assert(i == 5);
    assert(j == 5);
}

fun void TestIf() {
    let int i = 1;
    let int j = 2;
    if (i < j) 
        assert(1);
    else 
        assert(0);
    
    if (2 > 1) {
        assert(1);
        assert(2);
        assert(3);
    } else {
        assert(0);
    }

    if (1==0)
        assert(0);
    else
        if (1==1)
            assert(1);
        else
            assert(0);
}

struct ResultType
{
    int first;
    int second;
};
fun void EmptyFunc() {
}
fun int Foo() {
  return 100;
}
fun void EarlyReturn() {
    return;
    assert(0);
}
fun int MustNotBeCalled() {
    assert(0);
    return 1;
}
fun int ReturnsOne() {
    return 1;
}
fun int ReturnsZero() {
    return 0;
}
fun ResultType ReturnComplexResult() {
    let ResultType result;
    result.first = 1;
    result.second = 2;
    return result;
}
fun void TestFunctionCalls() {
    let int result = Foo();
    assert(result == 100);
    assert((Foo() + 100) == 200);
    EmptyFunc();
    EarlyReturn();
    let ResultType mymy = ReturnComplexResult();
    assert(mymy.first == 1);
    assert(mymy.second == 2);

    let int dummy = ReturnsOne() && ReturnsZero() && MustNotBeCalled();
    assert(dummy == 0);
}

fun void TestPointers() {
    let short  a = 123; 
    let short* b = &a; 
    let short  c = *b;
    assert(c == 123);

    let int  d = 321; 
    let int* e = &d; 
    let int  f = *e;
    assert(f == 321);

    let int* zero = nullptr;
    if (zero)
        assert(0);
    else
        assert(1);

    let int* nonzero = &d;
    if (nonzero && *nonzero > 0)
        assert(1);
    else
        assert(0);
}

let int  glob_d = 100 + 20 + 3; 
let int* glob_e;
let int  glob_f;
fun void TestGlobals() {
    glob_e = &glob_d; 
    glob_f = *glob_e; 
    assert(glob_d == 123);
    assert(glob_f == 123);
}

fun void TestPointerMath() {
    let int a = 10;
    let int b = 20;
    let int c = 30;

    let int *ap = &a;
    let int *bp = &b;
    let int *cp = &c;

    // Seems we cannot cannot assert these because of address layout changes?
    PrintNumber(cp - ap); // assert(cp - ap == 2);
    PrintNumber(cp - bp); // assert(cp - bp == 1);
    PrintNumber(bp - ap); // assert(bp - ap == 1);

    let int* cp2 = ap + 2;
    PrintNumber(*cp2); // assert(*cp2 == c);

    let int* ap2 = cp - 2;
    PrintNumber(*ap2); // assert(*ap2 == a);
}

fun void TestLValues() {
    let int a = 10;
    let int* ap = &a;
    assert(*ap == a);
    assert(*ap == 10);
    *ap = 123;
    assert(a == 123);
}

fun void TestParenthesis() {
    assert((1 + 2) * 3 == 9);
    assert(1 + 2 * 3 == 1 + (2 * 3));
}

fun int IndexArray(int* arrPointer, int index) {
    return *(arrPointer + index);
}
fun void TestArrays() {
    let char a[100];
    let int  b[100];
    let longint c[100];
    let int  d;

    b[15] = 123; 
    d = 5;
    c[10] = 5;
    d = b[d + c[10] + 5];
    assert(d == 123);

    // let int index;
    let int fill[10];
    fill[5] = 500;
    for (let int index = 0; index < 10; index = index + 1) {
        if (index == 5)
            continue;
        fill[index] = index;
    }
    assert(fill[0] == 0);
    assert(fill[1] == 1);
    assert(fill[2] == 2);
    assert(fill[3] == 3);
    assert(fill[4] == 4);
    assert(fill[5] == 500);
    assert(fill[6] == 6);
    assert(fill[7] == 7);
    assert(fill[8] == 8);
    assert(fill[9] == 9);

    let int five = 5;
    let int prefilled[] = { 0, 1, 2, 3, 4, five, 6, 7, 8, 9 };
    for (let int index = 0; index < 10; index = index + 1) {
        assert(prefilled[index] == index);
    }
    let int* prefilledPointer = prefilled;
    for (let int index = 0; index < 10; index = index + 1) {
        assert(*prefilledPointer == index);
        prefilledPointer += 1;
    }
    let int* fivePointer = &prefilled[5];
    assert(*fivePointer == 5);
    let int result = IndexArray(prefilled, 5);
    assert(result == 5);

    let char charses[] = { 'a', 'b', 'c' };
    assert(charses[0] == 'a');
    assert(charses[1] == 'b');
    assert(charses[2] == 'c');
    PrintChar(charses[0]);
    PrintChar(charses[1]);
    PrintChar(charses[2]);
}

fun void TestStrings() {
    let char* str = "Hellou" "\"uu\tworld!!\n";
    PrintString(str);

    let char* cur = str;
    assert(*cur == 'H');  cur += 1;
    assert(*cur == 'e');  cur += 1;
    assert(*cur == 'l');  cur += 1;
    assert(*cur == 'l');  cur += 1;
    assert(*cur == 'o');  cur += 1;
    assert(*cur == 'u');  cur += 1;
    assert(*cur == '"');  cur += 1;
    assert(*cur == 'u');  cur += 1;
    assert(*cur == 'u');  cur += 1;
    assert(*cur == '\t'); cur += 1;
    assert(*cur == 'w');  cur += 1;
    assert(*cur == 'o');  cur += 1;
    assert(*cur == 'r');  cur += 1;
    assert(*cur == 'l');  cur += 1;
    assert(*cur == 'd');  cur += 1;
    assert(*cur == '!');  cur += 1;
    assert(*cur == '!');

    assert(*cur == '!');  cur -= 1;
    assert(*cur == '!');  cur -= 1;
    assert(*cur == 'd');  cur -= 1;
    assert(*cur == 'l');  cur -= 1;
    assert(*cur == 'r');  cur -= 1;
    assert(*cur == 'o');  cur -= 1;
    assert(*cur == 'w');  cur -= 1;
    assert(*cur == '\t'); cur -= 1;
    assert(*cur == 'u');  cur -= 1;
    assert(*cur == 'u');  cur -= 1;
    assert(*cur == '"');  cur -= 1;
    assert(*cur == 'u');  cur -= 1;
    assert(*cur == 'o');  cur -= 1;
    assert(*cur == 'l');  cur -= 1;
    assert(*cur == 'l');  cur -= 1;
    assert(*cur == 'e');  cur -= 1;
    assert(*cur == 'H');  cur -= 1;

    let int index = 0;
    for (cur = str; *cur; cur += 1) {
        assert(*cur == str[index]);
        index += 1;
    }
}

fun void TestOperators() {
    assert(0 ^ 1 == 1);
    assert(1 << 1 == 2);
    assert(2 >> 1 == 1);
    assert(~0 == -1);
    assert(!0 == 1);
    assert(!1 == 0);
    assert((0 || 0) == 0);
    assert((0 || 1) == 1);
    assert((0 && 1) == 0);
    assert((1 && 1) == 1);
    assert((0 | 0) == 0);
    assert((0 | 1) == 1);
    assert((0 & 1) == 0);
    assert((1 & 1) == 1);
}

let int scope_test = 1;
fun void TestScopeShadowing() {
    assert(scope_test == 1);

    let int scope_test = 2;
    assert(scope_test == 2);
    {
        let int scope_test = 3;
        assert(scope_test == 3);
        if (1) {
            let int scope_test = 4;
            assert(scope_test == 4);
        } 
        assert(scope_test == 3);
    }
    assert(scope_test == 2);
}

fun void Params1(short a) {
    assert(a == 1);
}

fun void Params2(short a, int b) {
    assert(a == 1);
    assert(b == 2);
}

fun void Params3(short a, int b, longint c) {
    assert(a == 1);
    assert(b == 2);
    assert(c == 3);
}

fun void TestParams() {
    Params1(1);
    Params2(1, 2);
    Params3(1, 2, 3);
}

fun longint ForwardDeclaredFunction(short a, int b, longint c);
fun void TestForwardDeclaredFunction() {
    let longint result = ForwardDeclaredFunction(1, 2, 3);
    assert(result == 6);
}
fun longint ForwardDeclaredFunction(short a, int b, longint c) {
    return a + b + c;
}

fun void TestMultipointer() {
    let int a = 5;
    let int* ap = &a;
    let int** app = &ap;
    assert(a == 5);
    assert(*ap == 5);
    assert(**app == 5);
}

struct MyStruct; // Forward declaration
struct MyOtherStruct {
    int lala;
    MyStruct* other;
};
struct MyStruct {
    short a;
    int b;
    longint c;
    int* d;
};
struct SelfReferential {
    int value[3];
    SelfReferential* next;
    SelfReferential* previous;
};
fun void TestStructs() {
    let MyStruct my;
    my.a = 5;

    let MyStruct* myp = &my;
    assert(myp->a == 5);

    let int a = myp->a + 5;
    assert(a == 10);

    let MyOtherStruct mymy;
    mymy.other = myp;
    assert(mymy.other->a == 5);

    let int b = mymy.other->a + 5;
    assert(b == 10);

    let SelfReferential one;
    one.value[0] = 1;
    one.value[1] = 1;
    one.value[2] = 1;
    let SelfReferential two;
    two.value[0] = 2;
    two.value[1] = 2;
    two.value[2] = 2;
    let SelfReferential tre;
    tre.value[0] = 3;
    tre.value[1] = 3;
    tre.value[2] = 3;

    one.previous = nullptr;
    one.next = &two;

    two.previous = &one;
    two.next = &tre;

    tre.previous = &two;
    tre.next = nullptr;

    assert(one.value[0] == 1);
    assert(one.value[1] == 1);
    assert(one.value[2] == 1);

    assert(two.value[0] == 2);
    assert(two.value[1] == 2);
    assert(two.value[2] == 2);

    assert(tre.value[0] == 3);
    assert(tre.value[1] == 3);
    assert(tre.value[2] == 3);

    assert(one.next->value[0] == 2);
    assert(two.next->value[0] == 3);
    assert(tre.next == nullptr);

    assert(one.previous == nullptr);
    assert(two.previous->value[0] == 1);
    assert(tre.previous->value[0] == 2);

    assert(one.next->next->value[0] == 3);
    assert(tre.previous->previous->value[0] == 1);

    assert(one.next->next->previous->previous->value[0] == 1);
    assert(one.next->next->previous->previous->value[0] + 4 == 5);
}

struct MyUnion; // Forward declaration
struct MyOtherUnion {
    int lala;
    MyUnion* other;
};
struct MyUnion {
    short a;
    int b;
    longint c;
    int* d;
};
fun void TestUnions() {
    let MyUnion my;
    my.a = 5;

    let MyUnion* myp = &my;
    assert(myp->a == 5);

    let int a = myp->a + 5;
    assert(a == 10);

    let MyOtherUnion mymy;
    mymy.other = myp;
    assert(mymy.other->a == 5);

    let int b = mymy.other->a + 5;
    assert(b == 10);
}

enum class MyEnum {
    One = 1,
    Three = 3,
    Five = 5,
};

fun void EnumTest() {
    let MyEnum enummi = MyEnum::Three;
    assert(enummi == MyEnum::Three);
    assert(enummi != MyEnum::One);
    assert(enummi > MyEnum::One);
    assert(MyEnum::One < MyEnum::Three);
    assert(MyEnum::One < MyEnum::Five);
    assert(MyEnum::One < MyEnum::Five);

    let MyEnum* enummip = &enummi;
    assert(*enummip == MyEnum::Three);

    let MyEnum arr[] = { MyEnum::One, MyEnum::Three, MyEnum::Five };
    assert(arr[0] == MyEnum::One);
    assert(arr[1] == MyEnum::Three);
    assert(arr[2] == MyEnum::Five);
}

fun void TestSwitchStatements() {
    let int a = 5;
    switch (a) {
        case 1: // Fallthrough
        case 3:
            assert(0);
            break;
        case 5:
            assert(1);
            break;
        default:
            assert(0);
            break;
    }

    let MyEnum numnum = MyEnum::Five;
    switch (numnum) {
        case MyEnum::One: { {
                assert(0);
                break;
            }
        }
        case MyEnum::Three:
            assert(0);
            break;
        case MyEnum::Five:
            assert(1);
            break;
        default:
            assert(0);
            break;
    }

}

struct Shape {
    int area;
};
struct Square {
    int area;
    int width;
};

fun void TestCasting() {
    let byte a = (as byte)(127 + 4);
    assert(a = 4);

    let int b = (as int)MyEnum::Five;
    assert(b == 5);

    let Square square;
    square.area = 25;
    square.width = 5;
    let Shape* shape = nullptr;
    assert(shape == nullptr);
    shape = (as Shape*)&square;
    assert(shape->area == 25);
    assert((as Shape*)&square == shape);
}

fun void TestSizeof() {
    assert(sizeof(char) == 1);
    assert(sizeof(byte) == 1);
    assert(sizeof(short) == 2);
    assert(sizeof(int) == 4);
    assert(sizeof(longint) == 8);
    assert(sizeof(Shape) == sizeof(int));
    assert(sizeof(Square) == 2 * sizeof(int));
    assert(sizeof(Shape*) == sizeof(Square*));
}

fun int LocalPersistFunc() {
    let localpersist int s_counter = 0;
    let int result = s_counter;
    s_counter += 1;
    return result;
}

fun void TestLocalPersist() {
    assert(LocalPersistFunc() == 0);
    assert(LocalPersistFunc() == 1);
    assert(LocalPersistFunc() == 2);
    assert(LocalPersistFunc() == 3);
}

fun void TestPrecedence() {
    let int* a = nullptr;
    let int* b = nullptr;
    let int* c = nullptr;
    if (a == nullptr || b == nullptr || c == nullptr) {
        assert(1);
    } else {
        assert(0);
    }
    if (a == nullptr && b == nullptr && c == nullptr) {
        assert(1);
    } else {
        assert(0);
    }
}

fun void main() {
    TestBasics();
    TestComparisons();
    TestForLoops();
    TestWhileLoops();
    TestIf();
    TestFunctionCalls();
    TestPointers();
    TestGlobals();
    TestPointerMath();
    TestLValues();
    TestParenthesis();
    TestArrays();
    TestStrings();
    TestOperators();
    TestScopeShadowing();
    TestParams();
    TestMultipointer();
    TestStructs();
    TestUnions();
    TestSwitchStatements();
    TestCasting();
    TestSizeof();
    TestLocalPersist();

    PrintString("=====================================");
    PrintString("ALL TESTS PASSED");
    PrintString("=====================================");
}