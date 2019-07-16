#include "test.h"

int failed = 0;

int plus(int a, int b) {
    return a + b;
}

int end(int arr[5]) {
    return arr[4];
}

int calc() {
    return 5 * 9;
}

void no_return() {
}

int global_var;
int global_a = 10;
int *global_b = &global_a;
char *global_text = "Hello world";
int global_arr[3] = { 50, 41, 95 };
int global_arr2[3][3] = {
    { 6, 3 },
    { 9, 10, 32 }
};
int global_arr3[] = { 40, 23, 31, 3 };

struct Foo {
    char a;
    int b;
    int c;
};
struct Foo foo1;

typedef struct {
    int a;
} Bar;

enum {
    ONE,
    TWO,
    THREE = 10,
    FOUR
};

int main() {
    expect("zero", 0, 0);
    expect("one", 1, 1);
    expect("-1", -1, -1);
    expect("-2", -2, 0 - 2);

    expect("hex", 48879, 0xbeef);
    expect("octal", 30341, 073205);

    expect("add", 3, 1 + 2);
    expect("sub", 10, 84 - 74);
    expect("mul", 24, 3 * 8);
    expect("div", 5, 25 / 5);
    expect("div2", 5, 28 / 5);
    expect("mod", 2, 32 % 10);
    expect("precedence", 19, 4 + 5 * 3);
    expect("precedence2", 11, 5 + (7 - 5) * 3);

    expect("bit and", 128, 135 & 232);
    expect("bit or", 239, 135 | 232);
    expect("bit xor", 111, 135 ^ 232);
    expect("bit not", -54, ~53);
    expect("left shift", 18176, 71 << 8);
    expect("right shift", 12, 50 >> 2);

    int a;
    a = 5;
    expect("variable", 5, a);
    a = a * 4;
    expect("variable2", 20, a);
    if (0)
        a = 3;
    expect("if", 20, a);
    if (1)
        a = 5;
    expect("if", 5, a);

    int l = 5;
    expect("pre-increment", 5, l++);
    expect("post-increment", 7, ++l);
    expect("pre-decrement", 7, l--);
    expect("post-decrement", 5, --l);

    expect("less than", 0, 5 < 4);
    expect("less than 2", 0, 5 < 5);
    expect("greater than", 1, 6 > 5);
    expect("greater than or equal", 1, 6 >= 6);
    expect("equal", 1, 5 == 5);
    expect("not equal", 1, 5 != 4);

    int b;
    b = 0;
    while (b < 5) {
        if (b == 1) {
            b = b + 2;
            continue;
        }
        b = b + 1;
        if (b == 4) {
            break;
        }
    }
    expect("while", 4, b);
    int i;
    for (i = 0; i < 10; i = i + 1) {
    }
    expect("for", 10, i);
    int num = 0;
    for (int j = 0; j < 5; j = j + 1) {
        for (int k = 0; k < 5; k = k + 1) {}
        if (j == 4)
           break;
        else if (j == 0)
           continue;
        num = num + 5;
    }
    expect("for 2", 15, num);

    int num2 = 0;
    switch (num2) {
        case 0:
            num2 = 10;
        case 1:
            num2 = 15;
            break;
        case 2:
            num2 = 20;
            break;
    }
    expect("switch", 15, num2);
    switch (3) {
        case 1:
            num2 = 2;
        default:
            num2 = 5;
    }
    expect("switch 2", 5, num2);

    int *c;
    c = &a;
    *c = 10;
    expect("pointer", 10, a);
    int **d;
    d = &c;
    **d = 20;
    expect("double pointer", 20, a);

    int arr[5];
    arr[0] = 4;
    arr[1] = 7;
    arr[4] = 10;
    expect("array", 4, arr[0]);
    int arr3[3][3] = { 
        { 3, 5, 1 },
        { 5, 10, 41 },
        { 93, 2, 65 }
    };
    arr3[2][1] = 50;
    expect("two-dimentional array", 91, arr3[1][2] + arr3[2][1]);

    expect("pointer calculation", 7, *(arr + 1));

    expect("call", 45, plus(10, 35));
    expect("call2", 30, plus(5, 4) + plus(5, 16));
    expect("call3", 10, end(arr));
    expect("call4", 45, calc());
    expect("call5", 0, no_return());

    char ch;

    expect("sizeof", 4, sizeof(a));
    expect("sizeof pointer", 8, sizeof c);
    expect("sizeof char", 1, sizeof(ch));
    expect("sizeof array", 20, sizeof(arr));

    global_var = 14;
    expect("global variable", 14, global_var);
    expect("global_a = 10", 10, global_a);
    *global_b = 20;
    expect("global_b = &global_a", 20, global_a);
    expect("global variable of string", 101, global_text[1]);
    expect("global variable with initializer list", 95, global_arr[2]);
    expect("global variable with initializer list 2", 32, global_arr2[1][2]);
    expect("global variable with initializer list 3", 16, sizeof(global_arr3));

    char *text;
    text = "hello";
    expect("string literal", 108, text[3]);

    int e = 13;
    expect("initialization expression", 13, e);
    char *text2 = "HELLO WORLD";
    expect("initialization expression 2", 69, text2[1]);
    int arr2[5] = { 5, 3, 9, 3, 7 };
    expect("initializer list", 9, arr2[2]);
    int arr4[] = { 3, 9, 13, 51, 6 };
    expect("initializer list 2", 26, arr4[4] + sizeof(arr4));

    expect("objlike macro", 10, MACRO);
    MACRO2;
    expect("macro function", 31, ADD(23, 8));

    foo1.a = 53;
    foo1.b = 31;
    foo1.c = 98;
    expect("global struct", 182, foo1.a + foo1.b + foo1.c);
    struct Foo foo2;
    foo2.a = 35;
    foo2.b = 66;
    foo2.c = 9;
    expect("local struct", 22, foo2.b - foo2.a - foo2.c);
    struct Foo *foo3 = &foo2;
    foo3->b = 41;
    foo3->c = 94;
    expect("->", 170, foo3->a + foo2.b + foo2.c);

    typedef int i32;
    i32 f = 32;
    Bar bar;
    bar.a = 41;
    expect("typedef", 73, f + bar.a);

    short g = 61;
    expect("short", 61, g);
    long h = 214109;
    expect("long", 214109, h);

    enum {
        FIVE,
        SIX = 10,
        SEVEN = 15
    };
    expect("enum", 47, ONE + TWO + THREE + FOUR + FIVE + SIX + SEVEN);

    expect("complex calculation", 128, ((((((1+1)+(1+1))+(1+1)+(1+1))+(((1+1)+(1+1))+(1+1)+(1+1)))+((((1+1)+(1+1))+(1+1)+(1+1))+(((1+1)+(1+1))+(1+1)+(1+1))))+(((((1+1)+(1+1))+(1+1)+(1+1))+(((1+1)+(1+1))+(1+1)+(1+1)))+((((1+1)+(1+1))+(1+1)+(1+1))+(((1+1)+(1+1))+(1+1)+(1+1)))))+((((((1+1)+(1+1))+(1+1)+(1+1))+(((1+1)+(1+1))+(1+1)+(1+1)))+((((1+1)+(1+1))+(1+1)+(1+1))+(((1+1)+(1+1))+(1+1)+(1+1))))+(((((1+1)+(1+1))+(1+1)+(1+1))+(((1+1)+(1+1))+(1+1)+(1+1)))+((((1+1)+(1+1))+(1+1)+(1+1))+(((1+1)+(1+1))+(1+1)+(1+1))))));

    return failed;
}

int expect(char *name, int expected, int actual) {
    if (expected == actual) {
        printf("%s => %d\n", name, actual);
    } else {
        printf("Expected %d, but got %d\n", expected, actual);
        failed = 1;
    }
    return 0;
}
