int failed = 0;

int expect(char *name, int expected, int actual) {
    if (expected == actual) {
        printf("%s => %d\n", name, actual);
    } else {
        printf("Expected %d, but got %d\n", expected, actual);
        failed = 1;
    }
    return 0;
}

int plus(int a, int b) {
    return a + b;
}

int end(int arr[5]) {
    return arr[4];
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

int main() {
    expect("zero", 0, 0);
    expect("one", 1, 1);
    expect("-1", -1, -1);
    expect("-2", -2, 0 - 2);

    expect("add", 3, 1 + 2);
    expect("sub", 10, 84 - 74);
    expect("mul", 24, 3 * 8);
    expect("div", 5, 25 / 5);
    expect("div2", 5, 28 / 5);
    expect("precedence", 19, 4 + 5 * 3);
    expect("precedence2", 11, 5 + (7 - 5) * 3);

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

    expect("less than", 0, 5 < 4);
    expect("less than 2", 0, 5 < 5);
    expect("greater than", 1, 6 > 5);
    expect("greater than or equal", 1, 6 >= 6);
    expect("equal", 1, 5 == 5);
    expect("not equal", 1, 5 != 4);

    int b;
    b = 0;
    while (b < 5) {
        b = b + 1;
    }
    expect("while", 5, b);
    int i;
    for (i = 0; i < 10; i = i + 1) {
    }
    expect("for", 10, i);

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

    char *text;
    text = "hello";
    expect("string literal", 108, text[3]);

    int e = 13;
    expect("initialization expression", 13, e);
    char *text2 = "HELLO WORLD";
    expect("initialization expression 2", 69, text2[1]);
    int arr2[5] = { 5, 3, 9, 3, 7 };
    expect("initializer list", 9, arr2[2]);

    expect("complex calculation", 128, ((((((1+1)+(1+1))+(1+1)+(1+1))+(((1+1)+(1+1))+(1+1)+(1+1)))+((((1+1)+(1+1))+(1+1)+(1+1))+(((1+1)+(1+1))+(1+1)+(1+1))))+(((((1+1)+(1+1))+(1+1)+(1+1))+(((1+1)+(1+1))+(1+1)+(1+1)))+((((1+1)+(1+1))+(1+1)+(1+1))+(((1+1)+(1+1))+(1+1)+(1+1)))))+((((((1+1)+(1+1))+(1+1)+(1+1))+(((1+1)+(1+1))+(1+1)+(1+1)))+((((1+1)+(1+1))+(1+1)+(1+1))+(((1+1)+(1+1))+(1+1)+(1+1))))+(((((1+1)+(1+1))+(1+1)+(1+1))+(((1+1)+(1+1))+(1+1)+(1+1)))+((((1+1)+(1+1))+(1+1)+(1+1))+(((1+1)+(1+1))+(1+1)+(1+1))))));

    return failed;
}
