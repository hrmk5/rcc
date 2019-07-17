#define MACRO 10
#define MACRO2 expect("nested macro", 15, MACRO + 5)
#define ADD(a, b) a + b

int expect(char *name, int expected, int actual);
