#define MACRO 10
#define MACRO2 expect("nested macro", 15, MACRO + 5)
#define ADD(a, b) a + b

extern int external_var;
extern int failed;

int expect(char *name, int expected, int actual);
void expect_f(char *name, float expected, float actual);
void expect_d(char *name, double expected, double actual);
