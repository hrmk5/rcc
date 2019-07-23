#include "ctest/test.h"

extern void printf(const char *format, float a);
extern void putchar(int c);
extern void puts(const char *s);

int external_var = 143;

void expect_f(const char *name, float expected, float actual) {
    if (expected == actual) {
        for (int i = 0; name[i] != 0; i++) {
            putchar(name[i]);
        }
        printf(" => ", 0);
        printf("%f\n", actual);
    } else {
        printf("Expected %f, ", expected);
        printf("but got %f\n", actual);
        failed = 1;
    }
}
