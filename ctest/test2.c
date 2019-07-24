#include "ctest/test.h"

extern void printf(const char *format, double a);
extern void putchar(int c);

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

void expect_d(const char *name, double expected, double actual) {
    if (expected == actual) {
        for (int i = 0; name[i] != 0; i++) {
            putchar(name[i]);
        }
        printf(" => ", 0);
        printf("%lf\n", actual);
    } else {
        printf("Expected %lf, ", expected);
        printf("but got %lf\n", actual);
        failed = 1;
    }
}
