#include <stdio.h>

extern void adainit();
extern void adafinal();
extern int my_func(int a);

int main() {
    adainit();
    int v = my_func(2);
    printf("the result of v is: %d\n", v);
    adafinal();
}
