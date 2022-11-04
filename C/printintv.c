#include <stddef.h>
#include <stdio.h>
#include "printintv.h"

void printintv(int* array, size_t len) {
    printf("{");
    for (int i = 0; i < len - 1; i++)
        printf("%i, ", array[i]);
    printf("%i}", array[len - 1]);
}
