#include "printintv.h"
#include <stddef.h>
#include <stdio.h>

void printintv(int *arr, size_t len) {
    printf("{");
    if (len == 0) {
        printf("}");
        return;
    }
    for (int i = 0; i < len - 1; i++)
        printf("%i, ", arr[i]);
    printf("%i}", arr[len - 1]);
}
