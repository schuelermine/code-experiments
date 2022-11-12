#include "bubble_sort.h"
#include "insertion_sort.h"
#include "printintv.h"
#include "quick_sort.h"
#include "swap.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

int *mk_random_array(int *len) {
    *len = rand() % 30;
    if (*len == 0) {
        return malloc(0);
    }
    int *array = malloc(*len * sizeof(int));
    if (array == NULL) {
        perror("mk_random_array");
        exit(1);
    }
    for (int i = 0; i < *len; i++) {
        array[i] = rand() % 100 - 40;
    }
    return array;
}

bool is_sorted(int *array, size_t len) {
    bool sorted = true;
    for (size_t i = 1; i < len; i++) {
        if (array[i - 1] > array[i]) {
            sorted = false;
        }
    }
    return sorted;
}

void _demonstrate_sort_in_place(void (*f)(int *, size_t), char *name) {
    printf("Demonstrating %s\n", name);
    int len;
    int *array = mk_random_array(&len);
    printintv(array, len);
    printf("\n");
    (*f)(array, len);
    printintv(array, len);
    printf("\n");
    if (is_sorted(array, len)) {
        printf("correctly sorted!\n");
    } else {
        printf("incorrectly sorted.\n");
    }
    free(array);
}

#define demonstrate_sort_in_place(name) _demonstrate_sort_in_place(&name, #name)

int main(int argc, char **argv) {
    unsigned int seed;
    while (scanf("%u", &seed) <= 0) {
    };
    srand(seed);
    demonstrate_sort_in_place(bubble_sort);
    demonstrate_sort_in_place(insertion_sort);
    demonstrate_sort_in_place(quick_sort);
    return 0;
}
