#include "bubble_sort.h"
#include "count_sort.h"
#include "insertion_sort.h"
#include "merge_sort.h"
#include "printintv.h"
#include "quick_sort.h"
#include "selection_sort.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

const int random_value_spread = 100;
const int random_value_min = -40;
const int count_sort_min = random_value_min;
const int count_sort_max = random_value_spread - 1 + random_value_spread;
const int max_array_size = 30;

void count_sort_(int *array, size_t len) {
    count_sort(array, len, count_sort_min, count_sort_max);
}

int *mk_random_array(size_t *len) {
    *len = rand() % max_array_size;
    if (*len == 0) {
        return malloc(0);
    }
    int *array = malloc(*len * sizeof(int));
    if (array == NULL) {
        perror("mk_random_array");
        exit(1);
    }
    for (size_t i = 0; i < *len; i++) {
        array[i] = rand() % random_value_spread + random_value_min;
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

void _demonstrate_sort(void (*f)(int *, size_t), char *name) {
    printf("Demonstrating %s\n", name);
    size_t len;
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

#define demonstrate_sort(name) _demonstrate_sort(&name, #name)

int main(int argc, char **argv) {
    unsigned int seed;
    while (scanf("%u", &seed) <= 0) {
    };
    srand(seed);
    demonstrate_sort(bubble_sort);
    demonstrate_sort(insertion_sort);
    demonstrate_sort(quick_sort);
    _demonstrate_sort(&count_sort_, "count_sort");
    demonstrate_sort(selection_sort);
    demonstrate_sort(merge_sort);
    return 0;
}
