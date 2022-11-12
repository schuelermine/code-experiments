#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include "printintv.h"
#include "swap.h"

void bubble_sort(int *array, size_t len) {
    if (len == 0)
        return;
    for (size_t j = len - 1; j >= 1; j--) {
        bool sorted = true;
        size_t i;
        for (i = 0; i < j; i++) {
            if (array[i] > array[i + 1]) {
                swap(array, i, i + 1);
                sorted = false;
            }
        }
        if (sorted)
            break;
    }
}

void insertion_sort(int *array, size_t len) {
    if (len == 0)
        return;
    int current_value_copy;
    size_t compare_index;
    for (size_t current_index = 1; current_index < len; current_index++) {
        current_value_copy = array[current_index];
        for (compare_index = current_index;
             compare_index >= 1 &&
             array[compare_index - 1] > current_value_copy;
             compare_index--) {
            array[compare_index] = array[compare_index - 1];
        }
        // compare_index instead of compare_index - 1 because the for loop
        // decrements one last time
        array[compare_index] = current_value_copy;
    }
}

void quick_sort(int *array, size_t len) {
    if (len == 0)
        return;
    int pivot_copy = array[len - 1];
    // partition array and calculate pivot_index
    size_t pivot_index = 0;
    for (size_t current_index = 0; current_index < len - 1; current_index++) {
        if (array[current_index] <= pivot_copy) {
            pivot_index++;
            swap(array, pivot_index - 1, current_index);
        }
    }
    swap(array, pivot_index, len - 1);
    // recursive part
    quick_sort(array, pivot_index);
    quick_sort(array + pivot_index + 1, len - pivot_index - 1);
}

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
