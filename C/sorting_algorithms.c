#include <stddef.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "printintv.h"

#include "swap_array_members.h"

void insertion_sort(int* array, size_t len) {
    if (len == 0) return;
    assert(len > 0);
    int current_value_copy;
    size_t compare_index;
    for (size_t current_index = 1; current_index < len; current_index++) {
        current_value_copy = array[current_index];
        for (
            compare_index = current_index;
            compare_index >= 1 && array[compare_index - 1] > current_value_copy;
            compare_index--
        ) array[compare_index] = array[compare_index - 1];
        // compare_index instead of compare_index - 1 because the for loop decrements one last time
        array[compare_index] = current_value_copy;
    }
}

void quick_sort(int* array, size_t len) {
    if (len == 0) return;
    assert(len > 0);
    int pivot_copy = array[len - 1];
    // partition array and calculate pivot_index
    size_t pivot_index = 0;
    for (int current_index = 0; current_index < len - 1; current_index++) {
        if (array[current_index] < pivot_copy) {
            swap_array_members(array, pivot_index, current_index);
            pivot_index++;
        }
    }
    swap_array_members(array, pivot_index, len - 1);
    // recursive part
    quick_sort(array, pivot_index);
    quick_sort(array + pivot_index, len - pivot_index - 1);
}

int* mk_random_array(int *len) {
    *len = rand() % 30;
    int* array = malloc(*len * sizeof(int));
    if (array == NULL) {
        perror("mk_random_array");
        exit(1);
    }
    for (int i = 0; i < *len; i++)
        array[i] = rand() % 100 - 40;
    return array;
}

void demonstrate_sort(void (*f)(int*, size_t)) {
    int len;
    int* array = mk_random_array(&len);
    printintv(array, len);
    printf("\n");
    (*f)(array, len);
    printintv(array, len);
    printf("\n");
    free(array);
}

int main(int argc, char** argv) {
    unsigned int seed;
    while (scanf("%u", &seed) <= 0);
    srand(seed);
    demonstrate_sort(&insertion_sort);
    demonstrate_sort(&quick_sort);
    return 0;
}
