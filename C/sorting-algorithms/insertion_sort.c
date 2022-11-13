#include "insertion_sort.h"
#include <stddef.h>

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
