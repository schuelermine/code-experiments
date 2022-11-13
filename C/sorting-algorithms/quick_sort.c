#include "quick_sort.h"
#include "swap.h"

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
