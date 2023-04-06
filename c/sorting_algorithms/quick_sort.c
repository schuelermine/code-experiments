#include "quick_sort.h"
#include "swap.h"
#include <stddef.h>

void quick_sort(int *arr, size_t len) {
    if (len == 0)
        return;
    int pivot_cpy = arr[len - 1];
    // partition array and calculate pivot_index
    size_t pivot_ix = 0;
    for (size_t ix = 0; ix < len - 1; ix++) {
        if (arr[ix] <= pivot_cpy) {
            pivot_ix++;
            swap(arr, pivot_ix - 1, ix);
        }
    }
    swap(arr, pivot_ix, len - 1);
    // recursive part
    quick_sort(arr, pivot_ix);
    quick_sort(arr + pivot_ix + 1, len - pivot_ix - 1);
}
