#include "insertion_sort.h"
#include <stddef.h>

void insertion_sort(int *arr, size_t len) {
    if (len == 0)
        return;
    int val_cpy;
    size_t cmp_ix;
    for (size_t ix = 1; ix < len; ix++) {
        val_cpy = arr[ix];
        for (cmp_ix = ix;
             cmp_ix >= 1 &&
             arr[cmp_ix - 1] > val_cpy;
             cmp_ix--) {
            arr[cmp_ix] = arr[cmp_ix - 1];
        }
        // compare_index instead of compare_index - 1 because the for loop
        // decrements one last time
        arr[cmp_ix] = val_cpy;
    }
}
