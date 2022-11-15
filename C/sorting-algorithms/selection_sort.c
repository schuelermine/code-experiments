#include "swap.h"
#include "get_least_index.h"
#include "selection_sort.h"
#include <assert.h>
#include <stddef.h>

void selection_sort(int *arr, size_t len) {
    if (len == 0 || len == 1) return;
    assert(len > 1);
    size_t least_ix = get_least_index(arr, len);
    swap(arr, 0, least_ix);
    selection_sort(arr + 1, len - 1);
}
