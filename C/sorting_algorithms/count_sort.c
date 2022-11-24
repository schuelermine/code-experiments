#include "count_sort.h"
#include <assert.h>
#include <stddef.h>
#include <stdlib.h>

void count_sort(int *arr, size_t len, int min, int max) {
    assert(min <= max);
    size_t *counts = malloc((max - min + 1) * sizeof(size_t));
    for (int val = min; val <= max; val++) {
        counts[val - min] = 0;
    }
    for (size_t ix = 0; ix < len; ix++) {
        counts[arr[ix] - min]++;
    }
    size_t ix = 0;
    for (int val = min; val <= max; val++) {
        size_t count = counts[val - min];
        for (size_t i = 0; i < count; i++) {
            arr[ix] = val;
            ix++;
        }
    }
    assert(ix == len);
    free(counts);
}
