#include "merge_sort.h"
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

void _merge(int *src, int *dest, size_t len, size_t mid) {
    size_t ix_l = 0;
    size_t ix_r = mid;
    for (size_t ix = 0; ix < len; ix++) {
        if (ix_l < mid && (ix_r >= len || src[ix_l] <= src[ix_r])) {
            dest[ix] = src[ix_l];
            ix_l++;
        } else {
            dest[ix] = src[ix_r];
            ix_r++;
        }
    }
}

void _merge_sort(int *arr1, int *arr2, size_t len) {
    if (len <= 1)
        return;
    size_t mid = len / 2;
    _merge_sort(arr2, arr1, mid);
    _merge_sort(arr2 + mid, arr1 + mid, len - mid);
    _merge(arr2, arr1, len, mid);
}

void merge_sort(int *arr, size_t len) {
    int *arr2 = malloc(len * sizeof(int));
    memcpy(arr2, arr, len * sizeof(int));
    _merge_sort(arr, arr2, len);
    free(arr2);
}
