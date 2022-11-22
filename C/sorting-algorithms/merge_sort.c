#include "merge_sort.h"
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

void _merge(int *arr, int *work_arr, size_t len, size_t mid) {
    size_t ix_l = 0;
    size_t ix_r = mid;
    for (size_t ix = 0; ix < len; ix++) {
        if (ix_l < mid && (ix_r >= len || work_arr[ix_l] <= work_arr[ix_r])) {
            arr[ix] = work_arr[ix_l];
            ix_l++;
        } else {
            arr[ix] = work_arr[ix_r];
            ix_r++;
        }
    }
}

void _merge_sort(int *arr, int *work_arr, size_t len) {
    if (len <= 1)
        return;
    size_t mid = len / 2;
    _merge_sort(arr, work_arr, mid);
    _merge_sort(arr + mid, work_arr + mid, len - mid);
    _merge(arr, work_arr, len, mid);
}

void merge_sort(int *arr, size_t len) {
    int *work_arr = malloc(len * sizeof(int));
    memcpy(work_arr, arr, len * sizeof(int));
    _merge_sort(arr, work_arr, len);
}
