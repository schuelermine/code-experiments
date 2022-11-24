#include "bubble_sort.h"
#include "swap.h"
#include <stdbool.h>
#include <stddef.h>

void bubble_sort(int *arr, size_t len) {
    if (len == 0)
        return;
    for (size_t j = len - 1; j >= 1; j--) {
        bool sorted = true;
        size_t i;
        for (i = 0; i < j; i++) {
            if (arr[i] > arr[i + 1]) {
                swap(arr, i, i + 1);
                sorted = false;
            }
        }
        if (sorted)
            break;
    }
}
