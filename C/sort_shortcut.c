#include <assert.h>
#include <stddef.h>

inline void swap(int *arr, int ix1, int ix2) {
    int temp = arr[ix1];
    arr[ix1] = arr[ix2];
    arr[ix2] = temp;
}

void sort_shortcut(void (*f)(int *, size_t), int *arr, size_t len) {
    assert(f != NULL);
    if (len == 2) {
        if (arr[0] > arr[1])
            swap(arr, 0, 1);
        return;
    }
    if (len <= 1)
        return;
    (*f)(arr, len);
}
