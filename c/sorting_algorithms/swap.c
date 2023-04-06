#include "swap.h"

inline void swap(int *arr, int ix1, int ix2) {
    int temp = arr[ix1];
    arr[ix1] = arr[ix2];
    arr[ix2] = temp;
}
