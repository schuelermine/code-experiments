#include "swap.h"

inline void swap(int *array, int ix1, int ix2) {
    int temp = array[ix1];
    array[ix1] = array[ix2];
    array[ix2] = temp;
}
