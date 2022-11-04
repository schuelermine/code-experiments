#include "swap_array_members.h"

inline void swap_array_members(int* array, int index1, int index2) {
    int temp = array[index1];
    array[index1] = array[index2];
    array[index2] = temp;
}
