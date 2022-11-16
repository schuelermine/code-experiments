#include "get_least_index.h"
#include <stddef.h>

inline size_t get_least_index(int *arr, size_t len) {
    size_t least_ix = 0;
    for (size_t ix = 1; ix < len; ix++) {
        if (arr[ix] < arr[least_ix]) {
            least_ix = ix;
        }
    }
    return least_ix;
}
