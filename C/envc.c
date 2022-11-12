#include "envc.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

size_t envc(char **envp) {
    size_t size = 0;
    while (true) {
        if (envp[size] == NULL)
            break;
        size++;
    }
    return size;
}
