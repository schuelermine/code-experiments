#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

size_t envc(char **envp) {
    size_t size = 0;
    while (true) {
        if (envp[size] == NULL) break;
        size++;
    }
    return size;
}
