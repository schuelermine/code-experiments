#include <stdlib.h>
#include <stdbool.h>

size_t envc(char **envp) {
    size_t size = 0;
    while (true) {
        if (envp[size] == NULL) break;
        size++;
    }
    return size;
}
