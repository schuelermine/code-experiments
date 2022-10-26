#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

size_t envc(char **envp) {
    size_t size = 0;
    while (true) {
        if (envp[size] == NULL) break;
        size++;
    }
    return size;
}

int main(int argc, char** argv, char** envp) {
    printf("%lld\n", (long long int) envc(envp));
}
