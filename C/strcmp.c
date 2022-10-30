#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

int strcmp(const char *str1, const char *str2) {
    int diff;
    for (size_t i = 0; true; i++) {
        if (str1[i] == '\0' && str2[i] == '\0')
            return 0;
        diff = str1[i] - str2[i];
        if (diff != 0) return diff;
    }
}

int main(int argc, char** argv, char** envp) {
    if (argc != 3) {
        fprintf(stderr, "Invalid number of arguments!\n");
        exit(22);
    }

    printf("%d", strcmp(argv[1], argv[2]));
}
