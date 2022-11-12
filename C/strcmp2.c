#include <stdbool.h>
#include <stdlib.h>

#include "strcmp.h"

int strcmp(const char *str1, const char *str2) {
    char c1 = str1[0];
    char c2 = str2[0];
    while (c1 == c2) {
        c1 = *++str1;
        c2 = *++str2;
        if (c1 == '\0')
            break;
    }
    return c1 - c2;
}
