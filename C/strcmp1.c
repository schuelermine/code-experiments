#include <stdbool.h>
#include <stdlib.h>

#include "strcmp.h"

int
strcmp(const char* str1, const char* str2)
{
    int diff;
    for (size_t i = 0; true; i++) {
        if (str1[i] == '\0')
            return 0;
        diff = str1[i] - str2[i];
        if (diff != 0)
            return diff;
    }
}
