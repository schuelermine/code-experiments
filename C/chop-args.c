#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include "printstrv.h"
#include "define_map.h"

define_map(string, char*, string, char*, false)

char* chop_last_char_off_arg(char* string) {
    if (string == NULL) return string;
    size_t len2 = strlen(string);
    if (len2 == 0) {
        char* string2 = malloc(1 * sizeof(char));
        string2[0] = '\0';
        return string2;
    }
    char* string2 = malloc(len2 * sizeof(char));
    for (int i = 0; i < len2 - 1; i++)
        string2[i] = string[i];
    string2[len2 - 1] = '\0';
    return string2;
}

int main(int argc, char** argv) {
    argv = map_string_string(&chop_last_char_off_arg, argv, argc + 1);
    printstrv(argv);
    for (int i = 0; i <= argc; i++)
        free(argv[i]);
    free(argv);
}
