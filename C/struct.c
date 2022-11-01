#include <stddef.h>
#include <stdlib.h>
#include "printstrv.h"

struct MyStruct {
    int my_integer;
    char* my_string;
    char** my_string_array;
};

// Allocates 3*char
struct MyStruct mk_my_struct() {
    char** my_string_array = malloc((2 + 1) * sizeof(char*));
    my_string_array[0] = "3";
    my_string_array[1] = "4";
    my_string_array[2] = NULL;
    struct MyStruct my_struct = {
        1,
        "2",
        my_string_array
    };
    return my_struct;
}

int main() {
    struct MyStruct my_struct = mk_my_struct();
    printstrv(my_struct.my_string_array);
    free(my_struct.my_string_array);
}
