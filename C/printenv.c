#include <stddef.h>
#include <stdio.h>

#include "define_foreach.h"
#include "envc.h"

define_foreach(string, char*)

    void println(char* string) {
    printf("%s\n", string);
}

int main(int argc, char** argv, char** envp) {
    foreach_string(&println, envp, envc(envp));
}
