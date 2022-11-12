#include "printstrv.h"

#include <stddef.h>
#include <stdio.h>

void
printstrv (char **array)
{
    printf ("{");
    while (*array != NULL)
        {
            printf ("\"%s\"", *array);
            array++;
            if (*array != NULL)
                printf (", ");
        }
    printf ("}");
}
