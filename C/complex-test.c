#include <complex.h>
#include <stdio.h>

int
main ()
{
    complex float x = cpow (2.3 + 3.2 * I, 3.1 + 2.4 * I);
    printf ("%f + %f i", creal (x), cimag (x));
}
