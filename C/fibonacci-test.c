#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

long long unsigned int positive_fibonacci_linear(unsigned int n)
{
    if (n <= 1)
        return n;

    long long unsigned int a = 0;
    long long unsigned int b = 1;
    long long unsigned int x = a + b;

    for (unsigned int i = 0; i < n - 2; i++)
    {
        a = b;
        b = x;
        x = a + b;
    }

    return x;
}

long long unsigned int positive_fibonacci_recursive_memoizing(size_t n, long long unsigned int *memoize_array,
                                                              size_t memoize_array_len)
{
    if (n <= 1)
        return n;

    bool memoize = n - 2 < memoize_array_len;

    if (memoize && memoize_array[n - 2] != 0)
        return memoize_array[n - 2];

    long long unsigned int result = positive_fibonacci_recursive_memoizing(n - 1, memoize_array, memoize_array_len) +
                                    positive_fibonacci_recursive_memoizing(n - 2, memoize_array, memoize_array_len);

    if (memoize)
        memoize_array[n - 2] = result;

    return result;
}

long long unsigned int positive_fibonacci_recursive(size_t n)
{
    if (n <= 1)
        return n;

    long long unsigned int *memoize_array = malloc(sizeof(long long unsigned int) * (n - 2));

    return positive_fibonacci_recursive_memoizing(n, memoize_array, n - 2);

    free(memoize_array);
}

int main(int argc, char **argv)
{
    if (argc != 2)
    {
        fprintf(stderr, "Invalid number of arguments passed\n");
        return 1;
    }

    unsigned int max;
    if (1 != sscanf(argv[1], "%u", &max))
    {
        fprintf(stderr, "Couldn't parse arguments\n");
        return 1;
    };

    for (unsigned int i = 0; i <= max; i++)
        printf("fibonacci(%u) = %llu\n", i, positive_fibonacci_recursive(i));
}
