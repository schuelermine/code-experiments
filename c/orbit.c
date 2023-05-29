#include <complex.h>
#include <errno.h>
#include <inttypes.h>
#include <math.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdlib.h>

#define ESCAPE_NORM 4
#define ROUND MPFR_RNDN

size_t orbit(const mpfr_t c_re, const mpfr_t c_im, size_t M,
             double complex *output, mpfr_prec_t prec, mpfr_rnd_t round,
             void (*callback)(size_t)) {
    mpfr_t z_re, z_im;
    mpfr_inits2(prec, z_re, z_im, NULL);
    mpfr_set_ui(z_re, 0, round);
    mpfr_set_ui(z_im, 0, round);
    mpfr_t re_tmp, im_tmp;
    mpfr_inits2(prec, re_tmp, im_tmp, NULL);
    double interval_d = ceil(sqrt((double)M));
    size_t interval;
    if (interval_d < 1) {
        interval = 1;
    } else {
        interval = (size_t)interval_d;
    }
    output[0] = 0;
    size_t m = 1;
    for (; m < M; m++) {
        mpfr_sqr(re_tmp, z_re, round);
        mpfr_sqr(im_tmp, z_im, round);
        mpfr_sub(im_tmp, im_tmp, c_re, round);
        mpfr_sub(re_tmp, re_tmp, im_tmp, round);
        mpfr_mul(z_im, z_re, z_im, round);
        mpfr_mul_ui(z_im, z_im, 2, round);
        mpfr_add(z_im, z_im, c_im, round);
        mpfr_set(z_re, re_tmp, round);
        output[m] = mpfr_get_d(z_re, round) + mpfr_get_d(z_im, round) * I;
        mpfr_sqr(re_tmp, z_re, round);
        mpfr_sqr(im_tmp, z_im, round);
        mpfr_add(re_tmp, re_tmp, im_tmp, round);
        if (mpfr_cmp_ui(re_tmp, ESCAPE_NORM) > 0)
            break;
        if (m % interval == 0) {
            (*callback)(m);
        }
    }
    mpfr_clears(re_tmp, im_tmp, z_re, z_im, NULL);
    return m;
}

int digitsiz(size_t value) {
    int count = 0;
    while (value >= 10) {
        value /= 10;
        count++;
    }
    return count;
}

void progress(size_t m) { fprintf(stderr, "computed iteration %zu\n", m); }

int main(int argc, char **argv) {
    if (argc != 5) {
        fprintf(stderr, "wrong argument count %d, should be 4\n", argc - 1);
        fprintf(
            stderr,
            "usage: %s <Re[c]> <Im[c]> <precision> <maximum iteration count>\n",
            argv[0]);
        exit(-5);
    }
    char *endptr;
    errno = 0;
    signed long prec_l = strtol(argv[3], &endptr, 10);
    if (argv[3][0] == '\0' || *endptr != '\0') {
        fprintf(stderr, "failed to parse argument precision as a number\n");
        exit(-3);
    }
    if (errno == ERANGE) {
        fprintf(stderr, "argument precision is too large\n");
        exit(-3);
    }
    if (prec_l <= 0) {
        fprintf(stderr, "argument precision must be positive\n");
        exit(-3);
    }
    mpfr_prec_t prec = (mpfr_prec_t)prec_l;
    errno = 0;
    uintmax_t iter_umax = strtoumax(argv[4], &endptr, 10);
    if (argv[4][0] == '\0' || *endptr != '\0') {
        fprintf(
            stderr,
            "failed to parse argument maximum iteration count as a number\n");
        exit(-4);
    }
    if (iter_umax > (uintmax_t)SIZE_MAX || errno == ERANGE) {
        fprintf(stderr, "argument maximum iteration count is too large\n");
        exit(-4);
    }
    size_t iter = (size_t)iter_umax;
    mpfr_t c_re;
    mpfr_t c_im;
    mpfr_inits2(prec, c_re, c_im, NULL);
    if (mpfr_set_str(c_re, argv[1], 10, ROUND) != 0) {
        fprintf(stderr, "failed to parse argument Re[c] as a number\n");
        mpfr_clears(c_re, c_im, NULL);
        exit(-4);
    };
    if (mpfr_set_str(c_im, argv[2], 10, ROUND) != 0) {
        fprintf(stderr, "failed to parse argument Im[c] as a number\n");
        mpfr_clears(c_re, c_im, NULL);
        exit(-5);
    }
    double complex *result = malloc(iter * sizeof(double complex));
    if (result == NULL) {
        perror("orbit array allocation");
        mpfr_clears(c_re, c_im, NULL);
        exit(-6);
    }
    size_t m = orbit(c_re, c_im, iter, result, prec, ROUND, progress);
    if (m == SIZE_MAX) {
        printf("iterations = max");
    } else {
        printf("iterations = %zu\n", m + 1);
    }
    int width = digitsiz(m);
    for (size_t i = 0; i < m; i++) {
        printf("orbit[%0*zu] = %13.6e + %13.6ei\n", width, i, creal(result[i]),
               cimag(result[i]));
    }
    mpfr_clears(c_re, c_im, NULL);
    free(result);
}
