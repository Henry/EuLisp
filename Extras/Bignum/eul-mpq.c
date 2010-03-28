#include "eulisp.h"
#include "gmp.h"

mpz_t *C_mpz_init();

mpq_t *C_mpq_init()
{
    mpq_t *result = (mpq_t *) gc_malloc(sizeof(mpq_t));
    mpq_init(*result);
    return (result);
}

mpq_t *C_mpq_init_set(mpq_t * a)
{
    mpq_t *result = (mpq_t *) gc_malloc(sizeof(mpq_t));
    mpq_set(*result, *a);
    return (result);
}

mpq_t *C_mpq_init_set_z(mpz_t * a)
{
    mpq_t *result = (mpq_t *) gc_malloc(sizeof(mpq_t));
    mpq_init(*result);
    mpq_set_z(*result, *a);
    return (result);
}

mpq_t *C_mpq_init_set_ui(unsigned long int i, unsigned long int j)
{
    mpq_t *result = (mpq_t *) gc_malloc(sizeof(mpq_t));
    mpq_init(*result);
    mpq_set_ui(*result, i, j);
    mpq_canonicalize(*result);
    return (result);
}

mpq_t *C_mpq_init_set_si(signed long int i, signed long int j)
{
    mpq_t *result = (mpq_t *) gc_malloc(sizeof(mpq_t));
    mpq_init(*result);
    mpq_set_si(*result, i, j);
    mpq_canonicalize(*result);
    return (result);
}

long C_mpq_clear(mpq_t * a)
{
    mpq_clear(*a);
    gc_free(a);
    return (1);
}

long C_mpq_mul(mpq_t * a, mpq_t * b, mpq_t * c)
{
    mpq_mul(*a, *b, *c);
    return (1);
}

mpq_t *C_mpq_mul_init(mpq_t * a, mpq_t * b)
{
    mpq_t *result = C_mpq_init();
    mpq_mul(*result, *a, *b);
    return (result);
}

long C_mpq_add(mpq_t * a, mpq_t * b, mpq_t * c)
{
    mpq_add(*a, *b, *c);
    return (1);
}

mpq_t *C_mpq_add_init(mpq_t * a, mpq_t * b)
{
    mpq_t *result = C_mpq_init();
    mpq_add(*result, *a, *b);
    return (result);
}

long C_mpq_sub(mpq_t * a, mpq_t * b, mpq_t * c)
{
    mpq_sub(*a, *b, *c);
    return (1);
}

mpq_t *C_mpq_sub_init(mpq_t * a, mpq_t * b)
{
    mpq_t *result = C_mpq_init();
    mpq_sub(*result, *a, *b);
    return (result);
}

long C_mpq_div(mpq_t * a, mpq_t * b, mpq_t * c)
{
    mpq_div(*a, *b, *c);
    return (1);
}

mpq_t *C_mpq_div_init(mpq_t * a, mpq_t * b)
{
    mpq_t *result = C_mpq_init();
    mpq_div(*result, *a, *b);
    return (result);
}

long C_mpq_neg(mpq_t * a, mpq_t * b)
{
    mpq_neg(*a, *b);
    return (1);
}

mpq_t *C_mpq_neg_init(mpq_t * a)
{
    mpq_t *result = C_mpq_init();
    mpq_neg(*result, *a);
    return (result);
}

long C_mpq_inv(mpq_t * a, mpq_t * b)
{
    mpq_inv(*a, *b);
    return (1);
}

mpq_t *C_mpq_inv_init(mpq_t * a)
{
    mpq_t *result = C_mpq_init();
    mpq_inv(*result, *a);
    return (result);
}

long C_mpq_cmp(mpq_t * a, mpq_t * b)
{
    return (mpq_cmp(*a, *b));
}

long C_mpq_cmp_ui(mpq_t * a, unsigned long int b, unsigned long int c)
{
    return (mpq_cmp_ui(*a, b, c));
}

long C_mpq_sgn(mpq_t * a)
{
    return (mpq_sgn(*a));
}

long C_mpq_equal(mpq_t * a, mpq_t * b)
{
    return (mpq_equal(*a, *b));
}

double C_mpq_get_d(mpq_t * a)
{
    return (mpq_get_d(*a));
}

long C_mpq_set_num(mpq_t * a, mpz_t * b)
{
    mpq_set_num(*a, *b);
    mpq_canonicalize(*a);
    return (1);
}

long C_mpq_set_den(mpq_t * a, mpz_t * b)
{
    mpq_set_den(*a, *b);
    mpq_canonicalize(*a);
    return (1);
}

mpz_t *C_mpq_get_num_init(mpq_t * a)
{
    mpz_t *result = C_mpz_init();
    mpq_get_num(*result, *a);
    return (result);
}

mpz_t *C_mpq_get_den_init(mpq_t * a)
{
    mpz_t *result = C_mpz_init();
    mpq_get_den(*result, *a);
    return (result);
}

long C_mpq_canonicalize(mpq_t * a)
{
    mpq_canonicalize(*a);
    return (1);
}


/** ----------------------------------------------------------------- **/
