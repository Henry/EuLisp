/// Copyright 1997 A. Kind & University of Bath
/// Copyright 2010 Henry G. Weller
///-----------------------------------------------------------------------------
//  This file is part of
/// ---                         EuLisp System 'Youtoo'
///-----------------------------------------------------------------------------
//
//  Youtoo is free software: you can redistribute it and/or modify it under the
//  terms of the GNU General Public License version 2 as published by the Free
//  Software Foundation.
//
//  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
//  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
//  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
//  details.
//
//  You should have received a copy of the GNU General Public License along with
//  this program.  If not, see <http://www.gnu.org/licenses/>.
//
///-----------------------------------------------------------------------------
/// Title: Wrapper for GNU mpz library
///  Library: bignum
///  Authors: Danius Michaelides, Andreas Kind
///  Maintainer: Henry G. Weller
///  Compilation
//    see Makefile
///-----------------------------------------------------------------------------
#include "eulisp.h"
#include "gmp.h"

mpz_t *C_mpz_init()
{
    mpz_t *result = (mpz_t *) gc_malloc(sizeof(mpz_t));
    mpz_init(*result);
    return (result);
}

mpz_t *C_mpz_init_set(mpz_t * a)
{
    mpz_t *result = (mpz_t *) gc_malloc(sizeof(mpz_t));
    mpz_init_set(*result, *a);
    return (result);
}

mpz_t *C_mpz_init_set_si(int i)
{
    mpz_t *result = (mpz_t *) gc_malloc(sizeof(mpz_t));
    mpz_init_set_si(*result, i);
    return (result);
}

mpz_t *C_mpz_init_set_str(char *s, int base)
{
    mpz_t *result = (mpz_t *) gc_malloc(sizeof(mpz_t));
    mpz_init_set_str(*result, s, base);
    return (result);
}

mpz_t *C_mpz_init_set_d(double d)
{
    mpz_t *result = (mpz_t *) gc_malloc(sizeof(mpz_t));
    mpz_init_set_d(*result, d);
    return (result);
}

long C_mpz_clear(mpz_t * a)
{
    mpz_clear(*a);
    gc_free(a);
    return (1);
}

signed long int C_mpz_get_si(mpz_t * a)
{
    return (mpz_get_si(*a));
}

double C_mpz_get_d(mpz_t * a)
{
    return (mpz_get_d(*a));
}

char *C_mpz_get_str(int base, mpz_t * a)
{
    int len = mpz_sizeinbase(*a, base) + 2;
    char *result = gc_malloc(len);
    mpz_get_str(result, base, *a);
    return (result);
}

long C_mpz_out_str(int bas, mpz_t * a)
{
    mpz_out_str(NULL, bas, *a);
    return (1);
}

long C_mpz_mul(mpz_t * a, mpz_t * b, mpz_t * c)
{
    mpz_mul(*a, *b, *c);
    return (1);
}

mpz_t *C_mpz_mul_init(mpz_t * a, mpz_t * b)
{
    mpz_t *result = C_mpz_init();
    mpz_mul(*result, *a, *b);
    return (result);
}

long C_mpz_mul_ui(mpz_t * a, mpz_t * b, unsigned long int c)
{
    mpz_mul_ui(*a, *b, c);
    return (1);
}

mpz_t *C_mpz_mul_ui_init(mpz_t * a, unsigned long int b)
{
    mpz_t *result = C_mpz_init();

    if (((signed int)b) < 0)
    {
        mpz_t *tmp = C_mpz_init();

        mpz_mul_ui(*tmp, *a, (unsigned long int)(0 - b));
        mpz_neg(*result, *tmp);
    }
    else
    {
        mpz_mul_ui(*result, *a, b);
    }

    return (result);
}

long C_mpz_add(mpz_t * a, mpz_t * b, mpz_t * c)
{
    mpz_add(*a, *b, *c);
    return (1);
}

mpz_t *C_mpz_add_init(mpz_t * a, mpz_t * b)
{
    mpz_t *result = C_mpz_init();
    mpz_add(*result, *a, *b);
    return (result);
}

mpz_t *C_mpz_add_ui_init(mpz_t * a, unsigned long int b)
{
    mpz_t *result = C_mpz_init();

    if (((signed int)b) < 0)
    {
        mpz_sub_ui(*result, *a, (unsigned long int)(0 - b));
    }
    else
    {
        mpz_add_ui(*result, *a, b);
    }

    return (result);
}

long C_mpz_sub(mpz_t * a, mpz_t * b, mpz_t * c)
{
    mpz_sub(*a, *b, *c);
    return (1);
}

mpz_t *C_mpz_sub_init(mpz_t * a, mpz_t * b)
{
    mpz_t *result = C_mpz_init();
    mpz_sub(*result, *a, *b);
    return (result);
}

long C_mpz_sub_ui(mpz_t * a, mpz_t * b, unsigned long int c)
{
    mpz_sub_ui(*a, *b, c);
    return (1);
}

mpz_t *C_mpz_sub_ui_init(mpz_t * a, unsigned long int b)
{
    mpz_t *result = C_mpz_init();

    if (((signed int)b) < 0)
    {
        mpz_add_ui(*result, *a, (unsigned long int)(0 - b));
    }
    else
    {
        mpz_sub_ui(*result, *a, b);
    }

    return (result);
}


long C_mpz_mul_2exp(mpz_t * a, mpz_t * b, unsigned long int c)
{
    mpz_mul_2exp(*a, *b, c);
    return (1);
}

mpz_t *C_mpz_mul_2exp_init(mpz_t * a, unsigned long int b)
{
    mpz_t *result = C_mpz_init();
    mpz_mul_2exp(*result, *a, b);
    return (result);
}

long C_mpz_neg(mpz_t * a, mpz_t * b)
{
    mpz_neg(*a, *b);
    return (1);
}

mpz_t *C_mpz_neg_init(mpz_t * a)
{
    mpz_t *result = C_mpz_init();
    mpz_neg(*result, *a);
    return (result);
}

long C_mpz_abs(mpz_t * a, mpz_t * b)
{
    mpz_abs(*a, *b);
    return (1);
}

mpz_t *C_mpz_abs_init(mpz_t * a)
{
    mpz_t *result = C_mpz_init();
    mpz_abs(*result, *a);
    return (result);
}

long C_mpz_fac_ui(mpz_t * a, unsigned long int b)
{
    mpz_fac_ui(*a, b);
    return (1);
}

mpz_t *C_mpz_fac_ui_init(unsigned long int a)
{
    mpz_t *result = C_mpz_init();
    mpz_fac_ui(*result, a);
    return (result);
}


long C_mpz_tdiv_q(mpz_t * a, mpz_t * b, mpz_t * c)
{
    mpz_tdiv_q(*a, *b, *c);
    return (1);
}

mpz_t *C_mpz_tdiv_q_init(mpz_t * a, mpz_t * b)
{
    mpz_t *result = C_mpz_init();
    mpz_tdiv_q(*result, *a, *b);
    return (result);
}

long C_mpz_tdiv_q_ui(mpz_t * a, mpz_t * b, unsigned long int c)
{
    mpz_tdiv_q_ui(*a, *b, c);
    return (1);
}

mpz_t *C_mpz_tdiv_q_ui_init(mpz_t * a, unsigned long int b)
{
    mpz_t *result = C_mpz_init();
    mpz_tdiv_q_ui(*result, *a, b);
    return (result);
}


long C_mpz_tdiv_r(mpz_t * a, mpz_t * b, mpz_t * c)
{
    mpz_tdiv_r(*a, *b, *c);
    return (1);
}

mpz_t *C_mpz_tdiv_r_init(mpz_t * a, mpz_t * b)
{
    mpz_t *result = C_mpz_init();
    mpz_tdiv_r(*result, *a, *b);
    return (result);
}

long C_mpz_tdiv_r_ui(mpz_t * a, mpz_t * b, unsigned long int c)
{
    mpz_tdiv_r_ui(*a, *b, c);
    return (1);
}

mpz_t *C_mpz_tdiv_r_ui_init(mpz_t * a, unsigned long int b)
{
    mpz_t *result = C_mpz_init();
    mpz_tdiv_r_ui(*result, *a, b);
    return (result);
}

long C_mpz_tdiv_qr(mpz_t * a, mpz_t * b, mpz_t * c, mpz_t * d)
{
    mpz_tdiv_qr(*a, *b, *c, *d);
    return (1);
}

long C_mpz_tdiv_qr_ui(mpz_t * a, mpz_t * b, mpz_t * c, unsigned long int d)
{
    mpz_tdiv_qr_ui(*a, *b, *c, d);
    return (1);
}

long C_mpz_fdiv_q(mpz_t * a, mpz_t * b, mpz_t * c)
{
    mpz_fdiv_q(*a, *b, *c);
    return (1);
}

mpz_t *C_mpz_fdiv_q_init(mpz_t * a, mpz_t * b)
{
    mpz_t *result = C_mpz_init();
    mpz_fdiv_q(*result, *a, *b);
    return (result);
}

long C_mpz_fdiv_q_ui(mpz_t * a, mpz_t * b, unsigned long int c)
{
    mpz_fdiv_q_ui(*a, *b, c);
    return (1);
}

mpz_t *C_mpz_fdiv_q_ui_init(mpz_t * a, unsigned long int b)
{
    mpz_t *result = C_mpz_init();
    mpz_fdiv_q_ui(*result, *a, b);
    return (result);
}

long C_mpz_fdiv_r(mpz_t * a, mpz_t * b, mpz_t * c)
{
    mpz_fdiv_r(*a, *b, *c);
    return (1);
}

mpz_t *C_mpz_fdiv_r_init(mpz_t * a, mpz_t * b)
{
    mpz_t *result = C_mpz_init();
    mpz_fdiv_r(*result, *a, *b);
    return (result);
}

unsigned long int C_mpz_fdiv_r_ui(mpz_t * a, mpz_t * b, unsigned long int c)
{
    return (mpz_fdiv_r_ui(*a, *b, c));
}

long C_mpz_fdiv_qr(mpz_t * a, mpz_t * b, mpz_t * c, mpz_t * d)
{
    mpz_fdiv_qr(*a, *b, *c, *d);
    return (1);
}

unsigned long int C_mpz_fdiv_qr_ui
(
    mpz_t *a,
    mpz_t *b,
    mpz_t *c,
    unsigned long int d
)
{
    return (mpz_fdiv_qr_ui(*a, *b, *c, d));
}

unsigned long int C_mpz_fdiv_ui(mpz_t * a, unsigned long int b)
{
    return (mpz_fdiv_ui(*a, b));
}

long C_mpz_cdiv_q(mpz_t * a, mpz_t * b, mpz_t * c)
{
    mpz_cdiv_q(*a, *b, *c);
    return (1);
}

mpz_t *C_mpz_cdiv_q_init(mpz_t * a, mpz_t * b)
{
    mpz_t *result = C_mpz_init();

    mpz_cdiv_q(*result, *a, *b);
    return (result);
}

long C_mpz_cdiv_q_ui(mpz_t * a, mpz_t * b, unsigned long int c)
{
    mpz_cdiv_q_ui(*a, *b, c);
    return (1);
}

mpz_t *C_mpz_cdiv_q_ui_init(mpz_t * a, unsigned long int b)
{
    mpz_t *result = C_mpz_init();

    mpz_cdiv_q_ui(*result, *a, b);
    return (result);
}

long C_mpz_cdiv_r(mpz_t * a, mpz_t * b, mpz_t * c)
{
    mpz_cdiv_r(*a, *b, *c);
    return (1);
}

mpz_t *C_mpz_cdiv_r_init(mpz_t * a, mpz_t * b)
{
    mpz_t *result = C_mpz_init();

    mpz_cdiv_r(*result, *a, *b);
    return (result);
}

unsigned long int C_mpz_cdiv_r_ui(mpz_t * a, mpz_t * b, unsigned long int c)
{
    return (mpz_cdiv_r_ui(*a, *b, c));
}


long C_mpz_cdiv_qr(mpz_t * a, mpz_t * b, mpz_t * c, mpz_t * d)
{
    mpz_cdiv_qr(*a, *b, *c, *d);
    return (1);
}

unsigned long int C_mpz_cdiv_qr_ui(mpz_t * a, mpz_t * b, mpz_t * c,
    unsigned long int d)
{
    return (mpz_cdiv_qr_ui(*a, *b, *c, d));
}

unsigned long int C_mpz_cdiv_ui(mpz_t * a, unsigned long int b)
{
    return (mpz_cdiv_ui(*a, b));
}

long C_mpz_mod(mpz_t * a, mpz_t * b, mpz_t * c)
{
    mpz_mod(*a, *b, *c);
    return (1);
}

mpz_t *C_mpz_mod_init(mpz_t * a, mpz_t * b)
{
    mpz_t *result = C_mpz_init();

    mpz_mod(*result, *a, *b);
    return (result);
}

long C_mpz_mod_ui(mpz_t * a, mpz_t * b, unsigned long int c)
{
    mpz_mod_ui(*a, *b, c);
    return (1);
}

long C_mpz_divexact(mpz_t * a, mpz_t * b, mpz_t * c)
{
    mpz_divexact(*a, *b, *c);
    return (1);
}

mpz_t *C_mpz_divexact_init(mpz_t * a, mpz_t * b)
{
    mpz_t *result = C_mpz_init();
    mpz_divexact(*result, *a, *b);
    return (result);
}

long C_mpz_tdiv_q_2exp(mpz_t * a, mpz_t * b, unsigned long int c)
{
    mpz_tdiv_q_2exp(*a, *b, c);
    return (1);
}

mpz_t *C_mpz_tdiv_q_2exp_init(mpz_t * a, unsigned long int b)
{
    mpz_t *result = C_mpz_init();
    mpz_tdiv_q_2exp(*result, *a, b);
    return (result);
}

long C_mpz_tdiv_r_2exp(mpz_t * a, mpz_t * b, unsigned long int c)
{
    mpz_tdiv_r_2exp(*a, *b, c);
    return (1);
}

mpz_t *C_mpz_tdiv_r_2exp_init(mpz_t * a, unsigned long int b)
{
    mpz_t *result = C_mpz_init();

    mpz_tdiv_r_2exp(*result, *a, b);
    return (result);
}

long C_mpz_fdiv_q_2exp(mpz_t * a, mpz_t * b, unsigned long int c)
{
    mpz_fdiv_q_2exp(*a, *b, c);
    return (1);
}

mpz_t *C_mpz_fdiv_q_2exp_init(mpz_t * a, unsigned long int b)
{
    mpz_t *result = C_mpz_init();
    mpz_fdiv_q_2exp(*result, *a, b);
    return (result);
}

long C_mpz_fdiv_r_2exp(mpz_t * a, mpz_t * b, unsigned long int c)
{
    mpz_fdiv_r_2exp(*a, *b, c);
    return (1);
}

mpz_t *C_mpz_fdiv_r_2exp_init(mpz_t * a, unsigned long int b)
{
    mpz_t *result = C_mpz_init();

    mpz_fdiv_r_2exp(*result, *a, b);
    return (result);
}

long C_mpz_powm(mpz_t * a, mpz_t * b, mpz_t * c, mpz_t * d)
{
    mpz_powm(*a, *b, *c, *d);
    return (1);
}

mpz_t *C_mpz_powm_init(mpz_t * a, mpz_t * b, mpz_t * c)
{
    mpz_t *result = C_mpz_init();

    mpz_powm(*result, *a, *b, *c);
    return (result);
}

long C_mpz_powm_ui(mpz_t * a, mpz_t * b, unsigned c, mpz_t * d)
{
    mpz_powm_ui(*a, *b, c, *d);
    return (1);
}

mpz_t *C_mpz_powm_ui_init(mpz_t * a, unsigned long int b, mpz_t * c)
{
    mpz_t *result = C_mpz_init();
    mpz_powm_ui(*result, *a, b, *c);
    return (result);
}

long C_mpz_pow_ui(mpz_t * a, mpz_t * b, unsigned c)
{
    mpz_pow_ui(*a, *b, c);
    return (1);
}

mpz_t *C_mpz_pow_ui_init(mpz_t * a, unsigned long int b)
{
    mpz_t *result = C_mpz_init();
    mpz_pow_ui(*result, *a, b);
    return (result);
}

long C_mpz_ui_pow_ui(mpz_t * a, unsigned long int b, unsigned c)
{
    mpz_ui_pow_ui(*a, b, c);
    return (1);
}

mpz_t *C_mpz_ui_pow_ui_init(unsigned long int a, unsigned long int b)
{
    mpz_t *result = C_mpz_init();
    mpz_ui_pow_ui(*result, a, b);
    return (result);
}

long C_mpz_sqrt(mpz_t * a, mpz_t * b)
{
    mpz_sqrt(*a, *b);
    return (1);
}

mpz_t *C_mpz_sqrt_init(mpz_t * a)
{
    mpz_t *result = C_mpz_init();
    mpz_sqrt(*result, *a);
    return (result);
}

long C_mpz_sqrtrem(mpz_t * a, mpz_t * b, mpz_t * c)
{
    mpz_sqrtrem(*a, *b, *c);
    return (1);
}

long C_mpz_perfect_square_p(mpz_t * a)
{
    return (mpz_perfect_square_p(*a));
}

long C_mpz_probab_prime_p(mpz_t * a, int b)
{
    return (mpz_probab_prime_p(*a, b));
}

long C_mpz_gcd(mpz_t * a, mpz_t * b, mpz_t * c)
{
    mpz_gcd(*a, *b, *c);
    return (1);
}

mpz_t *C_mpz_gcd_init(mpz_t * a, mpz_t * b)
{
    mpz_t *result = C_mpz_init();

    mpz_gcd(*result, *a, *b);
    return (result);
}

long C_mpz_gcd_ui(mpz_t * a, mpz_t * b, unsigned long int c)
{
    mpz_gcd_ui(*a, *b, c);
    return (1);
}

long C_mpz_gcdext(mpz_t * a, mpz_t * b, mpz_t * c, mpz_t * d, mpz_t * e)
{
    mpz_gcdext(*a, *b, *c, *d, *e);
    return (1);
}

long C_mpz_invert(mpz_t * a, mpz_t * b, mpz_t * c)
{
    return (mpz_invert(*a, *b, *c));
}

long C_mpz_jacobi(mpz_t * a, mpz_t * b)
{
    return (mpz_jacobi(*a, *b));
}

long C_mpz_legendre(mpz_t * a, mpz_t * b)
{
    return (mpz_legendre(*a, *b));
}

long C_mpz_cmp(mpz_t * a, mpz_t * b)
{
    return (mpz_cmp(*a, *b));
}

long C_mpz_cmp_ui(mpz_t * a, unsigned long int b)
{
    return (mpz_cmp_ui(*a, b));
}

long C_mpz_cmp_si(mpz_t * a, signed long int b)
{
    return (mpz_cmp_si(*a, b));
}

long C_mpz_sgn(mpz_t * a)
{
    return (mpz_sgn(*a));
}

long C_mpz_and(mpz_t * a, mpz_t * b, mpz_t * c)
{
    mpz_and(*a, *b, *c);
    return (1);
}

mpz_t *C_mpz_and_init(mpz_t * a, mpz_t * b)
{
    mpz_t *result = C_mpz_init();

    mpz_and(*result, *a, *b);
    return (result);
}

long C_mpz_ior(mpz_t * a, mpz_t * b, mpz_t * c)
{
    mpz_ior(*a, *b, *c);
    return (1);
}

mpz_t *C_mpz_ior_init(mpz_t * a, mpz_t * b)
{
    mpz_t *result = C_mpz_init();

    mpz_ior(*result, *a, *b);
    return (result);
}

long C_mpz_com(mpz_t * a, mpz_t * b)
{
    mpz_com(*a, *b);
    return (1);
}

mpz_t *C_mpz_com_init(mpz_t * a)
{
    mpz_t *result = C_mpz_init();
    mpz_com(*result, *a);
    return (result);
}

unsigned long int C_mpz_popcount(mpz_t * a)
{
    return (mpz_popcount(*a));
}

unsigned long int C_mpz_hamdist(mpz_t * a, mpz_t * b)
{
    return (mpz_hamdist(*a, *b));
}

unsigned long int C_mpz_scan0(mpz_t * a, unsigned long int b)
{
    return (mpz_scan0(*a, b));
}

unsigned long int C_mpz_scan1(mpz_t * a, unsigned long int b)
{
    return (mpz_scan1(*a, b));
}

long C_mpz_setbit(mpz_t * a, unsigned long int b)
{
    mpz_setbit(*a, b);
    return (1);
}

long C_mpz_clrbit(mpz_t * a, unsigned long int b)
{
    mpz_clrbit(*a, b);
    return (1);
}

long C_mpz_random(mpz_t * a, mp_size_t b)
{
    mpz_random(*a, b);
    return (1);
}

mpz_t *C_mpz_random_init(mp_size_t a)
{
    mpz_t *result = C_mpz_init();

    mpz_random(*result, a);
    return (result);
}

long C_mpz_random2(mpz_t * a, mp_size_t b)
{
    mpz_random2(*a, b);
    return (1);
}

mpz_t *C_mpz_random2_init(mp_size_t a)
{
    mpz_t *result = C_mpz_init();
    mpz_random2(*result, a);
    return (result);
}

size_t C_mpz_sizeinbase(mpz_t * a, int base)
{
    return (mpz_sizeinbase(*a, base));
}


///-----------------------------------------------------------------------------
