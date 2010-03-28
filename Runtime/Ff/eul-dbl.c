/** Copyright (c) 1997 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: level1, eval, youtoo
 **  Authos: Andreas Kind
 **  Description: foreign function support for double precision floats
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"

/** ----------------------------------------------------------------- **
 ** String buffer
 ** ----------------------------------------------------------------- **/

#define BUFFER_SIZE 512

static char buffer[BUFFER_SIZE];

/** ----------------------------------------------------------------- **
 ** Limits
 ** ----------------------------------------------------------------- **/

#ifndef SunOS
double eul_get_dbl_max()
{
    return DBL_MAX;
}

double eul_get_dbl_min()
{
    return DBL_MIN;
}

double eul_get_dbl_epsilon()
{
    return DBL_EPSILON;
}

double eul_get_neg_dbl_epsilon()
{
    return -DBL_EPSILON;
}
#else
double eul_get_dbl_max()
{
    return 1.7976931348623157E+308;
}

double eul_get_dbl_min()
{
    return 2.2250738585072014E-308;
}

double eul_get_dbl_epsilon()
{
    return 2.2204460492503131E-16;
}

double eul_get_neg_dbl_epsilon()
{
    return -2.2204460492503131E-16;
}
#endif /* SunOS */

/** ----------------------------------------------------------------- **
 ** Arithmetic
 ** ----------------------------------------------------------------- **/

double eul_dbl_sum(double x, double y)
{
    return x + y;
}

double eul_dbl_difference(double x, double y)
{
    return x - y;
}

double eul_dbl_product(double x, double y)
{
    return x * y;
}

double eul_dbl_quotient(double x, double y)
{
    return x / y;
}

double eul_dbl_remainder(double x, double y)
{
    return fmod(x, y);
}

int eul_dbl_mod(double x, double y)
{
    return (int)floor(fmod(x, y));
}

LispRef eul_dbl_less(double x, double y)
{
    return ((x < y) ? eul_true : eul_nil);
}

LispRef eul_dbl_equal(double x, double y)
{
    return ((x == y) ? eul_true : eul_nil);
}

/** ----------------------------------------------------------------- **
 ** Rounding ...
 ** ----------------------------------------------------------------- **/

double eul_dbl_ceiling(double x)
{
    return ceil(x);
}

double eul_dbl_floor(double x)
{
    return floor(x);
}

int eul_dbl_round(double x)
{
    return (int)(x < 0 ? x - .5 : x + .5);
}

int eul_dbl_truncate(double x)
{
    return (int)x;
}

/** ----------------------------------------------------------------- **
 ** Conversion double-float -> string, int -> double-float
 ** ----------------------------------------------------------------- **/

char *eul_dbl_as_str(double x)
{
    char *res;

    sprintf(buffer, "%f", x);
    res = (char *)gc_malloc(strlen(buffer) + 1);
    strcpy(res, buffer);
    return res;
}


// double eul_int_as_dbl(int x)
// {
//     return (double) x;
// }
