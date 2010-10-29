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
/// Title: foreign function support for double precision floats
///  Library: level-1, eval, youtoo
///  Authors: Andreas Kind
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "eulisp.h"

///-----------------------------------------------------------------------------
/// String buffer
///-----------------------------------------------------------------------------
#define BUFFER_SIZE 512
static char buffer[BUFFER_SIZE];


///-----------------------------------------------------------------------------
/// Limits
///-----------------------------------------------------------------------------
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


///-----------------------------------------------------------------------------
/// Arithmetic
///-----------------------------------------------------------------------------
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


///-----------------------------------------------------------------------------
/// Rounding ...
///-----------------------------------------------------------------------------
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


///-----------------------------------------------------------------------------
/// Conversion double-float -> string, int -> double-float
///-----------------------------------------------------------------------------

char *eul_dbl_as_str(double x)
{
    sprintf(buffer, "%f", x);
    char *res = (char *)gc_malloc(strlen(buffer) + 1);
    strcpy(res, buffer);

    return res;
}


// double eul_fpi_as_dbl(int x)
// {
//     return (double) x;
// }


///-----------------------------------------------------------------------------
