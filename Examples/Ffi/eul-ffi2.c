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
///  Library: misc
///  Authos: Andreas Kind
///  Description: foreign function test (see ffi2.em)
///  Compilation: cc -c eul-ffi2.c
///-----------------------------------------------------------------------------

#include "eul-ffi2.h"

int global_int = 42;

double global_double = 42.1;

char *global_string = "cba";

int ext_foo_int(int *x)
{
    int y;

    y = *x;
    *x = global_int;
    return y;
}

double ext_foo_double(double *x)
{
    double y;

    y = *x;
    *x = global_double;
    return y;
}

char *ext_foo_string(char **x)
{
    char *y;

    y = *x;
    *x = global_string;
    return y;
}

int *ext_foo_int2()
{
    return &global_int;
}

double *ext_foo_double2()
{
    return &global_double;
}

char **ext_foo_string2()
{
    return &global_string;
}

LispRef ext_nil()
{
    return eul_nil;
}

///-----------------------------------------------------------------------------
