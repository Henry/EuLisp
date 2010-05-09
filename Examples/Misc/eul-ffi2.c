/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
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
