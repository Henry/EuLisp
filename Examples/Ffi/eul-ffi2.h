/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: misc
///  Author: Andreas Kind
///  Description: foreign function test (see ffi2.em)
///  Compilation: cc -c eul-ffi2.c
///-----------------------------------------------------------------------------

#ifndef EUL_FFI2_H
#define EUL_FFI2_H

#include "eulisp.h"

extern int ext_foo_int(int *x);
extern double ext_foo_double(double *x);
extern char *ext_foo_string(char **x);
extern int* ext_foo_int2();
extern double* ext_foo_double2();
extern char **ext_foo_string2();
extern LispRef ext_nil();

///-----------------------------------------------------------------------------
#endif // EUL_FFI2_H
///-----------------------------------------------------------------------------
