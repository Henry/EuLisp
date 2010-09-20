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
