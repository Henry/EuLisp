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
/// Title: C++ interoperability
///  Authors: Andreas Kind
///  Maintainer: Henry G. Weller
///  Compilation:
//    ../youtoo -c test2 -l level-1
//    gcc -c -I../Gc eul-cc2.c
//    g++ -c eul-cc3.cc
//    ../youtoo test2 -l level-1 -fff eul-cc2 -fff eul-cc3 -recompile
///-----------------------------------------------------------------------------

#include "eulisp.h"
#include "test2.h"

EUL_DEFINTERN(bar, "bar", 2, test2)
extern void *baz(void *, void *);

char foo(char *str, int i)
{
    LispRef eul_str, eul_i, eul_res;

    eul_allocate_string(eul_str, str);
    eul_i = c_int_as_eul_fpi(i);
    eul_res = baz(eul_str, eul_i);

    return eul_char_as_c_char(eul_res);
}

///-----------------------------------------------------------------------------
