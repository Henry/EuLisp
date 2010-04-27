/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: cc
///  Authors: Andreas Kind
///  Description: C++ interoperability
///  Compilation:
///   ../youtoo -c test2 -l level1
///   gcc -c -I../Gc eul-cc2.c
///   g++ -c eul-cc3.cc
///   ../youtoo test2 -l level1 -fff eul-cc2 -fff eul-cc3 -recompile
///-----------------------------------------------------------------------------

#include "eulisp.h"
#include "test2.h"

EUL_DEFINTERN(bar, "bar", 2, test2)
extern void *baz(void *, void *);

char foo(char *str, int i)
{
    LispRef eul_str, eul_i, eul_res;

    eul_allocate_string(eul_str, str);
    eul_i = c_int_as_eul_int(i);
    eul_res = baz(eul_str, eul_i);

    return eul_char_as_c_char(eul_res);
}


///-----------------------------------------------------------------------------
