
/** Copyright (c) 1996 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: cc
 **  Authors: Andreas Kind
 **  Description: C++ interoperability
 **  Compilation:
 **   ../youtoo -c test2 -l level1
 **   gcc -c -I../Gc eul-cc2.c
 **   g++ -c eul-cc3.cc
 **   ../youtoo test2 -l level1 -fff eul-cc2 -fff eul-cc3 -recompile
 ** ----------------------------------------------------------------------- **/

extern "C" void *bar(void *, void *);

extern "C" void *baz(void *, void *);

void *baz(void *str, void *i)
{
    // Here could go any C++ code
    return bar(str, i);
}

/* eof */
