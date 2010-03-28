
/** Copyright (c) 1996 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library:
 **  Authors: Andreas Kind
 **  Description:  C++ interoperability
 **  Compilation:
 **   g++ -c eul-cc1.cc
 **   ../youtoo test1 -l level1 -fff eul-cc1
 ** ----------------------------------------------------------------------- **/

extern "C" char foo(char *, int);

char foo(char *str, int i)
{
    // Here could go any C++ code
    return *(str + i);
}


/* eof */
