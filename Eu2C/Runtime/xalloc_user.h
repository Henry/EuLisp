/// Copyright 1994-2010 Fraunhofer ISST
/// Copyright 2010 Henry G. Weller
///-----------------------------------------------------------------------------
//  This file is part of
/// ---                           EuLisp System 'Eu2C'
///-----------------------------------------------------------------------------
//
//  Eu2C is free software: you can redistribute it and/or modify it under the
//  terms of the GNU General Public License version 2 as published by the Free
//  Software Foundation.
//
//  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
//  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
//  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
//  details.
//
//  You should have received a copy of the GNU General Public License along with
//  this program.  If not, see <http://www.gnu.org/licenses/>.
//
///-----------------------------------------------------------------------------
/// Title: xalloc user include file
///  Library: Runtime
///  Authors: Jens Bimberg
///  Description:
///    Macros and prototypes of functions around xalloc / garbage collection
///-----------------------------------------------------------------------------
#ifndef XALLOC_USER_H
#define XALLOC_USER_H

#include "xalloc_arch.h"

///-----------------------------------------------------------------------------

#ifdef __cplusplus
extern "C"
{
#endif

    typedef unsigned long int CardDscr;
    typedef unsigned long int TypeDscr;

    TypeDscr describe_type
    (
        unsigned long usertype,
        void* (*mark_fcn)(void *, long)
    );

    // 1 <= td < lowest_type_descriptor
    TypeDscr set_type_descriptor
    (
        TypeDscr td,
        unsigned long usertype,
        void* (*mark_fcn)(void *, long)
    );

    // 1 <= lowest_type_descriptor
    int set_lowest_type_descriptor
    (
        TypeDscr lowest_type_descriptor
    );

    CardDscr describe_card
    (
        char cardtype,
        long size,
        TypeDscr t_dscr
    );

    // 0 <= cd < lowest_card_descriptor
    CardDscr set_card_descriptor
    (
        CardDscr cd,
        char cardtype,
        long size,
        TypeDscr t_dscr
    );

    //      0 <= lowest_card_descriptor
    int set_lowest_card_descriptor(CardDscr lowest_card_descriptor);

    TypeDscr get_type_descriptor(long *ptr);

    unsigned long type_from_descriptor(TypeDscr td);

    // previous two funcions at once
    unsigned long  get_type(long *ptr );

    // returns NO_CONSISTENT_POINTER if so
    unsigned long safe_get_type(long *ptr);

    #define NO_CONSISTENT_POINTER   0xffffffff

    // possible values for cardtype
    #define MTSS    1
    #define STMS    2
    #define STSS    4

    char get_card_type(CardDscr cd);

    long get_object_size(void *ptr);

    void *xalloc_stss(CardDscr cd );

    void *xalloc_mtss(CardDscr cd, TypeDscr td );

    void *xalloc_stms(CardDscr cd, long size );

    void *xalloc(CardDscr cd, long possible_second_value );

    void *trace_pointer(void *ptr );

    void force_garbage_collection();

    long inc_heap_size(int nb_of_cards );

    void initialize_root_set();

    void add_to_root_set(void *pointer);

    void delete_from_root_set(void *pointer );

    // predefined mark_functions

    // trace from all locations inside the object
    void *trace_all(register void *ptr, register long length);

    // trace from nowhere inside this object
    void *trace_nothing(register void *ptr, register long length);

    // assuming the first 4 byte to be a pointer
    void *trace_first(register void *ptr, register long length);

    // assuming the second 4 byte to be a pointer
    void *trace_second(register void *ptr, register long length);

    // assuming the first 8 byte to be 2 pointers
    void *trace_pair(register void *ptr, register long length);

    // information about gc configuration
    void xalloc_info();

    // the following macro is required to be called in main() to tell the
    // garbage collector about the highest possible stack address
    void set_stacktop(void **p);

    #define INITIALIZE_GC   {void *p; set_stacktop(&p);}

    #ifdef __cplusplus
}
#endif

///-----------------------------------------------------------------------------
#endif // XALLOC_USER_H
///-----------------------------------------------------------------------------
