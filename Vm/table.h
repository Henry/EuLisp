/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Description: hash tables
///-----------------------------------------------------------------------------

#ifndef TABLE_H
#define TABLE_H

#include "object.h"

extern LispRef eul_fast_table_rehash (LispRef);
extern LispRef eul_fast_table_ref (LispRef, char *);
extern LispRef eul_fast_table_set (LispRef, char *, LispRef);
extern int eul_hash_object(LispRef key);
extern int eul_hash_object_aux(LispRef key, int *leaves);


///-----------------------------------------------------------------------------
/// Table access
///-----------------------------------------------------------------------------

#define TABLE_ENTRIES(x) (slot_ref((x), 0))
#define TABLE_FILL_VALUE(x) (slot_ref((x), 1))
#define TABLE_POPULATION(x) (slot_ref((x), 2))
#define TABLE_THRESHOLD(x) (slot_ref((x), 3))

#define TABLE_SIZE 4
#define TABLE_FILL_FACTOR 2
#define TABLE_UNUSED 4       // We want some fragmentation
#define MIN_TABLE_ENTRIES 16

#define TABLE_ENTRY_KEY(x) eul_car(x)
#define TABLE_ENTRY_VALUE(x) eul_cdr(x)
#define IS_TABLE_ENTRY(x) eul_is_cons(x)


///-----------------------------------------------------------------------------
/// Table allocation
///-----------------------------------------------------------------------------

#define eul_allocate_table(loc, fill_value)                                    \
    {                                                                          \
        eul_allocate_object(loc, PGLOBAL(glob_table_class), TABLE_SIZE, eul_nil); \
        eul_allocate_object(TABLE_ENTRIES(loc), PGLOBAL(glob_vector_class),    \
        MIN_TABLE_ENTRIES, eul_nil);                                           \
        TABLE_ENTRIES(loc) = eul_nil;                                          \
        TABLE_POPULATION(loc) = c_int_as_eul_int(0);                           \
        TABLE_THRESHOLD(loc) = c_int_as_eul_int(MIN_TABLE_ENTRIES-TABLE_UNUSED); \
        TABLE_FILL_VALUE(loc) = fill_value;                                    \
    }

#define eul_allocate_table_entry(loc, key, value)                              \
    eul_allocate_cons(loc, key, value)

///-----------------------------------------------------------------------------
#endif // TABLE_H
///-----------------------------------------------------------------------------
