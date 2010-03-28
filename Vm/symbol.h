/** Copyright (c) 1997 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: eulvm (Bytecode Interpreter -- Eutopia)
 **  Authors: Keith Playford, Andreas Kind
 **  Description: symbols
 ** ----------------------------------------------------------------------- **/

#ifndef SYMBOL_H
#define SYMBOL_H

/** ----------------------------------------------------------------- **
 ** Symbol access
 ** ----------------------------------------------------------------- **/

#define SYMBOL_SIZE (1)
#define SYMBOL_NAME(x) (slot_ref((x), 0))
#define eul_symbol_name(x) SYMBOL_NAME(x)
#define eul_symbol_as_c_string(x) eul_string_as_c_string(SYMBOL_NAME(x))
#define eul_is_symbol(x)                                                       \
    (computed_object_class(x) == PGLOBAL(glob_symbol_class))

/** ----------------------------------------------------------------- **
 ** Symbol allocation
 ** ----------------------------------------------------------------- **/

#define eul_allocate_symbol(loc, str)                                          \
    ALLOCATE_WARM_OBJECT(loc, PGLOBAL(glob_symbol_class), SYMBOL_SIZE);        \
    eul_allocate_string(SYMBOL_NAME(loc), str)

#define eul_intern_symbol(loc, str)                                            \
    loc = eul_fast_table_ref(PGLOBAL(glob_symbols), str);                      \
    if (eul_null(loc))                                                         \
    {                                                                          \
        eul_allocate_symbol(loc, str);                                         \
        eul_fast_table_set(PGLOBAL(glob_symbols), str, loc);                   \
    }


#endif // SYMBOL_H
