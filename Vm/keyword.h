/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Description: keywords
///-----------------------------------------------------------------------------
#ifndef KEYWORD_H
#define KEYWORD_H

///-----------------------------------------------------------------------------
/// Keyword access
///-----------------------------------------------------------------------------

#define KEYWORD_SIZE (1)
#define KEYWORD_NAME(x) (slot_ref((x), 0))
#define eul_keyword_name(x) KEYWORD_NAME(x)
#define eul_keyword_as_c_string(x) eul_string_as_c_string(KEYWORD_NAME(x))
#define eul_is_keyword(x)                                                      \
    (computed_object_class(x) == PGLOBAL(glob_keyword_class))

///-----------------------------------------------------------------------------
/// Keyword allocation
///-----------------------------------------------------------------------------

#define eul_allocate_keyword(loc, str)                                         \
    ALLOCATE_WARM_OBJECT(loc, PGLOBAL(glob_keyword_class), KEYWORD_SIZE);      \
    eul_allocate_string(KEYWORD_NAME(loc), str)

#define eul_intern_keyword(loc, str)                                           \
    loc = eul_fast_table_ref(PGLOBAL(glob_keywords), str);                     \
    if (eul_null(loc))                                                         \
    {                                                                          \
        eul_allocate_keyword(loc, str);                                        \
        eul_fast_table_set(PGLOBAL(glob_keywords), str, loc);                  \
    }

///-----------------------------------------------------------------------------
#endif // KEYWORD_H
///-----------------------------------------------------------------------------
