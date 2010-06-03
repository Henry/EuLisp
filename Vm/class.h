/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Description: classes and short names for global registers
///-----------------------------------------------------------------------------
#ifndef CLASS_H
#define CLASS_H

///-----------------------------------------------------------------------------
/// Global register short names
///-----------------------------------------------------------------------------

#define eul_true PGLOBAL(glob_true)
#define eul_nil PGLOBAL(glob_nil)
#define eul_symbols PGLOBAL(glob_symbols)
#define eul_keywords PGLOBAL(glob_keywords)
#define eul_modules PGLOBAL(glob_modules)
#define eul_argc PGLOBAL(glob_argc)
#define eul_argv PGLOBAL(glob_argv)

#define CONCAT_STATIC(x) x##_static
#define eul_as_static(x) ((LispRef)CONCAT_STATIC(x))

#define eul_static_string_class eul_as_static(PGLOBAL(glob_string_class))
#define eul_static_cons_class eul_as_static(PGLOBAL(glob_cons_class))
#define eul_static_nil eul_as_static(PGLOBAL(glob_nil))

///-----------------------------------------------------------------------------
/// nil allocation
///-----------------------------------------------------------------------------

#define EUL_NIL_SIZE (c_int_as_eul_int(0))

#define eul_allocate_static_nil(loc)                                           \
    LispRef loc##_static[] = {EUL_NIL_SIZE, NULL};                             \
    LispRef loc = eul_static_nil

#define eul_finalize_nil(loc)                                                  \
    object_class(loc) = PGLOBAL(glob_null_class)

///-----------------------------------------------------------------------------
/// Dynamic class allocation
///-----------------------------------------------------------------------------

#define CLASS_SIZE (10)
#define EUL_CLASS_SIZE (c_int_as_eul_int(CLASS_SIZE))
#define CLASS_NAME(o) (slot_ref((o), 0))

#define eul_allocate_class(loc)                                                \
    eul_allocate_object(loc, eul_nil, CLASS_SIZE, eul_nil)

#define eul_allocate_static_class(loc)                                         \
    LispRef loc##_static[] =                                                   \
    {                                                                          \
        EUL_CLASS_SIZE, eul_static_nil,                                        \
        eul_static_nil, eul_static_nil, eul_static_nil,                        \
        eul_static_nil, eul_static_nil, eul_static_nil,                        \
        eul_static_nil, eul_static_nil, eul_static_nil,                        \
        eul_static_nil                                                         \
    };                                                                         \
    LispRef loc = eul_as_static(loc)


///-----------------------------------------------------------------------------
#endif // CLASS_H
///-----------------------------------------------------------------------------
