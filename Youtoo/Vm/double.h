/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Andreas Kind
///  Description: double floats
///-----------------------------------------------------------------------------
#ifndef DOUBLE_H
#define DOUBLE_H

///-----------------------------------------------------------------------------
/// Double access
///-----------------------------------------------------------------------------

#define DOUBLE_SIZE (2)
#define eul_double_as_c_double(x) (*((double *) &(slot_ref(x, 0))))
#define eul_is_double(x)                                                       \
    (computed_object_class(x) == PGLOBAL(glob_double_class))

///-----------------------------------------------------------------------------
/// Double allocation
///-----------------------------------------------------------------------------

#define eul_allocate_double(loc, x)                                            \
    {                                                                          \
        double *d_ptr;                                                         \
        ALLOCATE_WARM_OBJECT(loc, PGLOBAL(glob_double_class), DOUBLE_SIZE);    \
        d_ptr = (double *) &(slot_ref(loc, 0));                                \
        *d_ptr = x;                                                            \
    }

#define eul_allocate_double2(loc, data1, data2)                                \
    {                                                                          \
        ALLOCATE_WARM_OBJECT(loc, PGLOBAL(glob_double_class), DOUBLE_SIZE);    \
        slot_ref(loc, 0) = (LispRef) data1;                                    \
        slot_ref(loc, 1) = (LispRef) data2;                                    \
    }


///-----------------------------------------------------------------------------
#endif // DOUBLE_H
///-----------------------------------------------------------------------------
