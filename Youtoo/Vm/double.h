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
