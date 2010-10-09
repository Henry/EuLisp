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
///  Title: symbols
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------

#ifndef SYMBOL_H
#define SYMBOL_H

///-----------------------------------------------------------------------------
/// Symbol access
///-----------------------------------------------------------------------------

#define SYMBOL_SIZE (1)
#define SYMBOL_NAME(x) (slot_ref((x), 0))
#define eul_symbol_name(x) SYMBOL_NAME(x)
#define eul_symbol_as_c_string(x) eul_string_as_c_string(SYMBOL_NAME(x))
#define eul_is_symbol(x)                                                       \
    (computed_object_class(x) == PGLOBAL(glob_symbol_class))

///-----------------------------------------------------------------------------
/// Symbol allocation
///-----------------------------------------------------------------------------

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

///-----------------------------------------------------------------------------
#endif // SYMBOL_H
///-----------------------------------------------------------------------------
