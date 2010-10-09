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
///  Title: strings
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#ifndef EUL_STRING_H
#define EUL_STRING_H

///-----------------------------------------------------------------------------
/// String access
///-----------------------------------------------------------------------------
#define STRING_SIZE (1)
#define EUL_STRING_SIZE c_int_as_eul_int(STRING_SIZE)
#define STRING_DATA(x) (slot_ref((x), 0))
#define eul_string_size(x) object_size(x)
#define eul_string_ref(str, i) (*(((char *) STRING_DATA(str))+i))
#define eul_string_as_c_string(x) ((char *) STRING_DATA(x))
#define eul_is_string(x)                                                       \
    (!is_immediate(x) && (object_class(x) == PGLOBAL(glob_string_class)))
#define eul_is_vector(x)                                                       \
    (!is_immediate(x) && (object_class(x) == PGLOBAL(glob_vector_class)))


///-----------------------------------------------------------------------------
/// String allocation
///-----------------------------------------------------------------------------
#define eul_allocate_string(loc, str)                                          \
    {                                                                          \
        char *tmp_str = str;                                                   \
        size_t n = strlen(tmp_str);                                            \
        ALLOCATE_WARM_OBJECT(loc, PGLOBAL(glob_string_class), STRING_SIZE);    \
        eul_allocate_int(eul_string_size(loc), n);                             \
        STRING_DATA(loc) = (LispRef)tmp_str;                                   \
    }

#define eul_allocate_nstring(loc, str, n)                                      \
    {                                                                          \
        char *tmp_str = str;                                                   \
        ALLOCATE_WARM_OBJECT(loc, PGLOBAL(glob_string_class), STRING_SIZE);    \
        eul_allocate_int(eul_string_size(loc), n);                             \
        STRING_DATA(loc) = (LispRef)tmp_str;                                   \
    }

#define eul_allocate_blank_string(loc, n)                                      \
    {                                                                          \
        char *str = (char *) gc_malloc(n+1);                                   \
        memset(str, ' ', n);                                                   \
        *(str + n) = '\0';                                                     \
        eul_allocate_string(loc, str);                                         \
    }

#define eul_allocate_static_string(loc, str, n)                                \
    static LispRef loc##_static[] =                                            \
    {                                                                          \
        c_int_as_eul_int(n),                                                   \
        NULL,                                                                  \
        (LispRef) str                                                          \
    };                                                                         \
    static LispRef loc = eul_as_static(loc)


///-----------------------------------------------------------------------------
/// Vector allocation
///-----------------------------------------------------------------------------
#define eul_allocate_vector(loc, n, list)                                      \
    {                                                                          \
        LispRef last_init_value = eul_nil;                                     \
        ALLOCATE_WARM_OBJECT(loc, PGLOBAL(glob_vector_class), n);              \
        for (size_t i=0; i<n; i++)                                             \
        {                                                                      \
            if (eul_null(list))                                                \
            {                                                                  \
                slot_ref(loc, i) = last_init_value;                            \
            }                                                                  \
            else                                                               \
            {                                                                  \
                slot_ref(loc, i) = eul_car(list);                              \
                last_init_value = eul_car(list);                               \
                list = eul_cdr(list);                                          \
            }                                                                  \
        }                                                                      \
    }

#define eul_allocate_string_vector(loc, n, vec)                                \
    {                                                                          \
        ALLOCATE_WARM_OBJECT(loc, PGLOBAL(glob_vector_class), n);              \
        for (size_t i=0; i<n; i++)                                             \
        {                                                                      \
            eul_allocate_string(slot_ref(loc, i), vec[i]);                     \
        }                                                                      \
    }

///-----------------------------------------------------------------------------
#endif // EUL_STRING_H
///-----------------------------------------------------------------------------
