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
/// Title: Garbage collection for variable precision numbers
///  Library: bignum
///  Authors: Danius Michaelides, Andreas Kind
///  Maintainer: Henry G. Weller
///  Compilation
//    see Makefile
///-----------------------------------------------------------------------------
#include "eulisp.h"
#include "gmp.h"

///-----------------------------------------------------------------------------
/// Set the memory funtions
///-----------------------------------------------------------------------------

void *C_gc_realloc(void *p, size_t o, size_t n)
{
    return (gc_realloc(p, n));
}

void C_gc_free(void *p, size_t o)
{
    gc_free(p);
}

void *C_gc_malloc(size_t n)
{
    return (gc_malloc(n));
}

long eul_gmp_init()
{
    mp_set_memory_functions(C_gc_malloc, C_gc_realloc, C_gc_free);
    return (1);
}


///-----------------------------------------------------------------------------
