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
/// Title: Initialization of c runtime constants
///  Library: Runtime
///  Authors: Jens Bimberg
///  Description:
//    e.g. size of FILE or jmp_buf
///-----------------------------------------------------------------------------
#include <stdio.h>
#include <signal.h>
#include "xalloc.h"

#ifdef ROOT_SET_IN_USE
long StaticLiteralBegin;
long StaticLiteralEnd;
#endif

// be careful changeing the following decls requires change in c-runtime.h too

#ifdef ROOT_SET_IN_USE
void gc_init()
{
    long *ptr;
    void c_runtime_init();

    // call runtime initialization of system constants
    // for
    // (
    //     ptr = (long *) StaticVariableRoot;
    //     ptr < (long *)&StaticObjectRoot;
    //     ++ptr
    // )
    // {
    //     if (p_in_heap((void *)**ptr) add_to_root_set((void *))**ptr);
    // }

    // for
    // (
    //     ptr = (long *) StaticObjectRoot;
    //     ptr < (long *)&StaticRootEnd;
    //     ++ptr
    // )
    // {
    //     if (p_in_heap((void *)*ptr) add_to_root_set((void *)) *ptr);
    // }

    for
    (
        ptr = (long*)&StaticLiteralBegin;
        ptr < (long*)&StaticLiteralEnd;
        ptr++
    )
    {
        if (p_in_heap((long *)*ptr))
        {
            add_to_root_set((long *)*ptr);
        }
    }
}
#endif

void c_runtime_init()
{
    #ifdef ROOT_SET_IN_USE
    gc_init();
    #endif
}


///-----------------------------------------------------------------------------
