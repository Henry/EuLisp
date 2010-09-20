/// Copyright 1996 A. Kind & University of Bath
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
///  Library: fthread (foreign thread interface)
///  Authors: Andreas Kind, Liam Wickins
///  Description: Foreign function support for solaris locks
///-----------------------------------------------------------------------------

#include <eulisp.h>
#include <synch.h>
#include <gc/gc.h>

///-----------------------------------------------------------------------------
/// eul_lock_create()
///
/// Description: create a UI mutex
/// Args: none
/// Returns: lock handler or eul_nil on failure
///-----------------------------------------------------------------------------

LispRef eul_lock_create()
{
    mutex_t *mt = (mutex_t *) gc_malloc(sizeof(mutex_t));

    // Initialise a new mutex
    if(mutex_init(mt, USYNC_THREAD, NULL) == 0)
    {
        return (LispRef) mt;
    }
    else
    {
        return eul_nil;
    }
}

///-----------------------------------------------------------------------------
/// eul_sem_create()
///
/// Description: create a UI semaphore
/// Args: count
/// Returns: semaphore handler or eul_nil on failure
///-----------------------------------------------------------------------------

LispRef eul_sema_create(int count)
{
    sema_t *sem = (sema_t *) gc_malloc(sizeof(sema_t));

    // Initialise a new semaphore
    if(sema_init(sem, count, USYNC_THREAD, NULL) == 0)
    {
        return (LispRef) sem;
    }
    else
    {
        return eul_nil;
    }
}

///-----------------------------------------------------------------------------
