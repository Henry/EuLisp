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
///  Title: Interface thread -- mm-system and thread -- Lisp
///  Library: Runtime
///  Authors: Jens Bimberg
///-----------------------------------------------------------------------------
#ifndef THREAD_H
#define THREAD_H
///-----------------------------------------------------------------------------

typedef void *m_thread;
extern volatile int thread_schedule;

#ifndef NOPREEMPTIVE
// if preemptive scheduling is allowed, xalloc may be interrupted and
// then multiple allocs of the same memory location are possible. Thus
// a mechanism to avoid this is necessary. the most simple thing is
// setting and resetting a variable, which in turn is checked by the
// scheduler.

#define ENABLE_SCHEDULE()   thread_schedule = 1
#define DISABLE_SCHEDULE()  thread_schedule = 0
#else
#define ENABLE_SCHEDULE()
#define DISABLE_SCHEDULE()
#endif

extern m_thread m_thread_init();
extern m_thread m_thread_create(m_thread (*fun)(), void * args);
extern void m_thread_yield(m_thread dest);
extern void m_thread_mark(void (*mark_range)(void *, void *), void * mainstack);

///-----------------------------------------------------------------------------
#endif // THREAD_H
///-----------------------------------------------------------------------------
