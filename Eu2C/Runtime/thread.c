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
/// Title: Threads with setjmp and longjmp
///  Library: Xalloc
///  Authors: Jens Bimberg
///  Maintainer: Henry G. Weller
///  Problems:
//    System V setjmp/longjmp do not save/restore signal mask and so not
//    supported.
///-----------------------------------------------------------------------------
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <setjmp.h>
#include "xalloc_arch.h"
#include "xalloc_conf.h"

#ifdef MULTI_FSL
// a closer connection between threads and xalloc takes place
#include "xalloc_th.h"
#endif

#ifdef STACK_GROWS_DOWN
#define ST(b, s)                ((void *) (((int) (b) + (s)) & ~7))
#define mark_stack(sp, st)      mark_range((sp), (st))
#else
#ifdef STACK_GROWS_UP
#define ST(b, s)                ((void *) (((int) (b) + 7) & ~7))
#define mark_stack(sp, st)      mark_range((st), (sp))
#else
#error "you probably forgot to include xalloc_arch.h"
#endif
#endif

#define THREAD_STACK_SIZE (64*1024)

typedef struct m_thread
{
    jmp_buf th_machine_state;   // MUST be first slot
    #ifdef MULTI_FSL
    th_xa_arrays xalloc_arrays;
    #endif
    struct m_thread *next;
    char thread_stack[THREAD_STACK_SIZE];
} *m_thread;

extern void stack_switch_and_go(void *, void (*)());

#define STACK(mt)       ST((mt)->thread_stack, THREAD_STACK_SIZE)
#define STATE(mt)       mt->th_machine_state
#define SP(mt)          mt->th_machine_state[2]

// the following struct must match the first part of struct m_thread
static struct
{
    jmp_buf th_machine_state;
    #ifdef MULTI_FSL
    th_xa_arrays xalloc_arrays;
    #endif
} _mainstate;

#define mainstate ((m_thread)&_mainstate)

m_thread active_m_thread = mainstate;
#ifdef MULTI_FSL
th_xa_arrays *current_th_xa_arrays = &_mainstate.xalloc_arrays;
#endif
volatile int thread_schedule = 0;


void m_thread_yield(m_thread dest)
{
    if (!setjmp(STATE(active_m_thread)))
    {
        active_m_thread = dest;
        #ifdef MULTI_FSL
        current_th_xa_arrays = &(dest->xalloc_arrays);
        #endif
        longjmp(STATE(dest), 1);
    }
}


m_thread m_thread_init()
{
    return mainstate;
}


static m_thread free_m_threads = NULL;
static m_thread used_m_threads = NULL;


void m_thread_cleanup(m_thread t)
{
    if (t == used_m_threads)
    {
        // first in chain
        used_m_threads = t->next;
        t->next = free_m_threads;
        free_m_threads = t;
        return;
    }

    m_thread lastx = used_m_threads;

    m_thread x;
    while ((x = lastx->next))
    {
        if (x == t)
        {
            lastx->next = t->next;
            t->next = free_m_threads;
            free_m_threads = t;
            return;
        }

        lastx = x;
    }

    // failing, t was not in use
    return;
}


static m_thread start_m_thread;
static m_thread(*start_fun) ();
static void *start_arg;


static void m_thread_function()
{
    m_thread(*fun) () = start_fun;      // locally safe fun and arg
    void *arg = start_arg;

    if (!setjmp(STATE(start_m_thread))) // jump back
    {
        longjmp(STATE(active_m_thread), 1);
    }

    m_thread next = fun(arg);    // evaluate the thread function
    m_thread_cleanup(active_m_thread);
    m_thread_yield(next);
}


m_thread m_thread_create(m_thread(*fun) (), void *arg)
{
    m_thread mt = free_m_threads;
    if (mt)
    {
        free_m_threads = mt->next;
    }
    else
    {
        mt = (m_thread) sbrk(sizeof(struct m_thread));

        if (mt == (m_thread) - 1)
        {
            fprintf(stderr, "m_thread_create: insufficient space\n");
            abort();
        }
    }

    #ifdef MULTI_FSL
    memset((char *)&(mt->xalloc_arrays), 0, sizeof(mt->xalloc_arrays));
    #endif

    mt->next = used_m_threads;
    used_m_threads = mt;
    start_fun = fun;
    start_arg = arg;
    start_m_thread = mt;

    if (!setjmp(STATE(active_m_thread)))
    {
        stack_switch_and_go(STACK(mt), m_thread_function);
    }

    return mt;
}


void m_thread_mark(void (*mark_range) (void *, void *), void *mainstack)
{
    // the main stack
    if (active_m_thread == mainstate)
    {
        m_thread t;
        mark_stack((void *)&t, mainstack);
    }
    else
    {
        mark_stack((void *)&SP(mainstate), mainstack);
        mark_range(mainstate, (char *)mainstate + sizeof(jmp_buf));
    }

    // all the other stacks
    for (m_thread t = used_m_threads; t != NULL; t = t->next)
    {
        if (active_m_thread == t)
        {
            mark_stack((void *)&t, STACK(t));
        }
        else
        {
            mark_stack((void *)&SP(t), STACK(t));
            mark_range(t, (char *)t + sizeof(jmp_buf));
        }
    }
}


///-----------------------------------------------------------------------------
