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
/// Title: Foreign threads based on MIT threads
///  Library: fthread (foreign thread interface)
///  Authors: Liam Wickins, Andreas Kind
///  Maintainer: Henry G. Weller
///  Description:
//    see fthread.em
///-----------------------------------------------------------------------------

#include <eulisp.h>
#include <pthread.h>
#include <gc/gc.h>

///-----------------------------------------------------------------------------
/// Thread access
///-----------------------------------------------------------------------------

#define THREAD_FUNCTION(x) (slot_ref((x),2))
#define THREAD_RETURNED(x) (slot_ref((x),3))
#define THREAD_RETURN_VALUE(x) (slot_ref((x),4))
#define THREAD_RETURN_MUTEX(x) (slot_ref((x),5))
#define THREAD_HANDLE(x) (slot_ref((x),6))
#define THREAD_REGISTERS(x) (slot_ref((x),7))

///-----------------------------------------------------------------------------
/// Initialization
///-----------------------------------------------------------------------------

pthread_key_t eul_thr_key;

LispRef eul_initialize_foreign_threads(LispRef thr)
{
    pthread_key_create(&eul_thr_key, NULL);

    // Lisp thread should be accessable from the UI thread (see eul_thr_self)
    pthread_setspecific(eul_thr_key, thr);

    return eul_nil;
}

///-----------------------------------------------------------------------------
/// eul_thr_interpret()
/// Description: function actually running on a thread
/// Args: Lisp thread
/// Returns: void
/// See also: eul_thr_create()
///-----------------------------------------------------------------------------


void eul_thr_interpret(LispRef thr)
{
    RegisterRef tame_regs;


    // Lisp thread should be accessable from the UI thread (see eul_thr_self)
    pthread_setspecific(eul_thr_key, thr);

    tame_regs = (RegisterRef) THREAD_REGISTERS(thr);
    interpret(tame_regs);
    pthread_exit(0);
}

///-----------------------------------------------------------------------------
/// eul_thr_create()
/// Description: Allocates and sets up the interpreter stack for execution
///              of the initial function; creates the thread handle;
///              pushes the functions arguments on the stack and counts arity
/// Args: Lisp thread plus Lisp args
/// Returns: thread
///-----------------------------------------------------------------------------

LispRef eul_thr_create(LispRef thr, LispRef args)
{
    int status, arity = 0;
    pthread_t *thr_handle;
    RegisterRef tame_regs;
    LispRef fn;
    pthread_mutex_t *thr_join_mt;

    // Ensure initial function is a lambda function
    fn = THREAD_FUNCTION(thr);
    if(!eul_is_lambda(fn))
        if(!eul_is_gf(fn)) {
            printf("\n*** ERROR [system]: bad foreign function in-call\n");
            fprint_ref(stdout,fn);
            exit(-1);
        }
        else {
            // This is a generic function, and can't be directly executed.
            // therefore, use the discriminator of this function
            fn = GF_DISC_FN(fn);
        }

    // Create the register set for the virtual machine
    tame_regs = (RegisterRef) gc_malloc(sizeof(RegisterSet));
    INITIALISE_REGISTER_SET(tame_regs);
    tame_regs->reg_value_stack->sp = tame_regs->reg_value_stack->base;
    tame_regs->reg_context_stack->sp = tame_regs->reg_context_stack->base;
    tame_regs->reg_current_cv = (Instruction *) LAMBDA_CODE(fn);
    tame_regs->reg_pc = tame_regs->reg_current_cv;
    tame_regs->reg_env = LAMBDA_ENV(fn);
    tame_regs->reg_next_methods = eul_nil;

    // Push arguments on to stack and count arity
    while(args != eul_nil) {
        EXTERNAL_PUSHVAL1(eul_car(args));
        args = eul_cdr(args);
        arity++;
    }
    tame_regs->reg_arg_count = arity;

    THREAD_REGISTERS(thr) = (LispRef) tame_regs;

    // Create the mutex to do MT save joining

    // Instead of mutex_init(thr_join_mt, USYNC_THREAD, NULL);
    thr_join_mt = (pthread_mutex_t *) gc_malloc(sizeof(pthread_mutex_t));
    THREAD_RETURN_MUTEX(thr) = (LispRef) thr_join_mt;

    // Create the thread
    thr_handle = (pthread_t *) gc_malloc(sizeof(pthread_t));
    THREAD_HANDLE(thr) = (LispRef) thr_handle;
    status = pthread_create(thr_handle, NULL, (void *(*) (void *)) eul_thr_interpret, thr);
    if (status != 0)
        return eul_nil;

    return thr;
}


///-----------------------------------------------------------------------------
/// eul_thr_join()
/// Description: Awaits for the thread to complete, pops its return
///              value from the stack and returns it.
/// Args: Lisp thread
/// Returns: Lisp Object
///-----------------------------------------------------------------------------

#define THREAD_WAITING 1
#define THREAD_JOINED  3

LispRef eul_thr_join(LispRef thr)
{
    LispRef res;
    void *status;
    pthread_t *thr_handle;
    RegisterRef tame_regs;
    pthread_mutex_t *thr_join_mt;

    thr_handle = (pthread_t *) THREAD_HANDLE(thr);
    tame_regs = (RegisterRef) THREAD_REGISTERS(thr);
    thr_join_mt = (pthread_mutex_t *) THREAD_RETURN_MUTEX(thr);

    // Wait for target thread to complete
    pthread_mutex_lock(thr_join_mt);

    if (THREAD_RETURNED(thr) == eul_nil) {
        if (pthread_join(*thr_handle, &status) != 0)
            res = eul_nil;
        else {
            EXTERNAL_POPVAL1(res);
            THREAD_RETURN_VALUE(thr) = res;
            THREAD_RETURNED(thr) = eul_true;
        }
    }
    else
        res = THREAD_RETURN_VALUE(thr);

    pthread_mutex_unlock(thr_join_mt);
    return res;
}


///-----------------------------------------------------------------------------
/// eul_thr_yield()
/// Description: yield processor to a thread
/// Args: none
/// Returns: eul_nil
///-----------------------------------------------------------------------------

LispRef eul_thr_yield()
{
    pthread_yield();
    return eul_nil;
}


///-----------------------------------------------------------------------------
/// eul_thr_self()
/// Description: return the calling thread
/// Args: none
/// Returns: Lisp thread
///-----------------------------------------------------------------------------

LispRef eul_thr_self()
{
    LispRef res;

    res = (LispRef) pthread_getspecific(eul_thr_key);
    return res;
}

///-----------------------------------------------------------------------------
/// eul_thr_suspend()
/// Description: suspends the specified thread
/// Args: eul_nil or Lisp list with one Lisp thread
/// Returns: eul_true, if suspended successfully; eul_nil, otherwise
/// See also: eul_thr_continue()
///-----------------------------------------------------------------------------

/*
LispRef eul_thr_suspend(LispRef thrs)
{
    pthread_t thr_handle_struct, *thr_handle;

    if (thrs == eul_nil) {
        thr_handle_struct = pthread_self();
        thr_handle = &thr_handle_struct;
    }
    else
        thr_handle = (pthread_t *) THREAD_HANDLE(eul_car(thrs));

    // Attempt to suspend the thread
    if(pthread_suspend(*thr_handle) == 0)
        return eul_true;
    else
        return eul_nil;
}
*/

///-----------------------------------------------------------------------------
/// eul_thr_continue()
/// Description: resumes the execution of the specified (suspended) thread
/// Args: Lisp thread
/// Returns: thread, if thread continued successfully; otherwise eul_nil
/// See also: eul_thr_suspend()
///-----------------------------------------------------------------------------

/*
LispRef eul_thr_continue(LispRef thr)
{
    pthread_t *thr_handle;

    thr_handle = (pthread_t *) THREAD_HANDLE(thr);

    // Attempt to continue thread
    if (pthread_continue(*thr_handle) == 0)
        return thr;
    else
        return eul_nil;
}
*/

///-----------------------------------------------------------------------------
