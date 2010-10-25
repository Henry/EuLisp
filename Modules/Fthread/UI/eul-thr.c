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
/// Title: FF interface to Unix International (aka Solaris) Threads (UI)
///  Library: fthread (foreign thread interface)
///  Authors: Liam Wickins, Andreas Kind
///  Maintainer: Henry G. Weller
///  Description:
//    see fthread.em
///-----------------------------------------------------------------------------
#include <eulisp.h>
#include <thread.h>
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
thread_key_t eul_thr_key;

LispRef eul_initialize_foreign_threads(LispRef thr)
{
  thr_keycreate(&eul_thr_key, NULL);

  /* Lisp thread should be accessable from the UI thread (see eul_thr_self) */
  thr_setspecific(eul_thr_key, thr);

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

  /* Lisp thread should be accessable from the UI thread (see eul_thr_self) */
  thr_setspecific(eul_thr_key, thr);

  tame_regs = (RegisterRef) THREAD_REGISTERS(thr);
  interpret(tame_regs);
  thr_exit(0);
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
  thread_t *thr_handle;
  RegisterRef tame_regs;
  LispRef fn;
  mutex_t *thr_join_mt;

  /* Ensure initial function is a lambda function */
  fn = THREAD_FUNCTION(thr);
  if(!eul_is_lambda(fn))
    if(!eul_is_gf(fn)) {
      printf("\n*** ERROR [system]: bad foreign function in-call\n");
      fprint_ref(stdout,fn);
      exit(-1);
    }
    else {
      /* This is a generic function, and can't be directly executed.
       * therefore, use the discriminator of this function */
      fn = GF_DISC_FN(fn);
    }

  /* Create the register set for the virtual machine */
  tame_regs = (RegisterRef) gc_malloc(sizeof(RegisterSet));
  INITIALISE_REGISTER_SET(tame_regs);
  tame_regs->reg_value_stack->sp = tame_regs->reg_value_stack->base;
  tame_regs->reg_context_stack->sp = tame_regs->reg_context_stack->base;
  tame_regs->reg_current_cv = (Instruction *) LAMBDA_CODE(fn);
  tame_regs->reg_pc = tame_regs->reg_current_cv;
  tame_regs->reg_env = LAMBDA_ENV(fn);
  tame_regs->reg_next_methods = eul_nil;

  /* Push arguments on to stack and count arity */
  while(args != eul_nil) {
    EXTERNAL_PUSHVAL1(eul_car(args));
    args = eul_cdr(args);
    arity++;
    }
  tame_regs->reg_arg_count = arity;

  THREAD_REGISTERS(thr) = (LispRef) tame_regs;

  /* Create the mutex to do MT save joining */

  /* Instead of mutex_init(thr_join_mt, USYNC_THREAD, NULL); */
  thr_join_mt = (mutex_t *) gc_malloc(sizeof(mutex_t));
  THREAD_RETURN_MUTEX(thr) = (LispRef) thr_join_mt;

  /* Create the thread */
  thr_handle = (thread_t *) gc_malloc(sizeof(thread_t));
  THREAD_HANDLE(thr) = (LispRef) thr_handle;
  status = thr_create(NULL, 0, (void *(*) (void *)) eul_thr_interpret, thr,
		      0, thr_handle);
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
  int status;
  thread_t *thr_handle;
  RegisterRef tame_regs;
  mutex_t *thr_join_mt;

  thr_handle = (thread_t *) THREAD_HANDLE(thr);
  tame_regs = (RegisterRef) THREAD_REGISTERS(thr);
  thr_join_mt = (mutex_t *) THREAD_RETURN_MUTEX(thr);

  /* Wait for target thread to complete */
  mutex_lock(thr_join_mt);

  if (THREAD_RETURNED(thr) == eul_nil) {
    status = thr_join(*thr_handle, 0, 0);
    if (status == THREAD_JOINED)
      res = eul_nil;
    else {
      EXTERNAL_POPVAL1(res);
      THREAD_RETURN_VALUE(thr) = res;
      THREAD_RETURNED(thr) = eul_true;
    }
  }
  else
    res = THREAD_RETURN_VALUE(thr);

  mutex_unlock(thr_join_mt);
  return res;
}

int eul_thr_join_all()
{
  thr_join(NULL, NULL, NULL);
}


///-----------------------------------------------------------------------------
/// eul_thr_yield()
/// Description: yield processor to a thread
/// Args: none
/// Returns: eul_nil
///-----------------------------------------------------------------------------
LispRef eul_thr_yield()
{
  thr_yield();
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

  thr_getspecific(eul_thr_key, (void **) &res);
  return res;
}

///-----------------------------------------------------------------------------
/// eul_thr_suspend()
/// Description: suspends the specified thread
/// Args: eul_nil or Lisp list with one Lisp thread
/// Returns: eul_true, if suspended successfully; eul_nil, otherwise
/// See also: eul_thr_continue()
///-----------------------------------------------------------------------------
LispRef eul_thr_suspend(LispRef thrs)
{
  thread_t thr_handle_struct, *thr_handle;

  if (thrs == eul_nil) {
    thr_handle_struct = thr_self();
    thr_handle = &thr_handle_struct;
  }
  else
    thr_handle = (thread_t *) THREAD_HANDLE(eul_car(thrs));

  /* Attempt to suspend the thread */
  if(thr_suspend(*thr_handle) == 0)
    return eul_true;
  else
    return eul_nil;
}


///-----------------------------------------------------------------------------
/// eul_thr_continue()
/// Description: resumes the execution of the specified (suspended) thread
/// Args: Lisp thread
/// Returns: thread, if thread continued successfully; otherwise eul_nil
/// See also: eul_thr_suspend()
///-----------------------------------------------------------------------------
LispRef eul_thr_continue(LispRef thr)
{
  thread_t *thr_handle;

  thr_handle = (thread_t *) THREAD_HANDLE(thr);

  /* Attempt to continue thread */
  if (thr_continue(*thr_handle) == 0)
    return thr;
  else
    return eul_nil;
}

///-----------------------------------------------------------------------------
/// eul_get_priority()
/// Description: return the priority of the specified thread
/// Args: thread handle
/// Returns: priority or eul_nil
///-----------------------------------------------------------------------------
LispRef eul_thr_get_priority(thread_t *thr_handle)
{
  int n;

  if (thr_getprio(*thr_handle, &n) == 0)
    return c_int_as_eul_fpi(n);
  else
    return eul_nil;
}

///-----------------------------------------------------------------------------
