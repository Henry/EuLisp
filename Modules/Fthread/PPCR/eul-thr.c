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
///  Library: fthread
///  Authors: Andreas Kind, Liam Wickins
///  Description: Foreign threads based on POSIX Portable Common Runtime
///  Compilation: see fthread.em
///-----------------------------------------------------------------------------

#include <eulisp.h>
#include <th/PCR_Th.h>
#include <th/PCR_ThTypes.h>
#include <th/PCR_ThSpecific.h>

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

PCR_ThSpecific_Key eul_thr_key;

LispRef eul_initialize_foreign_threads(LispRef thr)
{
  PCR_ThSpecific_CreateKey(NIL, &eul_thr_key, NULL);

  /* Lisp thread should be accessable from the UI thread (see eul_thr_self) */
  PCR_ThSpecific_Set(eul_thr_key, thr);

  return eul_nil;
}

///-----------------------------------------------------------------------------
/// eul_thr_interpret()
/// Description: function actually running on a thread
/// Args: virtual machine registers
/// Returns: void
/// See also: eul_thr_create()
///-----------------------------------------------------------------------------

void eul_thr_interpret(LispRef thr)
{
  RegisterRef tame_regs;

  /* Lisp thread should be accessable from the UI thread (see eul_thr_self) */
  PCR_ThSpecific_Set(eul_thr_key, thr);

  tame_regs = (RegisterRef) THREAD_REGISTERS(thr);
  interpret(tame_regs);
  PCR_Th_Exit(0,(void *)0);
}

///-----------------------------------------------------------------------------
/// eul_thr_create()
/// Description: Allocates and sets up the interpreter stack for execution
///              of the initial function; creates the thread handle;
///              pushes the functions arguments on the stack and counts arity
/// Args: Lisp thread plus Lisp args
/// Returns: Lisp thread
///-----------------------------------------------------------------------------

LispRef eul_thr_create(LispRef thr, LispRef args)
{
  int arity = 0;
  PCR_Th_T *thr_handle;
  RegisterRef tame_regs;
  LispRef fn;
  PCR_Th_ML *thr_join_mt;

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

  /* Setup registers */
  tame_regs = gc_malloc(sizeof(RegisterSet));
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

  thr_join_mt = (PCR_Th_ML *) gc_malloc(sizeof(PCR_Th_ML));
  PCR_Th_ML_Init(thr_join_mt);
  THREAD_RETURN_MUTEX(thr) = (LispRef) thr_join_mt;

  /* Create the thread in a suspended state */
  THREAD_HANDLE(thr) =
    (LispRef) PCR_Th_Fork((void (*) (void *)) eul_thr_interpret, thr);

  return thr;
}


///-----------------------------------------------------------------------------
/// eul_thr_join()
/// Description: Awaits for the thread to complete, pops its return
///              value from the stack and returns it.
/// Args: Lisp thread
/// Returns: Lisp Object
///-----------------------------------------------------------------------------

LispRef eul_thr_join(LispRef thr)
{
  LispRef res;
  PCR_ERes status;
  PCR_Th_T *thr_handle;
  RegisterRef tame_regs;
  PCR_Th_ML *thr_join_mt;

  thr_handle = (PCR_Th_T *) THREAD_HANDLE(thr);
  tame_regs = (RegisterRef) THREAD_REGISTERS(thr);
  thr_join_mt = (PCR_Th_ML *) THREAD_RETURN_MUTEX(thr);

  /* Wait for target thread to complete */
  PCR_Th_ML_Acquire(thr_join_mt);

  if (THREAD_RETURNED(thr) == eul_nil) {
    status =
      PCR_Th_T_Join(thr_handle, NIL, NIL, PCR_allSigsBlocked, PCR_waitForever);
    if (status != PCR_ERes_okay)
      res = eul_nil;
    else {
      EXTERNAL_POPVAL1(res);
      THREAD_RETURN_VALUE(thr) = res;
      THREAD_RETURNED(thr) = eul_true;
    }
  }
  else
    res = THREAD_RETURN_VALUE(thr);

  PCR_Th_ML_Release(thr_join_mt);
  return res;
}


///-----------------------------------------------------------------------------
/// eul_thr_yield()
/// Description: yield processor to a thread
/// Args: Lisp thread or eul_nil
/// Returns: eul_nil
///-----------------------------------------------------------------------------

LispRef eul_thr_yield(LispRef thr)
{
  PCR_Th_T *thr_handle;

  if (thr == eul_nil)
    PCR_Th_T_YieldTo(NIL);
  else {
    thr_handle = (PCR_Th_T *) THREAD_HANDLE(thr);
    PCR_Th_T_YieldTo(thr_handle);
  }

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

  PCR_ThSpecific_Get(eul_thr_key, (void **) &res);
  return res;
}


///-----------------------------------------------------------------------------
