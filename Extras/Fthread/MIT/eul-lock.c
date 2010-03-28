
/** Copyright (c) 1996 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: fthread (foreign thread interface)
 **  Authors: Andreas Kind
 **  Description: Foreign function support for locks in MIT threads
 ** ----------------------------------------------------------------------- **/

#include <eulisp.h>
#include <pthread.h>
#include <gc.h>

/** ----------------------------------------------------------------- **
 ** eul_lock_create()
 **
 ** Description: create a UI mutex
 ** Args: none
 ** Returns: lock handler or eul_nil on failure
 ** ----------------------------------------------------------------- **/

LispRef eul_lock_create()
{
  pthread_mutex_t *mt;

  mt = (pthread_mutex_t *) gc_malloc(sizeof(pthread_mutex_t));

  /* Initialise a new mutex */
  if(pthread_mutex_init(mt, NULL) == 0)
    return (LispRef) mt;
  else
    return eul_nil;
}

/** ----------------------------------------------------------------- **
 ** eul_sem_create()
 **
 ** Description: create a UI semaphore
 ** Args: count
 ** Returns: semaphore handler or eul_nil on failure
 ** ----------------------------------------------------------------- **/


/* LispRef eul_sema_create(int count) */
/* { */
/*   sema_t *sem; */

/*   sem = (sema_t *) gc_malloc(sizeof(sema_t)); */

  /* Initialise a new semaphore */
/*   if(sema_init(sem, count, USYNC_THREAD, NULL) == 0) */
/*     return (LispRef) sem; */
/*   else */
/*     return eul_nil; */
/* } */


/* eof */
