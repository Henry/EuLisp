
/**           Copyright (c) 1996 by L Wickins & University of Bath.
 **                            All rights reserved.                         **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: solaris-semaphore
 **  Authors: Liam Wickins, Andreas Kind
 **  Description: Foreign function support for solaris locks
 ** ----------------------------------------------------------------------- **/

#include <eulisp.h>
#include <th/PCR_Th.h>
#include <th/PCR_ThTypes.h>

/** ----------------------------------------------------------------- **
 ** eul_lock_create()
 **
 ** Description: Creates a PPCR mutex
 ** Args: none
 ** Returns: lock handle
 ** ----------------------------------------------------------------- **/

PCR_Th_ML *eul_lock_create()
{
  PCR_Th_ML *lock;

  lock = (PCR_Th_ML *) gc_malloc(sizeof(PCR_Th_ML));
  PCR_Th_ML_Init(lock);

  return lock;
}

/** ----------------------------------------------------------------- **
 ** eul_bsema_create()
 **
 ** Description: create a binary semaphore
 ** Args: none
 ** Returns: semaphore handler
 ** ----------------------------------------------------------------- **/

typedef struct PCR_Th_BSema_Struct {
  PCR_Th_ML *ml;
  PCR_Th_CV *cv;
} PCR_Th_BSema;

PCR_Th_BSema *eul_bsema_create()
{
  PCR_Th_BSema *bsem_handle;
  PCR_Th_ML *ml;
  PCR_Th_CV *cv;

  bsem_handle = (PCR_Th_BSema *) gc_malloc(sizeof(PCR_Th_BSema));
  PCR_Th_ML_Init(ml);
  PCR_Th_CV_Init(cv);
  bsem_handle->ml = ml;
  bsem_handle->cv = cv;

  return bsem_handle;
}

/** ----------------------------------------------------------------- **
 ** eul_bsema_lock()
 **
 ** Description: lock a binary semaphore
 ** Args: semaphore handler
 ** Returns: semaphore handler
 ** ----------------------------------------------------------------- **/

LispRef eul_bsema_lock(PCR_Th_BSema *bsem_handle)
{
  PCR_Th_ML *ml;
  PCR_Th_CV *cv;
  PCR_ERes status;

  ml = bsem_handle->ml;
  cv = bsem_handle->cv;

  PCR_Th_ML_Acquire(ml);
  status = PCR_Th_CV_Wait(cv, ml, PCR_allSigsBlocked, PCR_waitForever);
  PCR_Th_ML_Release(ml);

  if (status == PCR_ERes_okay)
    return (LispRef) bsem_handle;
  else
    return eul_nil;
}

/** ----------------------------------------------------------------- **
 ** eul_bsema_unlock()
 **
 ** Description: unlock a binary semaphore
 ** Args: semaphore handler
 ** Returns: semaphore handler
 ** ----------------------------------------------------------------- **/

PCR_Th_BSema *eul_bsema_unlock(PCR_Th_BSema *bsem_handle)
{
  PCR_Th_ML *ml;
  PCR_Th_CV *cv;

  ml = bsem_handle->ml;
  cv = bsem_handle->cv;

  PCR_Th_ML_Acquire(ml);
  PCR_Th_CV_StickyNotify(cv);
  PCR_Th_ML_Release(ml);
}




/* eof */
