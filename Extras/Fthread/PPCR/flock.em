;;; Copyright (c) 1996 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: fthread (foreign thread interface)
;;;  Authors: Andreas Kind, Liam Wickins
;;; Description: PPCR locks for EuLisp
;;;-----------------------------------------------------------------------------
(defmodule flock
  (syntax (macros)
   import (level1)
   export (<ppcr-lock> ppcr-lock? <thread-lock> thread-lock?))

;;;-----------------------------------------------------------------------------
;;; Lock classes
;;  EuLisp locks cannot be implemented with foreign mutexes because in
;;  order to unlock a foreign PPCR mutex (and this different in EuLisp) the
;;  mutex  must be locked and the calling thread must be the one
;;  that last locked the mutex (the owner).
;;
;;  Locks with PPCR semantics are provided with class <thread-lock>.
;;;-----------------------------------------------------------------------------
  (defclass <ppcr-lock> ()
    ((handle accessor: lock-handle))
    predicate: ppcr-lock?)

  ;; PPCR locks are now default
  (setq <lock> <ppcr-lock>)

  (defmethod initialize ((lk <ppcr-lock>) keywords)
    (call-next-method)
    ((setter lock-handle) lk (eul_bsema_create))
    lk)

  (defclass <thread-lock> ()
    (handle)
    predicate: thread-lock?)

  (defmethod initialize ((lk <thread-lock>) keywords)
    (call-next-method)
    ((setter lock-handle) lk (eul_lock_create))
    lk)

  (defextern eul_lock_create () ptr)

  (defextern eul_bsema_create () ptr)

;;;-----------------------------------------------------------------------------
;;; lock
;;;-----------------------------------------------------------------------------
  (defmethod lock ((lk <ppcr-lock>))
    (eul_bsema_lock (lock-handle lk))
    lk)

  (defmethod lock ((lk <thread-lock>))
    (eul_lock (lock-handle lk))
    lk)

  (defextern eul_bsema_lock (ptr) ptr)

  (defextern eul_lock (ptr) ptr "PCR_Th_ML_Acquire")

;;;-----------------------------------------------------------------------------
;;; unlock
;;;-----------------------------------------------------------------------------
  (defmethod unlock ((lk <ppcr-lock>))
    (eul_bsema_unlock (lock-handle lk))
    lk)

  (defmethod unlock ((lk <thread-lock>))
    (eul_unlock (lock-handle lk))
    lk)

  (defextern eul_bsema_unlock (ptr) ptr)

  (defextern eul_unlock (ptr) ptr "PCR_Th_ML_Release")

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
