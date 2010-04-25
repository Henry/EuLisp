;;; Copyright (c) 1996 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: fthread (foreign thread interface)

;;;  Authors: Andreas Kind, Liam Wickins
;;; Description: UI locks for EuLisp
;;;  Compilation
;;    see fthread.em
;;;-----------------------------------------------------------------------------
(defmodule flock
  (syntax (macros)
   import (level1 fcsem)
   export (<thread-lock> thread-lock-p lock-handle))

;;;-----------------------------------------------------------------------------
;;; Lock classes
;;  EuLisp locks cannot be implemented with foreign mutexes because in
;;  order to unlock a foreign UI mutex (and this different in EuLisp) the
;;  mutex  must be locked and the calling thread must be the one
;;  that last locked the mutex (the owner).
;;
;;  Locks with UI semantics are provided with class <thread-lock>.
;;;-----------------------------------------------------------------------------
  ;; UI locks are now default
  (setq <lock> <csemaphore>)

  (defclass <thread-lock> ()
    ((handle accessor: lock-handle))
    predicate: thread-lock-p)

  (defmethod initialize ((lk <thread-lock>) keywords)
    (call-next-method)
    ((setter lock-handle) lk (eul_lock_create))
    lk)

  (defextern eul_lock_create () ptr)

;;;-----------------------------------------------------------------------------
;;; lock
;;;-----------------------------------------------------------------------------
  (defmethod lock ((lk <csemaphore>))
    (cwait lk))

  (defmethod lock ((lk <thread-lock>))
    (eul_lock (lock-handle lk))
    lk)

  (defextern eul_lock (ptr) ptr "mutex_lock")

;;;-----------------------------------------------------------------------------
;;; unlock
;;;-----------------------------------------------------------------------------
  (defmethod unlock ((lk <csemaphore>))
    (csignal lk))

  (defmethod unlock ((lk <thread-lock>))
    (eul_unlock (lock-handle lk))
    lk)

  (defextern eul_unlock (ptr) ptr "mutex_unlock")

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
