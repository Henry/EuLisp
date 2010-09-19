;;; Copyright (c) 1996 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: fthread (foreign thread interface)
;;;  Authors: Andreas Kind, Liam Wickins
;;; Description: UI locks for EuLisp
;;;-----------------------------------------------------------------------------
(defmodule fcsem
  (syntax (macros)
   import (level1)
   export (<csemaphore> csemaphorep cwait csignal))

;;;-----------------------------------------------------------------------------
;;; Class <csemaphore>
;;;-----------------------------------------------------------------------------
(defclass <csemaphore> ()
  ((handle accessor: csemaphore-handle))
  predicate: csemaphorep
  keywords: (counter:))

(defmethod initialize ((csem <csemaphore>) inits)
  (call-next-method)
  (let ((n (init-list-ref inits counter:)))
    (if (integer? n)
        ((setter csemaphore-handle) csem (eul_sema_create n))
      ;; Basically a lock
      ((setter csemaphore-handle) csem (eul_sema_create 1)))
    csem))

(defextern eul_sema_create (<int>) ptr)

;;;-----------------------------------------------------------------------------
;;; Wait
;;;-----------------------------------------------------------------------------
(defun cwait (csem)
  (eul_sema_wait (csemaphore-handle csem))
  csem)

(defextern eul_sema_wait (ptr) ptr "sema_wait")

;;;-----------------------------------------------------------------------------
;;; Signal
;;;-----------------------------------------------------------------------------
(defun csignal (csem)
  (eul_sema_signal (csemaphore-handle csem))
  csem)

(defextern eul_sema_signal (ptr) ptr "sema_post")

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
