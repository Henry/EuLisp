;;; Copyright (c) 1996 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: fthread (foreign thread interface)
;;;  Authors: Andreas Kind, Liam Wickins
;;; Description: PPCR threads for EuLisp
;;;  Compilation
;;    youtoo -ar fthread -l level1
;;    make
;;;-----------------------------------------------------------------------------
(defmodule fthread
  (syntax (macros)
   import (level1)
   expose (flock)
   export (<ppcr-thread> ppcr-thread-p))

;;;-----------------------------------------------------------------------------
;;; Class <ppcr-thread>
;;;-----------------------------------------------------------------------------
  (defclass <ppcr-thread> (<abstract-thread>)
    ((function accessor: thread-function)
     (returned accessor: thread-returned?)
     (return-value accessor: thread-return-value)
     ;; Next slots only accessed from C
     return-mutex
     handle
     registers)
    predicate: ppcr-thread-p)

  (defmethod initialize ((thrd <ppcr-thread>) inits)
    (call-next-method)
    (let ((fun (init-list-ref inits function:)))
      (if (functionp fun)
          ((setter thread-function) thrd fun)
        (error "missing required keyword ~a" function:))
      thrd))

;;;-----------------------------------------------------------------------------
;;; Set default thread class (could be <ui-thread>, <posix-thread>,
;; <ppcr-thread> or <simple-thread>)
;;;-----------------------------------------------------------------------------
  ;; The default thread class
  (setq <thread> <ppcr-thread>)

  (deflocal *initial-thread* (make <ppcr-thread> function: (lambda ())))

  (initialize-foreign-threads *initial-thread*)

  (defextern initialize-foreign-threads (ptr) ptr
    "eul_initialize_foreign_threads")

;;;-----------------------------------------------------------------------------
;;; Current thread
;;;-----------------------------------------------------------------------------
  (setq current-thread (lambda () (eul_thr_self)))

  (defextern eul_thr_self () ptr)

;;;-----------------------------------------------------------------------------
;;; Thread reschedule
;;;-----------------------------------------------------------------------------
  (setq thread-reschedule
        (lambda thrds
          (eul_thr_yield (if thrds (car thrds) ()))))

  (defextern eul_thr_yield (ptr) ptr)

;;;-----------------------------------------------------------------------------
;;; Thread start
;;;-----------------------------------------------------------------------------
  (defmethod thread-start ((thrd <ppcr-thread>) . args)
    (eul_thr_create thrd args))

  (defextern eul_thr_create (ptr ptr) ptr)

;;;-----------------------------------------------------------------------------
;;; Thread value
;;;-----------------------------------------------------------------------------
  (defmethod thread-value ((thrd <ppcr-thread>))
    (eul_thr_join thrd))

  (defextern eul_thr_join (ptr) ptr)

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
