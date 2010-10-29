;;; Copyright 1996 A. Kind & University of Bath
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;
;;  Youtoo is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: MIT threads (POSIX) for EuLisp
;;;  Library: fthread (foreign thread interface)
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;  Compilation
;;    youtoo -ar fthread -l level-1
;;    make
;;;-----------------------------------------------------------------------------

(defmodule fthread
  (syntax (macros)
   import (level-1)
   expose (flock
           fcsem)
   export (<ui-thread>
           ui-thread-p
           ;thread-concurrency
           thread-priority))

;;;-----------------------------------------------------------------------------
;;; Class <ui-thread>
;;;-----------------------------------------------------------------------------
(defclass <ui-thread> <thread>
  ((function accessor: thread-function)
   (returned accessor: thread-returned?)
   (return-value accessor: thread-return-value)
   ;; Next slots mainly accessed from C
   return-mutex
   (handle reader: thread-handle)
   registers)
  predicate: ui-thread-p)

(defmethod initialize ((thrd <ui-thread>) inits)
  (call-next-method)
  (let ((fun (init-list-ref inits function:)))
    (if (function? fun)
        ((setter thread-function) thrd fun)
      (error <condition> "missing required keyword ~a" function:))
    thrd))

;;;-----------------------------------------------------------------------------
;;; Set default thread class (could be <solaris-thread>, <posix-thread>,
;;; <ui-thread> or <simple-thread>)
;;;-----------------------------------------------------------------------------
;; The default thread class
(setq <current-thread> <ui-thread>)

(deflocal *initial-thread* (make <ui-thread> function: (lambda ())))

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
(defmethod thread-start ((thrd <ui-thread>) . args)
  (eul_thr_create thrd args))

(defextern eul_thr_create (ptr ptr) ptr)

;;;-----------------------------------------------------------------------------
;;; Thread value
;;;-----------------------------------------------------------------------------
(defmethod thread-value ((thrd <ui-thread>))
  (eul_thr_join thrd))

(defextern eul_thr_join (ptr) ptr)

;;;-----------------------------------------------------------------------------
;;; Thread concurrency
;;;-----------------------------------------------------------------------------
;(defun thread-concurrency () (eul_thr_get_concurrency))
;(defun (setter thread-concurrency) (x) (eul_thr_set_concurrency x))
;(defextern eul_thr_get_concurrency () <fpi> "thr_getconcurrency")
;(defextern eul_thr_set_concurrency (<fpi>) ptr "thr_setconcurrency")

;;;-----------------------------------------------------------------------------
;;; Thread priority
;;;-----------------------------------------------------------------------------
(defun thread-priority (thrd))
;  (eul_thr_get_priority (thread-handle thrd)))

;(defun (setter thread-priority) (thrd x)
;  (eul_thr_set_priority (thread-handle thrd) x))

;(defextern eul_thr_get_priority (ptr) <fpi>)
;(defextern eul_thr_set_priority (ptr <fpi>) <fpi> "thr_setprio")

;;;-----------------------------------------------------------------------------
)  ;; End of module fthread
;;;-----------------------------------------------------------------------------
