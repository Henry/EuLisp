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
;;;  Library: fthread (foreign thread interface)
;;;  Authors: Andreas Kind, Liam Wickins
;;; Description: UI (alias Solaris) threads for EuLisp
;;;-----------------------------------------------------------------------------
(defmodule fthread
  (syntax (macros)
   import ((except (thread-return-value thread-returned?) level1))
   expose (flock fcsem)
   export (<ui-thread> ui-thread? thread-continue
                       thread-concurrency thread-priority thread-join-all))

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
  predicate: ui-thread?)

(defmethod initialize ((thrd <ui-thread>) inits)
  (call-next-method)
  (let ((fun (init-list-ref inits function:)))
    (if (function? fun)
        ((setter thread-function) thrd fun)
      (error <condition> "missing required keyword ~a" function:))
    thrd))

;;;-----------------------------------------------------------------------------
;;; Set default thread class (could be <solaris-thread>, <posix-thread>,
;; <ui-thread> or <simple-thread>)
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

(defextern thread-join-all () <int> "eul_thr_join_all")

;;;-----------------------------------------------------------------------------
;;; Thread suspend
;;;-----------------------------------------------------------------------------
(setq thread-suspend (lambda thrds (eul_thr_suspend thrds)))

(defextern eul_thr_suspend (ptr) ptr)

;;;-----------------------------------------------------------------------------
;;; Thread continue
;;;-----------------------------------------------------------------------------
(defun thread-continue (thrd) (eul_thr_continue thrd))

(defextern eul_thr_continue (ptr) ptr)

;;;-----------------------------------------------------------------------------
;;; Thread concurrency
;;;-----------------------------------------------------------------------------
(defun thread-concurrency () (eul_thr_get_concurrency))
(defun (setter thread-concurrency) (x) (eul_thr_set_concurrency x))
(defextern eul_thr_get_concurrency () <int> "thr_getconcurrency")
(defextern eul_thr_set_concurrency (<int>) ptr "thr_setconcurrency")

;;;-----------------------------------------------------------------------------
;;; Thread priority
;;;-----------------------------------------------------------------------------
(defun thread-priority (thrd)
  (eul_thr_get_priority (thread-handle thrd)))

(defun (setter thread-priority) (thrd x)
  (eul_thr_set_priority (thread-handle thrd) x))

(defextern eul_thr_get_priority (ptr) <int>)

(defextern eul_thr_set_priority (ptr <int>) <int> "thr_setprio")

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
