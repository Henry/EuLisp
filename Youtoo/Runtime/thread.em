;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Keith Playford, Andreas Kind
;;; Description: non-preemptive threads
;;;    Foreign thread modules need to set the default thread class variable
;;;    <current-thread> (e.g. to <posix-thread>) and reset the following
;;;    functions:
;;;        current-thread current-thread-queue thread-suspend thread-reschedule
;;;-----------------------------------------------------------------------------

(defmodule thread
  (syntax (_macros)
   import (telos
           event)
   export (<current-thread>
           <thread> thread? <simple-thread> simple-thread?
           thread? current-thread current-thread-queue thread-state
           thread-continuation thread-returned? thread-return-value
           thread-reschedule thread-suspend
           thread-block thread-unblock
           thread-start thread-value tconc
           thread-dynamic-variables thread-error-handlers
           <state> state-value-stack state-value-stack-size
           state-context-stack state-context-stack-size
           fill-thread-state restore-thread-state
           fill-simple-state restore-simple-state call1/cc))

;;;-----------------------------------------------------------------------------
;;; Classes <thread> and <simple-thread>
;;;-----------------------------------------------------------------------------
(defclass <thread> ()
  ((error-handlers
    accessor: thread-error-handlers
    keyword: handlers:)
   (dynamic-variables
    accessor: thread-dynamic-variables
    ;; Dynamic var *clean-ups* used by unwind-protect and let/cc
    ;; Use a new list for each thread, so that
    ;;     ((setter cdr) (thread-local-variables thrd) '(d d-val))
    ;; doesn't change the dynamic variables of *every* thread.
    default: (list '*clean-ups* ()))
   keyword: dynamic-variables:)
  abstract?: t
  keywords: (function:)
  predicate: thread?)

(defclass <simple-thread> <thread>
  ((continuation accessor: thread-continuation)
   (state accessor: thread-state)
   (returned accessor: thread-returned?)
   (return-value accessor: thread-return-value))
  predicate: simple-thread?)

;;;-----------------------------------------------------------------------------
;;; Initialization
;;;-----------------------------------------------------------------------------
;; The default thread class
(deflocal <current-thread> <simple-thread>)
(deflocal *current-thread* ())
(deflocal current-thread (lambda () *current-thread*))

(let ((thrd (make <simple-thread>)))
  ;; Must be before (defmethod initialize ((<simple-thread> thrd) inits)
  ((setter thread-state) thrd 'running)
  (setq *current-thread* thrd))

(defmethod initialize ((thrd <simple-thread>) inits)
  (call-next-method)
  (let ((fun (init-list-ref inits function:)))
    (if (function? fun) ()
      (error () "missing required keyword ~a" function:))
    (call1/cc
     (lambda (k)
       (let ((args (call1/cc
                    (lambda (new-k)
                      ((setter thread-continuation) thrd new-k)
                      ((setter thread-state) thrd 'limbo)
                      (k thrd)))))
         (^ thread-return (apply fun args)))))))

;;;-----------------------------------------------------------------------------
;;; Start thread
;;;-----------------------------------------------------------------------------
(defgeneric thread-start (thrd))

(defmethod thread-start ((thrd <simple-thread>) . args)
  (thread-enable (thread-feed thrd args))
  thrd)

(defun thread-feed (thrd x)
  (call1/cc
   (lambda (k)
     (let ((old-k (thread-continuation thrd)))
       (call1/cc
        (lambda (new-k)
          ((setter thread-continuation) thrd new-k)
          (k thrd)))
       (old-k x)))))

(defun thread-enable (thrd)
  ((setter thread-state) thrd 'ready)
  (thread-queue-append thrd))

(defun thread-run (thrd)
  (setq *current-thread* thrd)
  ((setter thread-state) thrd 'running)
  ((thread-continuation thrd) thrd))

;;;-----------------------------------------------------------------------------
;;; Scheduler
;;;-----------------------------------------------------------------------------
(defun ^ (fun . args)
  ;; Runs a function on a "scheduler" thread
  (call1/cc
   (lambda (k)
     (let ((thrd *current-thread*))
       ((setter thread-continuation) thrd k)
       (setq *current-thread* ())
       ;; On another thread notionally...
       (thread-run (apply fun thrd args))))))

;;;-----------------------------------------------------------------------------
;;; Return value
;;;-----------------------------------------------------------------------------
(defgeneric thread-value (thrd))

(defmethod thread-value ((thrd <simple-thread>))
  (if (thread-returned? thrd)
      (thread-return-value thrd)
    (progn
      (thread-reschedule)
      (thread-value thrd))))

(defun thread-return (thrd x)
  ((setter thread-return-value) thrd x)
  ((setter thread-returned?) thrd t)
  ((setter thread-state) thrd 'returned)
  (thread-queue-remove))

;;;-----------------------------------------------------------------------------
;;; Suspension
;;;-----------------------------------------------------------------------------
(deflocal thread-suspend
  (lambda thrds
    (if thrds
        (let ((thrd (car thrds)))
          ((setter thread-state) thrd 'limbo)
          (thread-queue-remove))
      (^ thread-suspend))))

;;;-----------------------------------------------------------------------------
;;; Rescheduling
;;;-----------------------------------------------------------------------------
(deflocal thread-reschedule
  (lambda thrds
    (if thrds
        (let ((thrd (car thrds)))
          ((setter thread-state) thrd 'ready)
          (thread-queue-append thrd)
          (thread-queue-remove))
      (^ thread-reschedule))))

;;;-----------------------------------------------------------------------------
;;; Blocking
;;;-----------------------------------------------------------------------------
(defgeneric thread-block (thrd))

(defmethod thread-block ((thrd <simple-thread>))
  ((setter thread-state) thrd 'blocked)
  (thread-queue-remove))

(defgeneric thread-unblock (thrd x))

(defmethod thread-unblock ((thrd <simple-thread>) x)
  (thread-enable (thread-feed thrd x)))

;;;-----------------------------------------------------------------------------
;;; Thread queue
;;;-----------------------------------------------------------------------------
(deflocal *thread-queue* ())
(deflocal current-thread-queue (lambda () *thread-queue*))

(defun thread-queue-append (thrd)
  (if (null? *thread-queue*)
      (setq *thread-queue* (cons thrd ()))
    (tconc *thread-queue* thrd)))

(defun thread-queue-remove ()
  (if (null? *thread-queue*)
      (error () "empty thread queue")
    (let ((thrd (car *thread-queue*)))
      (setq *thread-queue* (cdr *thread-queue*))
      thrd)))

(defun thread-queue-remove-last ()
  (let ((ll (reverse-list *thread-queue*)))
    (if (null? ll)
        (error () "empty thread queue")
      (let ((thrd (car ll)))
        (setq *thread-queue* (reverse-list (cdr ll)))
        thrd))))

(defun tconc (l x)
  (labels
   ((loop (ll)
          (if (null? (cdr ll))
              ((setter cdr) ll (cons x ()))
            (loop (cdr ll)))))
   (if (null? l)
       (setq l (cons x ()))  ; no side effect!
     (loop l))
   l))

;;;-----------------------------------------------------------------------------
;;; The state class and its primitives
;;;-----------------------------------------------------------------------------
(defclass <state> ()
  ((value-stack accessor: state-value-stack
                keyword: value-stack:)
   (value-stack-size accessor: state-value-stack-size
                     keyword: value-stack-size:)
   (context-stack accessor: state-context-stack
                  keyword: context-stack:)
   (context-stack-size accessor: state-context-stack-size
                       keyword: context-stack-size:))
  predicate: state?)

(defopencoded fill-simple-state (st) (fill-state))
(defopencoded restore-simple-state (st x) (restore-state))
(defopencoded fill-thread-state (st) (fill-thread-state))
(defopencoded restore-thread-state (st x) (restore-thread-state))

;;;-----------------------------------------------------------------------------
;;; Call-once call/cc
;;;-----------------------------------------------------------------------------
(defun call1/cc-aux (k st)
  (fill-thread-state st)
  (let ((res (k (lambda (x)
                  (restore-thread-state st x)))))
    (if res res ())))

(defun call1/cc (k)
  (let* ((st (make <state>))
         (res (call1/cc-aux k st)))
    ;; call1/cc-aux must not be in tail position
    (if res res ())))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
