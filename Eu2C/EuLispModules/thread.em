;;; Copyright 1994-2010 Fraunhofer ISST
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'Eu2C'
;;;-----------------------------------------------------------------------------
;;
;;  Eu2C is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;;  Title: Lisp Part of thread implementation
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Jens Bimberg
;;;-----------------------------------------------------------------------------
(defmodule thread
  (import (thread-ii
           thread-i
           thread-b
           event-i
           (only (%signed-word-integer
                  <object>
                  %cast
                  %eq
                  <class>
                  %function
                  %void)
                 tail)
           (only (top-dynamic)
                 letcc)
           (only (eq
                  =)
                 compare)
           (only (t)
                 basic-list)
           (only (null
                  <null>)
                 basic-list-0)
           (only (<symbol>)
                 symbol)
           (only (<integer>)
                 integer)
           (only (apply
                  <function>)
                 function)
           (only (function-address)
                 function-i)
           (only (set-interval-timer)
                 timer)
           (only (<thread-already-started>
                  error
                  signal)
                 condition))
   syntax ((only (unless
                  when
                  cond)
                 syntax-0)
           (only (with-handler)
                 condition))
   export (<thread>
           threadp
           thread-reschedule
           current-thread
           thread-start
           thread-value
           wait)
   expose ((only (<thread-condition>
                  <thread-already-started>
                  <wrong-thread-continuation>)
                 condition)))

; module function to get the currently working thread

(%define-function (current-thread <thread>)
  ()
  (%cast <thread> (First The-Sequential-Set)))

; rescheduling the thread

(defun thread-reschedule ()
  (hold-threads)
  (unless (TL-single The-Sequential-Set)
          (let ((from (TL-leave-first The-Sequential-Set)))
            (TL-on-end The-Sequential-Set from)
            (thread-yield from (First The-Sequential-Set))))
  (cont-threads)
  ())

; preemptive rescheduling: the signal handler function

(defun preemptive-thread-reschedule ()
  (unless (%eq thread-schedule #%i0)      ; scheduling disabled
          (thread-reschedule)))

(%define-function (thread-life <object>)
  ((thread <thread>))
  (restore-dynamics (saved-dynamics thread)); initial values
  (cont-threads)
  (let/cc catcher
    (with-handler
     (lambda (condition continuation)
       (set-value thread condition)
       (set-state thread 'aborted)
       (catcher ()))
     (progn
       (set-value thread
                  (apply (function thread) (args thread)))
       (set-state thread 'finished))
     ))
  (hold-threads)
  (TL-leave-first The-Sequential-Set)
  (TL-queue-on-top The-Sequential-Set (waiters thread))
  (free-tl-waiters thread (tmp-locks thread))
  (m-thread (First The-Sequential-Set)))

(deflocal timer-started ()) ; the interval timer is still not started

(defun start-timer ()
  (when (null timer-started)
        ; start the timer to periodically switch between threads
        (set-interval-timer 'real   ;real-timer
                            50000   ;each 50000 microseconds
                            preemptive-thread-reschedule)
        (setq timer-started t)))

(%define-function (thread-start <thread>)
  ((thread <thread>) . keywords )
  (start-timer)
  (unless (eq (state thread) 'new)
          (error "Thread already started" <thread-already-started>
                 'current-thread (current-thread) 'thread thread))
  (set-args thread keywords)
  (hold-threads)
  (set-state thread 'running)
  (TL-on-end The-Sequential-Set thread)
  (set-m-thread thread (m-thread-create
                        (function-address thread-life) thread))
  (cont-threads)
  thread)

; a simple thread-start for internal use wich doesn't start the scheduler
(%define-function (start-keep-up-thread %void)
  ((thread <thread>))
  (set-args thread ())
  (set-state thread 'running)
  (TL-on-end The-Sequential-Set thread)
  (set-m-thread thread (m-thread-create
                        (function-address thread-life) thread)))

(%define-function (thread-value <object>)
  ((thread <thread>))
  (cond   ((%eq (state thread) 'finished)
           (value thread))
          ((%eq (state thread) 'aborted)
           (signal (value thread) ()))
          (t (progn
               (wait-in-T-LST (waiters thread))
               (thread-value thread)))))

(%define-function (thread-terminated <object>)
  ((thread <thread>))
  (cond   ((%eq (state thread) 'finished) t)
          ((%eq (state thread) 'aborted) t)
          (t ())))

(%define-function (thread-wait-aux <object>)
  ((thread <thread>) (timer <object>))
  (thread-reschedule)
  (cond   ((thread-terminated thread) thread)
          ((timeout-expired timer) ())
          (t (thread-wait-aux thread timer))))

;       (defmethod wait ((thread <thread>) (timeout <object>))
;               (cond   ((thread-terminated thread) thread)
;                       ((null timeout) ())
;                       ((eq timeout t)
;                        (progn (wait-in-T-LST (waiters thread)) thread))
;                       (t (thread-wait-aux thread (get-timer timeout)))))

(defmethod wait ((thread <thread>) (timeout <null>))
  (if (thread-terminated thread)
      thread
    ()))

(defmethod wait ((thread <thread>) (timeout <symbol>))
  (if (eq timeout t)
      (if (thread-terminated thread)
          thread
        (progn (wait-in-T-LST (waiters thread)) thread))
    () ; error: second argument must be t
    ))


;       (defmethod wait ((thread <thread>) (timeout <integer>))
;               (thread-wait-aux thread (get-timer timeout)))


; new kind of waiting for timer: the temporary lock

(%define-standard-class (tmp-lock <class>) <object>
  ((result
    reader result writer set-result default 'unknown)
   (end-time
    reader end-time writer set-end-time)
   (waiter type <thread>
           reader waiter keyword waiter)
   (next-on-thread
    reader next-on-thread writer set-next-on-thread)
   (next-in-time
    reader next-in-time writer set-next-in-time))
  constructor (make-tmp-lock waiter)
  representation pointer-to-struct
  allocation multiple-type-card)


;       (defun register-tl-on-thread (thread tl)
;               (set-next-on-thread tl (tmp-locks thread))
;               (set-tmp-locks thread tl))

(defun find-tl (on-thread tl-lst thread)
  (if (null tl-lst)
      (let ((tl (make-tmp-lock thread)))
        (hold-threads)
        (set-next-on-thread tl (tmp-locks on-thread))
        (set-tmp-locks on-thread tl)
        (cont-threads)
        tl)
    (if (eq (waiter tl-lst) thread)
        (progn (set-result tl-lst 'unknown) tl-lst)
      (find-tl on-thread (next-on-thread tl-lst) thread))))


(defun free-tl-waiters (thread tl-lst)
  (unless (null tl-lst)
          (when (eq (result tl-lst) 'unknown)
                (unless (eq (waiter tl-lst) thread)
                        (set-result tl-lst thread)
                        (TL-on-end The-Sequential-Set (waiter tl-lst))
                        (free-tl-waiters thread (next-on-thread tl-lst))))))

(defmethod wait ((thread <thread>) (timeout <integer>))
  (cond ((thread-terminated thread) thread)
        ((= timeout 0) ())
        (t (wait-time thread timeout))))


(%define-function (wait-time <object>)
  ((thread <thread>) (timeout <integer>))
  (let ((tl (find-tl thread (tmp-locks thread) (current-thread))))
    (hold-threads)
    (let ((this-thread (TL-leave-first The-Sequential-Set)))
      (set-end-time tl (get-timer timeout))
      (register-tl-on-timer tl)
      (thread-yield this-thread (First The-Sequential-Set)))
    (cont-threads)
    (result tl)))


; main thread and exit handling
; the current program is silently converted into a thread. To assure that
; all the other threads can proceed after the end of main there are 2 steps
; necessary. A normal exit (return from main) must set the main-thread to
; state finished and proceed. This is done with a call to on-exit.
; A not handled condition received by main must set the state of main to
; aborted and proceed as well. This is done with setting a top level signal
; handler for the main thread

; (unfortunately, on-exit should not be used for portability reasons)

(%declare-external-function (on-exit %void)
  ((fun %function) argv)
  language c external-name |on_exit|)

(%declare-external-function (c-exit %void)
  ((value %signed-word-integer))
  language c external-name |exit|)

(deflocal main-thread (make-thread (%cast <function> #%i0)))
(set-state main-thread 'running)
(set-m-thread main-thread (m-thread-init))
(TL-on-top The-Sequential-Set main-thread)

(%define-function (catch-remaining-threads %void)
  ((status %signed-word-integer) argv)
  (hold-threads)
  (when (TL-single The-Sequential-Set)
        ;; main did not really end, deadlock!!, exit again
        (c-exit #%i3))
  (when (eq (state main-thread) 'running)
        (set-state main-thread 'finished)
        (set-value main-thread (%cast <object> status)))
  (TL-leave-first The-Sequential-Set)
  (TL-queue-on-top The-Sequential-Set (waiters main-thread))
  (free-tl-waiters main-thread (tmp-locks main-thread))
  (m-thread-yield (m-thread (First The-Sequential-Set))))

(on-exit (function-address catch-remaining-threads) ())

(defun normal-exit ()
  (c-exit (%cast %signed-word-integer (value main-thread)))
  ()) ; for TI

(defun main-signal-handler (condition continuation)
  (hold-threads)
  (set-state main-thread 'aborted)
  (set-value main-thread condition)
  (c-exit #%i1)
  ())     ; return NIL for type inference

(dynamic-setq dynamic-default-signal-handler main-signal-handler)

; we start one thread to stay in the SQS up to the time there's no other;
; in that case it will exit the program. This is a hack to exempt us from
; keeping trace of empty SQS whenever a thread does leave it

;       (defun keep-up ()
;               (when (TL-single The-Sequential-Set)
;                       (normal-exit))
;               (thread-reschedule)
;               (keep-up))
;
;       (thread-start (make-thread keep-up))

; for handling of temporary waiting threads the before function/thread is
; expanded to handle a list of tmp-lock's, the time-queue

(deflocal time-queue ())


(defun register-tl-on-timer (tl)
  (if (null time-queue)
      (progn
        (set-next-in-time tl ())
        (setq time-queue tl))
    (if (time-lt (end-time tl) (end-time time-queue))
        (progn
          (set-next-in-time tl time-queue)
          (setq time-queue tl))
      (insert-tl tl time-queue))))

(defun insert-tl (tl queue)
  (if (null (next-in-time queue))
      (progn
        (set-next-in-time tl ())
        (set-next-in-time queue tl))
    (if (time-lt (end-time tl) (end-time queue))
        (progn
          (set-next-in-time tl (next-in-time queue))
          (set-next-in-time queue tl))
      (insert-tl tl (next-in-time queue)))))

(defun keep-up ()
  (hold-threads)
  (when (busy-tmp-locks)
        (free-expired-waiters (get-this-time)))
  (if (TL-single The-Sequential-Set)
      (if (busy-tmp-locks)
          (progn (sleep-until (end-time time-queue)) (keep-up))
        (normal-exit))
    (progn (cont-threads) (thread-reschedule) (keep-up))))

(defun busy-tmp-locks ()
  (if (null time-queue)
      ()
    (if (eq (result time-queue) 'unknown)
        t
      (progn
        (setq time-queue (next-in-time time-queue))
        (busy-tmp-locks)))))

(defun free-expired-waiters (this-time)
  (unless (null time-queue)
          (when (time-lt (end-time time-queue) this-time)
                (when (eq (result time-queue) 'unknown)
                      (set-result time-queue ())
                      (TL-on-end The-Sequential-Set (waiter time-queue)))
                (setq time-queue (next-in-time time-queue))
                (free-expired-waiters this-time))))

(start-keep-up-thread (make-thread keep-up))
)
