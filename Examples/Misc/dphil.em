;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: Misc
;;;  Authors: Andreas Kind, Chris Murphy
;;; Description: dining philosophers
;;;  Compilation (default)
;;    youtoo dphil -l level1
;;;  Compilation (with foreign thread library)
;;    youtoo dphil -l level1 -l fthread
;;;-----------------------------------------------------------------------------
(defmodule dphil
  (syntax (macros)
   import (level1 csem)           ;; default
   ;; import (level1 fthread)     ;; with foreign thread library
   )

;;;-----------------------------------------------------------------------------
;;; Initialization
;;;-----------------------------------------------------------------------------
  (defconstant *no-philosophers* 10)
  (defconstant *no-meals* 100)
  (defconstant *no-chopsticks* 5)
  (deflocal *dining-room* (make <csemaphore> counter: 4))
  (deflocal *chopsticks* (make-vector *no-chopsticks*))

  (defun initialize-dining-philosophers ()
    (labels
     ((loop (i)
            (if (< i 5)
                (let ((lk (make <lock>)))
                  ((setter vector-ref) *chopsticks* i lk)
                  (loop (+ i 1)))
              ())))
     (print "initializing ...")
     (loop 0)))

;;;-----------------------------------------------------------------------------
;;; Perform dinner of one philosopher
;;;-----------------------------------------------------------------------------
  (defun philosopher (id)
    (labels
     ((loop (i)
            (if (< i *no-meals*)
                (progn
                  (think id)
                  (reschedule-check)
                  (cwait *dining-room*)
                  (format "    ~a entered room for meal ~a\n" id i)
                  (take-chopsticks id)
                  (reschedule-check)
                  (eat id i)
                  (reschedule-check)
                  (drop-chopsticks id)
                  (format "    ~a leaving room after meal ~a\n" id i)
                  (csignal *dining-room*)
                  (reschedule-check)
                  (loop (+ i 1)))
              ())))
     (format "--> ~a appears\n" id)
     (loop 0)
     (format "<-- ~a going home\n" id)))

  (defun eat (id i)
    (format "@~a  ~a is eating\n" i id)
    (reschedule-check))

  (defun think (id)
    (format "    ~a is thinking\n" id)
    (reschedule-check))

  (defun take-chopsticks (id)
    (let ((cs1 (% (+ id 1) *no-chopsticks*))
          (cs2 (% id *no-chopsticks*)))
      (lock (vector-ref *chopsticks* cs1))
      (format "+   ~a got one chopstick (~a)\n" id cs1)
      (lock (vector-ref *chopsticks* (% id *no-chopsticks*)))
      (format "++  ~a got other chopstick (~a)\n" id cs2)))

  (defun drop-chopsticks (id)
    (let ((cs1 (% id *no-chopsticks*))
          (cs2 (% (+ id 1) *no-chopsticks*)))
      (unlock (vector-ref *chopsticks* cs1))
      (format "--  ~a droped one chopstick (~a)\n" id cs1)
      (unlock (vector-ref *chopsticks* cs2))
      (format "-   ~a droped other chopstick (~a)\n" id cs2)))

  (defun reschedule-check ()
    (if (random-true-nil)
        (thread-reschedule)
      ()))

  (defun start-dining-philosophers ()
    (labels
     ((create-threads
        (n l)
        (if (= n 0) l
          (create-threads
            (- n 1)
            (cons (make <current-thread> function: philosopher) l))))
      (start-threads (i l)
                     (if (null? l) ()
                       (progn
                         (thread-start (car l) i)
                         (start-threads (+ i 1) (cdr l))))))
     (print "Creating threads ...")
     (let ((thrds (create-threads *no-philosophers* ())))
       (print "Starting threads ...")
       (start-threads 1 thrds)
       (print "Scheduling threads ...")
       ;;     (while (current-thread-queue) (thread-reschedule))
       ;;     (thread-join-all)
       (do thread-value thrds)
       )
     (format "Philosophers 1 to ~a had each ~a meals.\n"
             *no-philosophers* *no-meals*)))

  (initialize-dining-philosophers)

  (start-dining-philosophers)

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
