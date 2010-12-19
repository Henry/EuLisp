;;; Copyright 1997 A. Kind & University of Bath
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
;;; Title: Dining philosophers example
;;;  Authors: Andreas Kind, Chris Murphy
;;;  Maintainer: Henry G. Weller
;;;  Compilation (default)
;;    youtoo dphil -l level-1
;;;  Compilation (with foreign thread library)
;;    youtoo dphil -l level-1 -l fthread
;;;-----------------------------------------------------------------------------

(defmodule dphil
  (syntax (syntax-1)
   import (level-1
           csem))

;;;-----------------------------------------------------------------------------
;;; Initialization
;;;-----------------------------------------------------------------------------
(defconstant *no-philosophers* 10)
(defconstant *no-meals* 100)
(defconstant *no-chopsticks* 5)
(deflocal *dining-room* (make <csemaphore> counter: 4))
(deflocal *chopsticks* (make-vector *no-chopsticks*))

(defun initialize-dining-philosophers ()
  (letfuns
   ((loop (i)
          (if (< i 5)
              (let ((lk (make <lock>)))
                ((setter vector-ref) *chopsticks* i lk)
                (loop (+ i 1)))
            ())))
   (print "initializing ..." nl)
   (loop 0)))

;;;-----------------------------------------------------------------------------
;;; Perform dinner of one philosopher
;;;-----------------------------------------------------------------------------
(defun philosopher (id)
  (letfuns
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
  (letfuns
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
   (print "Creating threads ..." nl)
   (let ((thrds (create-threads *no-philosophers* ())))
     (print "Starting threads ..." nl)
     (start-threads 1 thrds)
     (print "Scheduling threads ..." nl)
     ;;     (while (current-thread-queue) (thread-reschedule))
     ;;     (thread-join-all)
     (do thread-value thrds)
     )
   (format "Philosophers 1 to ~a had each ~a meals.\n"
           *no-philosophers* *no-meals*)))

(initialize-dining-philosophers)

(start-dining-philosophers)

;;;-----------------------------------------------------------------------------
)  ;; End of module dphil
;;;-----------------------------------------------------------------------------
