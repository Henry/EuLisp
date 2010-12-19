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
;;; Title: Addition of n numbers, in parallel
;;;  Authors: Andreas Kind, Liam Wickins
;;;  Maintainer: Henry G. Weller
;;;  Compilation:
;;    youtoo padd -l level-1
;;;-----------------------------------------------------------------------------

(defmodule padd
  (syntax (syntax-1)
   import (level-1)
   export (p-add))

(defun p-add args
  ;; Expects at least two arguments
  (add-aux (spawn add-simple args)))

(defun spawn (fun args)
  (if (null? args)
      ()
    (let ((thrd (make <current-thread> function: fun)))
      (thread-start thrd (car args) (cadr args))
      (cons thrd (spawn fun (cddr args))))))

(defun add-aux (thrds)
  (if (null? (cdr thrds))
      (thread-value (car thrds))
    (add-aux (spawn add-wait thrds))))

(defun add-wait (thr1 thr2)
  (add-simple (thread-value thr1) (thread-value thr2)))

(defun add-simple (x1 x2)
  (format "computing ~a + ~a ...\n" x1 x2)
  (binary+ x1 x2))

;;;-----------------------------------------------------------------------------
;;; Testing
;;;-----------------------------------------------------------------------------
(defun test1 ()
  ;;((setter thread-concurrency) 16)
  (print (p-add 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16) nl))

(defun make-list (x)
  (if (zero? x)
      ()
    (cons x (make-list (- x 1)))))

(defun test2 ()
  (let (x)
    (while (progn
             (format "Add (power of 2): ") (flush)
             (setq x (read lispin () (eos-default-value)))
             (null? (eq x (eos-default-value))))
      ;;((setter thread-concurrency) x)
      (print (apply p-add (make-list x)) nl))))
(test2)

;;;-----------------------------------------------------------------------------
)  ;; End of module padd
;;;-----------------------------------------------------------------------------
