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
;;; Title: Towers of Hanoi
;;;  Description:
;;    Example use of classes in the algorithm to solve the "Towers of Hanoi"
;;    problem.
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;  Compilation
;;    youtoo hanoi -l level-0
;;;  EuXLisp Interpretation:
;;    (!> hanoi)
;;;  Youtoo Interpretation:
;;    : hanoi
;;;-----------------------------------------------------------------------------

(defmodule hanoi
  (syntax (syntax-0)
   import (level-0)
   export (hanoi))

;;;-----------------------------------------------------------------------------
;;; Tower definition
;;;-----------------------------------------------------------------------------
(defconstant *max-tower-height* 10)

(defclass <tower> ()
  ((id reader: tower-id keyword: id:)
   (blocks accessor: tower-blocks)))

(defun build-tower (x n)
  (letfuns ((loop (i res)
                 (if (= i 0) res
                   (loop (- i 1) (cons i res)))))
          ((setter tower-blocks) x (loop n ()))
          x))

(defmethod generic-print ((x <tower>) (s <stream>))
  (sformat s "#<tower ~a: ~a>" (tower-id x) (tower-blocks x)))

;;;-----------------------------------------------------------------------------
;;; Access to tower blocks
;;;-----------------------------------------------------------------------------
(defgeneric push (x y))

(defmethod push ((x <tower>) (y <fpi>))
  (let ((blocks (tower-blocks x)))
    (if (or (null? blocks) (< y (car blocks)))
        ((setter tower-blocks) x (cons y blocks))
      (error <condition>
             (fmt "cannot push block of size ~a on tower ~a" y x)))))

(defgeneric pop (x))

(defmethod pop ((x <tower>))
  (let ((blocks (tower-blocks x)))
    (if blocks
        (progn
          ((setter tower-blocks) x (cdr blocks))
          (car blocks))
      (error <condition>
             (fmt "cannot pop block from emtpy tower ~a" x)))))

;;;-----------------------------------------------------------------------------
;;; Move n blocks from tower x1 to tower x2 using x3 as buffer
;;;-----------------------------------------------------------------------------
(defgeneric move (n x1 x2 x3))

(defmethod move ((n <fpi>) (x1 <tower>) (x2 <tower>) (x3 <tower>))
  (if (= n 1)
      (progn
        (push x2 (pop x1))
        (print x1 nl x2 nl x3 nl nl))
    (progn
      (move (- n 1) x1 x3 x2)
      (move 1 x1 x2 x3)
      (move (- n 1) x3 x2 x1))))

;;;-----------------------------------------------------------------------------
;;; Initialize and run the 'Towers of Hanoi'
;;;-----------------------------------------------------------------------------
(defun hanoi ()
  (let ((x1 (make <tower> id: 0))
        (x2 (make <tower> id: 1))
        (x3 (make <tower> id: 2)))
    (build-tower x1 *max-tower-height*)
    (build-tower x2 0)
    (build-tower x3 0)
    (print x1 nl x2 nl x3 nl nl)
    (move *max-tower-height* x1 x2 x3)))

(hanoi)

;;;-----------------------------------------------------------------------------
)  ;; End of module hanoi
;;;-----------------------------------------------------------------------------
