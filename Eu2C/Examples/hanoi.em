;;; Copyright 1997 A Kind & Univesity of Bath
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
;;; Title: Towers of Hanoi
;;;  Description:
;;    Example use of classes in the algorithm to solve the "Towers of Hanoi"
;;    problem.
;;;  Authors: Andreas Kind, Henry G. Weller
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule hanoi
  (import (level-0)
   syntax (syntax-0))

;;;-----------------------------------------------------------------------------
;;; Tower definition
;;;-----------------------------------------------------------------------------
(defconstant *max-tower-height* 10)

(defclass <tower> ()
  ((id reader tower-id keyword id:)
   (blocks accessor tower-blocks)))

(defun build-tower (x n)
  (labels ((loop (i res)
                 (if (= i 0) res
                   (loop (- i 1) (cons i res)))))
          ((setter tower-blocks) x (loop n ()))
          x))

(defmethod generic-print ((x <tower>) (s <stream>))
  (sformat s "#<tower ~a: ~a>" (tower-id x) (tower-blocks x)))

;;;-----------------------------------------------------------------------------
;;; Access to tower blocks
;;;-----------------------------------------------------------------------------
(defgeneric push ((x <object>) (y <object>)))

(defmethod push ((x <tower>) (y <int>))
  (let ((blocks (tower-blocks x)))
    (if (or (null? blocks) (< y (car blocks)))
        ((setter tower-blocks) x (cons y blocks))
      (error <condition> "cannot push block of size ~a on tower ~a" y x))))

(defgeneric pop (x))

(defmethod pop ((x <tower>))
  (let ((blocks (tower-blocks x)))
    (if blocks
        (progn
          ((setter tower-blocks) x (cdr blocks))
          (car blocks))
      (error <condition> "cannot pop block from emtpy tower ~a" x))))

;;;-----------------------------------------------------------------------------
;;; Move n blocks from tower x1 to tower x2 using x3 as buffer
;;;-----------------------------------------------------------------------------
(defgeneric move (n x1 x2 x3))

(defmethod move ((n <int>) (x1 <tower>) (x2 <tower>) (x3 <tower>))
  (if (= n 1)
      (progn
        (push x2 (pop x1))
        (sprint stderr x1 "    " x2 "    " x3 nl))
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
    (sprint stderr x1 "    " x2 "    " x3 nl)
    (move *max-tower-height* x1 x2 x3)))

(hanoi)

;;;-----------------------------------------------------------------------------
)  ;; End of module hanoi
;;;-----------------------------------------------------------------------------
