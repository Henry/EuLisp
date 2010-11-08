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
;;; Title: Bignums test
;;;  Library: bignum
;;;  Authors: Danius Michaelides, Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;  Compilation:
;;    make test
;;;-----------------------------------------------------------------------------

(defmodule test1
  (syntax (syntax-0)
   import (level-0
           bignum))

;;;-----------------------------------------------------------------------------
;;; Bigints
;;;-----------------------------------------------------------------------------
(deflocal one (make <bigint> value: 1))
(format " 1: 1 == ~a\n" one)
(deflocal two (make <bigint> value: 2))
(format " 2: 2 == ~a\n" two)
(deflocal ten (make <bigint> value: 10))
(format " 3: 10 == ~a\n" ten)
(deflocal x (make <bigint> value: "1002031231231442347298742734123456789"))
(format " 4: ... == ~a\n" x)
(deflocal y (make <bigint> value: x))
(format " 5: ... == ~a\n" y)
(format " 6: 11 == ~a\n" (+ one 10))
(format " 7: 20 == ~a\n" (+ ten (make <bigint> value: "10")))
(format " 8: 20 == ~a\n" (+ ten (make <bigint> value: 10)))
(format " 9: ... == ~a\n" (- x ten))
(format "10: 9 == ~a\n" (- ten 1))
(format "11: 1 == ~a\n" (- 11 ten))
(format "12: -10 == ~a\n" (* ten (- ten 11)))
(format "13: -1 == ~a\n" (< ten 100))
(format "14: () == ~a\n" (< 100 ten))
(format "15: 0 == ~a\n" (= ten 10))
(format "16: 0 == ~a\n" (= 10 ten))
(format "17: () == ~a\n" (= 11 ten))
(defun fact (n)
  (if (= n 1)
      one
    (* (fact (- n 1)) n)))
(format "18: ... == ~a\n" (fact 50))
(format "19: ... == ~a\n" (fact 100))

(defun fact2 (x)
  (letfuns
   ((loop (xx res)
          (if (< xx two)
              res
            (loop (- xx one) (* res xx)))))
   (loop x 1)))
(format "20: ... == ~a\n" (fact2 50))
(format "21: -9 == ~a\n" (+ one (make <bigint> value: "-10")))
(format "22: -9 == ~a\n" (+ one (make <bigint> value: -10)))
(format "23: -9 == ~a\n" (+ one -10))
(format "24: 11 == ~a\n" (+ one (- (make <bigint> value: -10))))
(format "25: -9 == ~a\n" (+ one (- 0 (make <bigint> value: "10"))))
(format "26: -10 == ~a\n" (* one (- 0 10)))
(format "27: -9 == ~a\n" (+ one (- 0 10)))
(format "28: 11 == ~a\n" (- one (- 0 10)))
(format "29: () == ~a\n" (even? one))
(format "30: t == ~a\n" (odd? one))
(format "31: 1 == ~a\n" (% one 3))
;;;-----------------------------------------------------------------------------
;;; Bigrats
;;;-----------------------------------------------------------------------------
(deflocal z1 (make <bigrat> value: '(1 . 2)))
(deflocal z2 (make <bigrat> value: (cons x y)))
(format "32: 1/2 ==~a\n" z1)
(format "33: 1/4 == ~a\n" (* z1 z1))
(format "34: 5/1 == ~a\n" (* z1 10))
(format "35: -1/2 == ~a\n" (- z1 1))
(format "36: 1/1 == ~a\n" (+ z1 z1))
(format "39: ... == ~a\n" (make <bigrat> value: (cons x y)))
(format "40: ... == ~a\n" (make <bigrat> value: (cons 29 x)))
(format "41: ... == ~a\n" (make <bigrat> value: (cons x 29)))
(format "43: 1/10 == ~a\n" (make <bigrat> value: (cons one ten)))
(format "44: ... == ~a\n" (/ z2 y))
(format "45: 10/1 == ~a\n" (/ 10 z2))

;;;-----------------------------------------------------------------------------
;;; Testing fixed precision under/overflow
;;;-----------------------------------------------------------------------------
(defun fact3 (n)
  (if (= n 1)
      1
    (* (fact3 (- n 1)) n)))
(format "46: ... == ~a\n" (fact3 100))
(defun foo (n)
  (if (< n (make <bigint> value: "-11111111111111111111111111111111111111111111111111111111111111111111111"))
      n
    (foo (* n 2))))
(format "47: ... == ~a\n" (foo -2))

;;;-----------------------------------------------------------------------------
)  ;; End of module test1
;;;-----------------------------------------------------------------------------
