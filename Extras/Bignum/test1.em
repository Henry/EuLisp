;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: bignum
;;;  Authors: Danius Michaelides, Andreas Kind
;;;  Description: bignums test
;;;  Compilation: make test
;;; -----------------------------------------------------------------------
(defmodule test1
  (syntax (macros)
   import (level1 bignum))
;;; --------------------------------------------------------------------
;;; Bigints
;;; --------------------------------------------------------------------
  (deflocal one (make <bigint> value: 1))
  (format t " 1: 1 == ~a\n" one)
  (deflocal two (make <bigint> value: 2))
  (format t " 2: 2 == ~a\n" two)
  (deflocal ten (make <bigint> value: 10))
  (format t " 3: 10 == ~a\n" ten)
  (deflocal x (make <bigint> value: "1002031231231442347298742734123456789"))
  (format t " 4: ... == ~a\n" x)
  (deflocal y (make <bigint> value: x))
  (format t " 5: ... == ~a\n" y)
  (format t " 6: 11 == ~a\n" (+ one 10))
  (format t " 7: 20 == ~a\n" (+ ten (make <bigint> value: "10")))
  (format t " 8: 20 == ~a\n" (+ ten (make <bigint> value: 10)))
  (format t " 9: ... == ~a\n" (- x ten))
  (format t "10: 9 == ~a\n" (- ten 1))
  (format t "11: 1 == ~a\n" (- 11 ten))
  (format t "12: -10 == ~a\n" (* ten (- ten 11)))
  (format t "13: -1 == ~a\n" (< ten 100))
  (format t "14: () == ~a\n" (< 100 ten))
  (format t "15: 0 == ~a\n" (= ten 10))
  (format t "16: 0 == ~a\n" (= 10 ten))
  (format t "17: () == ~a\n" (= 11 ten))
  (defun fact (n)
    (if (= n 1)
        one
      (* (fact (- n 1)) n)))
  (format t "18: ... == ~a\n" (fact 50))
  (format t "19: ... == ~a\n" (fact 100))
  (defun fact2 (x)
    (labels
     ((loop (xx res)
            (if (< xx two)
                res
              (loop (- xx one) (* res xx)))))
     (loop x 1)))
  (format t "20: ... == ~a\n" (fact2 50))
  (format t "21: -9 == ~a\n" (+ one (make <bigint> value: "-10")))
  (format t "22: -9 == ~a\n" (+ one (make <bigint> value: -10)))
  (format t "23: -9 == ~a\n" (+ one -10))
  (format t "24: 11 == ~a\n" (+ one (- (make <bigint> value: -10))))
  (format t "25: -9 == ~a\n" (+ one (- 0 (make <bigint> value: "10"))))
  (format t "26: -10 == ~a\n" (* one (- 0 10)))
  (format t "27: -9 == ~a\n" (+ one (- 0 10)))
  (format t "28: 11 == ~a\n" (- one (- 0 10)))
  (format t "29: () == ~a\n" (evenp one))
  (format t "30: t == ~a\n" (oddp one))
  (format t "31: 1 == ~a\n" (% one 3))
;;; --------------------------------------------------------------------
;;; Bigrats
;;; --------------------------------------------------------------------
  (deflocal z1 (make <bigrat> value: '(1 . 2)))
  (deflocal z2 (make <bigrat> value: (cons x y)))
  (format t "32: 1/2 ==~a\n" z1)
  (format t "33: 1/4 == ~a\n" (* z1 z1))
  (format t "34: 5/1 == ~a\n" (* z1 10))
  (format t "35: -1/2 == ~a\n" (- z1 1))
  (format t "36: 1/1 == ~a\n" (+ z1 z1))
  (format t "39: ... == ~a\n" (make <bigrat> value: (cons x y)))
  (format t "40: ... == ~a\n" (make <bigrat> value: (cons 29 x)))
  (format t "41: ... == ~a\n" (make <bigrat> value: (cons x 29)))
  (format t "43: 1/10 == ~a\n" (make <bigrat> value: (cons one ten)))
  (format t "44: ... == ~a\n" (/ z2 y))
  (format t "45: 10/1 == ~a\n" (/ 10 z2))
;;; --------------------------------------------------------------------
;;; Testing fixed precision under/overflow
;;; --------------------------------------------------------------------
  (defun fact3 (n)
    (if (= n 1)
        1
      (* (fact3 (- n 1)) n)))
  (format t "46: ... == ~a\n" (fact3 100))
  (defun foo (n)
    (if (< n (make <bigint> value: "-11111111111111111111111111111111111111111111111111111111111111111111111"))
        n
      (foo (* n 2))))
  (format t "47: ... == ~a\n" (foo -2))
)  ; end of module
