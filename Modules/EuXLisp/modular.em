;;; Copyright 1994 Russell Bradford
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'EuXLisp'
;;;-----------------------------------------------------------------------------
;;
;;  EuXLisp is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  EuXLisp is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: Modular arithmetic
;;;  Authors: Russell Bradford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule modular
  (syntax (syntax-0)
   import (level-0)
   export (Mod
           <modular-error>
           <modular-number>
           <no-modular-inverse>))

(defclass <modular-number> ()
  ((value keyword: value:
          accessor: modular-number-value)
   (modulus keyword: modulus:
            accessor: modular-number-modulus))
  constructor: (make-modular-number value: modulus:)
  predicate: modular-number?)

(defmethod generic-write ((a <modular-number>) stream)
  (generic-print "#<" stream)
  (generic-print (modular-number-value a) stream)
  (generic-print " mod " stream)
  (generic-print (modular-number-modulus a) stream)
  (generic-print ">" stream)
  a)

(defun Mod (a n)
  (make-modular-number
   (if (< a 0) (+ n a) a)
   n))

(defcondition <modular-error> () ())

(defcondition <modular-argument-mismatch> <modular-error>
  ((arg1 default: 0)
   (arg2 default: 0)))

(defun modular-mismatch (moda modb)
  (error <modular-argument-mismatch>
         "mismatch moduli in modular +"
         arg1: moda
         arg2: modb))

(defmethod binary+ ((a <modular-number>) (b <modular-number>))
  (let ((moda (modular-number-modulus a))
        (modb (modular-number-modulus b)))
    (if (= moda modb)
        (Mod
         (% (+ (modular-number-value a)
               (modular-number-value b))
            moda)
         moda)
      (modular-mismatch moda modb))))

(defmethod binary- ((a <modular-number>) (b <modular-number>))
  (let ((moda (modular-number-modulus a))
        (modb (modular-number-modulus b)))
    (if (= moda modb)
        (Mod
         (% (- (modular-number-value a)
               (modular-number-value b))
            moda)
         moda)
      (modular-mismatch moda modb))))

;; (defmethod unary- ((a <modular-number>))
;;   (let ((moda (modular-number-modulus a)))
;;     (Mod (- moda (modular-number-value a)) moda)))

(defmethod binary* ((a <modular-number>) (b <modular-number>))
  (let ((moda (modular-number-modulus a))
        (modb (modular-number-modulus b)))
    (if (= moda modb)
        (Mod
         (% (* (modular-number-value a)
               (modular-number-value b))
            moda)
         moda)
      (modular-mismatch moda modb))))

(defmethod binary/ ((a <modular-number>) (b <modular-number>))
  (let ((moda (modular-number-modulus a))
        (modb (modular-number-modulus b)))
    (if (= moda modb)
        (binary* a (unary/ b))
      (modular-mismatch moda modb))))

;; (defmethod unary/ ((a <modular-number>))
;;   (let ((moda (modular-number-modulus a)))
;;     (Mod (inverse-mod-n (modular-number-value a) moda) moda)))

(defcondition <no-modular-inverse> <modular-error>
  ((value default: 0)
   (modulus default: 0)))

(defun inverse-mod-n (a n)
  (if (> (gcd a n) 1)
      (error <no-modular-inverse>
             "no modular inverse"
             value: a
             modulus: n)
    (let ((ans (gcd-cofactors a n 1 0 0 1)))
      (car ans))))

(defun gcd-cofactors (a b cfa1 cfa2 cfb1 cfb2)
  (if (= b 0)
      (cons cfa1 cfa2)
    (let ((q (quotient a b)))
      (gcd-cofactors
       b
       (- a (* q b))
       cfb1
       cfb2
       (- cfa1 (* q cfb1))
       (- cfa2 (* q cfb2))))))

;;;-----------------------------------------------------------------------------
)  ;; End of module modular
;;;-----------------------------------------------------------------------------
