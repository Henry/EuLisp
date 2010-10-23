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
;;; Title: Variable-precision integers
;;;  Library: bignum
;;;  Authors: Danius Michaelides, Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule bigint
  (syntax (syntax-0)
   import (level-0
           mpz)
   export (<bigint>
           bigint?
           bigint-value
           make-bigint-fast))

;;;-----------------------------------------------------------------------------
;;; Class definition
;;;-----------------------------------------------------------------------------
(defclass <bigint> <integer>
  ((value accessor: bigint-value keyword: value: required?: t))
  predicate: bigint?)

(defmethod initialize ((x <bigint>) inits)
  (call-next-method)
  (let ((val (bigint-value x)))
    (cond ((null? (object? val))) ;;This gives sometimes as SIGV!
          ((bigint? val)
           ((setter bigint-value) x (mpz-init-set (bigint-value val))))
          ((fpi? val)
           ((setter bigint-value) x (mpz-init-set-si val)))
          ((double? val)
           ((setter bigint-value) x (mpz-init-set-d val)))
          ((string? val)
           ((setter bigint-value) x (mpz-init-set-str val 10)))
          (t
           (error <condition> (fmt "cannot allocate bigint for ~a" val))))
    x))

(defun make-bigint-fast (ptr)
  (let ((res (primitive-allocate <bigint> 1)))
    ((setter primitive-ref) res 0 ptr)
    res))

(defmethod generic-write ((r <bigint>) (s <stream>) )
  (let ((str (mpz-get-str 10 (bigint-value r))))
    (print-string str (string-size str) s)))

;;;-----------------------------------------------------------------------------
;;; Binary +
;;;-----------------------------------------------------------------------------
(defmethod binary+ ((x <bigint>) (y <bigint>))
  (make-bigint-fast (mpz-add-init (bigint-value x) (bigint-value y))))

(defmethod binary+ ((x <bigint>) (y <fpi>))
  (make-bigint-fast (mpz-add-ui-init (bigint-value x) y)))

(defmethod binary+ ((x <fpi>) (y <bigint>))
  (make-bigint-fast (mpz-add-ui-init (bigint-value y) x)))

;;;-----------------------------------------------------------------------------
;;; Binary -
;;;-----------------------------------------------------------------------------
(defmethod binary- ((x <bigint>) (y <bigint>))
  (make-bigint-fast (mpz-sub-init (bigint-value x) (bigint-value y))))

(defmethod binary- ((x <bigint>) (y <fpi>))
  (make-bigint-fast (mpz-sub-ui-init (bigint-value x) y)))

(defmethod binary- ((x <fpi>) (y <bigint>))
  (make-bigint-fast (mpz-sub-init (mpz-init-set-si x) (bigint-value y))))

;;;-----------------------------------------------------------------------------
;;; Bianry *
;;;-----------------------------------------------------------------------------
(defmethod binary* ((x <bigint>) (y <bigint>))
  (make-bigint-fast (mpz-mul-init (bigint-value x) (bigint-value y))))

(defmethod binary* ((x <bigint>) (y <fpi>))
  (make-bigint-fast (mpz-mul-ui-init (bigint-value x) y)))

(defmethod binary* ((x <fpi>) (y <bigint>))
  (make-bigint-fast (mpz-mul-ui-init (bigint-value y) x)))

;;;-----------------------------------------------------------------------------
;;; Comparison
;;;-----------------------------------------------------------------------------
(defmethod binary= ((x <bigint>) (y <bigint>))
  (= (mpz-cmp (bigint-value x) (bigint-value y)) 0))

(defmethod binary= ((x <bigint>) (y <fpi>))
  (= (mpz-cmp-si (bigint-value x) y) 0))

(defmethod binary= ((x <fpi>) (y <bigint>))
  (= (mpz-cmp-si (bigint-value y) x) 0))

(defmethod binary< ((x <bigint>) (y <bigint>))
  (< (mpz-cmp (bigint-value x) (bigint-value y)) 0))

(defmethod binary< ((x <bigint>) (y <fpi>))
  (< (mpz-cmp-si (bigint-value x) y) 0))

(defmethod binary< ((x <fpi>) (y <bigint>))
  (< (mpz-cmp (mpz-init-set-si x) (bigint-value y)) 0))

;;;-----------------------------------------------------------------------------
;;; Gcd etc.
;;;-----------------------------------------------------------------------------
(defmethod binary-gcd ((x <bigint>) (y <bigint>))
  (make-bigint-fast (mpz-gcd-init (bigint-value x) (bigint-value y))))

(defmethod binary-gcd ((x <bigint>) (y <fpi>))
  (let ((result (mpz-init)))
    (mpz-gcd-ui result (bigint-value x) y)
    (make-bigint-fast result)))

(defmethod binary-gcd ((x <fpi>) (y <bigint>))
  (make-bigint-fast (mpz-gcd-init (mpz-init-set-si x) (bigint-value y))))

(defmethod binary-mod ((x <bigint>) (y <bigint>))
  (make-bigint-fast (mpz-mod-init (bigint-value x) (bigint-value y))))

(defmethod binary-mod ((x <bigint>) (y <fpi>))
  (let ((result (mpz-init)))
    (mpz-mod-ui result (bigint-value x) y)
    (make-bigint-fast result)))

(defmethod binary-mod ((x <fpi>) (y <bigint>))
  (make-bigint-fast (mpz-mod-init (mpz-init-set-si x) (bigint-value y))))

(defmethod binary% ((x <bigint>) (y <bigint>))
  (make-bigint-fast (mpz-tdiv-r-init (bigint-value x) (bigint-value y))))

(defmethod binary% ((x <bigint>) (y <fpi>))
  (make-bigint-fast (mpz-tdiv-r-ui-init (bigint-value x) y)))

(defmethod binary% ((x <fpi>) (y <bigint>))
  (let ((result (mpz-init)))
    (mpz-tdiv-r-ui result (bigint-value x) y)
    (make-bigint-fast result)))

;;;-----------------------------------------------------------------------------
;;; Division overflows to doubles
;;;-----------------------------------------------------------------------------
(defmethod binary/ ((x <bigint>) (y <bigint>))
  (binary/ (convert x <double>) (convert y <double>)))

(defmethod binary/ ((x <bigint>) (y <fpi>))
  (binary/ (convert x <double>) (convert y <double>)))

(defmethod binary/ ((x <fpi>) (y <bigint>))
  (binary/ (convert x <double>) (convert y <double>)))

(defmethod binary/ ((x <bigint>) (y <double>))
  (binary/ (convert x <double>) y))

(defmethod binary/ ((x <double>) (y <bigint>))
  (binary/ x (convert y <double>)))

;;;-----------------------------------------------------------------------------
;;; Misc
;;;-----------------------------------------------------------------------------
;(defun even? (x)
;  (mpz-cmp-si (mpz-mod x 2) 0))

;(defun odd? (x) (not (even? x)))

(defmethod zero? ((x <bigint>))
  (= (mpz-cmp-si (bigint-value x) 0) 0))

(defmethod negate ((x <bigint>))
  (make-bigint-fast (mpz-neg-init (bigint-value x))))

;;;-----------------------------------------------------------------------------
)  ;; End of module bigint
;;;-----------------------------------------------------------------------------
