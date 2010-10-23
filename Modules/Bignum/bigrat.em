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
;;; Title: Variable-precision rational numbers
;;;  Library: bignum
;;;  Authors: Danius Michaelides, Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule bigrat
  (syntax (syntax-0)
   import (level-0
           bigint
           mpz
           mpq)
   export (<bigrat>
           bigrat?
           make-bigrat-fast
           numerator
           denumerator))

;;;-----------------------------------------------------------------------------
;;; Class definition
;;;-----------------------------------------------------------------------------
(defclass <bigrat> <number>
  ((bigrat-value accessor: bigrat-value keyword: value: required?: t))
  predicate: bigrat?)

(defmethod initialize ((x <bigrat>) inits)
  (call-next-method)
  (let ((val (bigrat-value x)))
    (if (cons? val)
        (let ((a (car val))
              (b (cdr val)))
          (cond ((and (fpi? a) (fpi? b))
                 ((setter bigrat-value) x (mpq-init-set-si a b)))
                ((and (bigint? a) (bigint? b))
                 (let ((blah (mpq-init)))
                   (mpq-set-num blah (bigint-value a))
                   (mpq-set-den blah (bigint-value b))
                   ((setter bigrat-value) x blah)))
                ((and (fpi? a) (bigint? b))
                 (let ((blah (mpq-init)))
                   (mpq-set-num blah (mpz-init-set-si a))
                   (mpq-set-den blah (bigint-value b))
                   ((setter bigrat-value) x blah)))
                ((and (bigint? a) (fpi? b))
                 (let ((blah (mpq-init)))
                   (mpq-set-num blah (bigint-value a))
                   (mpq-set-den blah (mpz-init-set-si b))
                   ((setter bigrat-value) x blah)))
                (t
                 (error <condition> (fmt "cannot allocate bigrat for ~a" val)))))
      (error <condition> (fmt "cannot allocate bigrat for ~a" val)))
    x))

(defun make-bigrat-fast (ptr)
  (let ((res (primitive-allocate <bigrat> 1)))
    ((setter primitive-ref) res 0 ptr)
    res))

(defmethod generic-write ((r <bigrat>) (s <stream>))
  (let* ((ptr (bigrat-value r))
         (num (mpq-get-num ptr))
         (den (mpq-get-den ptr)))
    (sformat s "~a/~a" (mpz-get-str 10 num) (mpz-get-str 10 den))))

(defun denumerator (x)
  (mpq-get-den (bigrat-value x)))

(defun numerator (x)
  (mpq-get-num (bigrat-value x)))

;;;-----------------------------------------------------------------------------
;;; Binary+
;;;-----------------------------------------------------------------------------
(defmethod binary+ ((x <bigrat>) (y <bigrat>))
  (make-bigrat-fast (mpq-add-init (bigrat-value x) (bigrat-value y))))

(defmethod binary+ ((x <bigrat>) (y <fpi>))
  (let ((blah (mpq-init-set-si y 1)))
    (make-bigrat-fast (mpq-add-init (bigrat-value x) blah))))

(defmethod binary+ ((x <fpi>) (y <bigrat>))
  (let ((blah (mpq-init-set-si y 1)))
    (make-bigrat-fast (mpq-add-init blah (bigrat-value y) ))))

(defmethod binary+ ((x <bigrat>) (y <bigint>))
  (let ((blah (mpq-init-set-z (bigint-value 1))))
    (make-bigrat-fast (mpq-add-init (bigrat-value x) blah))))

(defmethod binary+ ((x <bigint>) (y <bigrat>))
  (let ((blah (mpq-init-set-z (bigint-value x))))
    (make-bigrat-fast (mpq-add-init blah (bigrat-value y) ))))

;;;-----------------------------------------------------------------------------
;;; Binary*
;;;-----------------------------------------------------------------------------
(defmethod binary* ((x <bigrat>) (y <bigrat>))
  (make-bigrat-fast (mpq-mul-init (bigrat-value x) (bigrat-value y))))

(defmethod binary* ((x <bigrat>) (y <fpi>))
  (let ((blah (mpq-init-set-si y 1)))
    (make-bigrat-fast (mpq-mul-init (bigrat-value x) blah))))

(defmethod binary* ((x <fpi>) (y <bigrat>))
  (let ((blah (mpq-init-set-si x 1)))
    (make-bigrat-fast (mpq-mul-init blah (bigrat-value y) ))))

(defmethod binary* ((x <bigrat>) (y <bigint>))
  (let ((blah (mpq-init-set-z (bigint-value y))))
    (make-bigrat-fast (mpq-mul-init (bigrat-value x) blah))))

(defmethod binary* ((x <bigint>) (y <bigrat>))
  (let ((blah (mpq-init-set-z (bigint-value x))))
    (make-bigrat-fast (mpq-mul-init blah (bigrat-value y) ))))

;;;-----------------------------------------------------------------------------
;;; Binary-
;;;-----------------------------------------------------------------------------
(defmethod binary- ((x <bigrat>) (y <bigrat>))
  (make-bigrat-fast (mpq-sub-init (bigrat-value x) (bigrat-value y))))

(defmethod binary- ((x <bigrat>) (y <fpi>))
  (let ((blah (mpq-init-set-si y 1)))
    (make-bigrat-fast (mpq-sub-init (bigrat-value x) blah))))

(defmethod binary- ((x <fpi>) (y <bigrat>))
  (let ((blah (mpq-init-set-si x 1)))
    (make-bigrat-fast (mpq-sub-init blah (bigrat-value y) ))))

(defmethod binary- ((x <bigrat>) (y <bigint>))
  (let ((blah (mpq-init-set-z (bigint-value y))))
    (make-bigrat-fast (mpq-sub-init (bigrat-value x) blah))))

(defmethod binary- ((x <bigint>) (y <bigrat>))
  (let ((blah (mpq-init-set-z (bigint-value x))))
    (make-bigrat-fast (mpq-sub-init blah (bigrat-value y) ))))

;;;-----------------------------------------------------------------------------
;;; Binary/
;;;-----------------------------------------------------------------------------
(defmethod binary/ ((x <bigrat>) (y <bigrat>))
  (make-bigrat-fast (mpq-div-init (bigrat-value x) (bigrat-value y))))

(defmethod binary/ ((x <bigrat>) (y <fpi>))
  (let ((blah (mpq-init-set-si y 1)))
    (make-bigrat-fast (mpq-div-init (bigrat-value x) blah))))

(defmethod binary/ ((x <fpi>) (y <bigrat>))
  (let ((blah (mpq-init-set-si x 1)))
    (make-bigrat-fast (mpq-div-init blah (bigrat-value y) ))))

(defmethod binary/ ((x <bigrat>) (y <bigint>))
  (let ((blah (mpq-init-set-z (bigint-value y))))
    (make-bigrat-fast (mpq-div-init (bigrat-value x) blah))))

(defmethod binary/ ((x <bigint>) (y <bigrat>))
  (let ((blah (mpq-init-set-z (bigint-value x))))
    (make-bigrat-fast (mpq-div-init blah (bigrat-value y) ))))

(defmethod binary/ ((x <bigint>) (y <bigint>))
  (make <bigrat> value: `(,x ,y)))

(defmethod binary/ ((x <bigint>) (y <fpi>))
  (make <bigrat> value: (list x (make <bigint> value: y))))

(defmethod binary/ ((x <fpi>) (y <bigint>))
  (make <bigrat> value: (list (make <bigint> value: x) y)))

;;;-----------------------------------------------------------------------------
;;; Comparison
;;;-----------------------------------------------------------------------------
(defmethod binary= ((x <bigrat>) (y <bigrat>))
  (= 0 (mpq-cmp (bigrat-value x) (bigrat-value y))))

(defmethod binary= ((x <bigrat>) (y <fpi>))
  (let ((blah (mpq-init-set-si y 1)))
    (= 0 (mpq-cmp (bigrat-value x) blah))))

(defmethod binary= ((x <fpi>) (y <bigrat>))
  (let ((blah (mpq-init-set-si x 1)))
    (= 0 (mpq-cmp (bigrat-value y) blah))))

(defmethod binary< ((x <bigrat>) (y <bigrat>))
  (= 0 (mpq-cmp (bigrat-value x) (bigrat-value y))))

(defmethod binary< ((x <bigrat>) (y <fpi>))
  (let ((blah (mpq-init-set-si y 1)))
    (< 0 (mpq-cmp (bigrat-value x) blah))))

(defmethod binary< ((x <fpi>) (y <bigrat>))
  (let ((blah (mpq-init-set-si x 1)))
    (< 0 (mpq-cmp (bigrat-value y) blah))))

;;;-----------------------------------------------------------------------------
;;; Misc
;;;-----------------------------------------------------------------------------
;(defun even? (x)
; (mpz-cmp-si (mpz-mod x 2) 0))

;(defun odd? (x) (not (even? x)))

;;;-----------------------------------------------------------------------------
)  ;; End of module bigrat
;;;-----------------------------------------------------------------------------
