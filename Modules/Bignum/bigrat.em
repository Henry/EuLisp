;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: bignum
;;;  Authors: Danius Michaelides, Andreas Kind
;;; Description: bignums
;;;-----------------------------------------------------------------------------
(defmodule bigrat
  (syntax (macros)
   import (level1 bigint mpz mpq)
   export (<bigrat> bigrat? make-bigrat-fast numerator denumerator))

;;;-----------------------------------------------------------------------------
;;; Class definition
;;;-----------------------------------------------------------------------------
  (defclass <bigrat> (<number>)
    ((bigrat-value accessor: bigrat-value keyword: value: required?: t))
    predicate: bigrat?)

  (defmethod initialize ((x <bigrat>) inits)
    (call-next-method)
    (let ((val (bigrat-value x)))
      (if (cons? val)
          (let ((a (car val))
                (b (cdr val)))
            (cond ((and (int? a) (int? b))
                   ((setter bigrat-value) x (mpq-init-set-si a b)))
                  ((and (bigint? a) (bigint? b))
                   (let ((blah (mpq-init)))
                     (mpq-set-num blah (bigint-value a))
                     (mpq-set-den blah (bigint-value b))
                     ((setter bigrat-value) x blah)))
                  ((and (int? a) (bigint? b))
                   (let ((blah (mpq-init)))
                     (mpq-set-num blah (mpz-init-set-si a))
                     (mpq-set-den blah (bigint-value b))
                     ((setter bigrat-value) x blah)))
                  ((and (bigint? a) (int? b))
                   (let ((blah (mpq-init)))
                     (mpq-set-num blah (bigint-value a))
                     (mpq-set-den blah (mpz-init-set-si b))
                     ((setter bigrat-value) x blah)))
                  (t
                   (error "cannot allocate bigrat for ~a" val))))
        (error "cannot allocate bigrat for ~a" val))
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

  (defmethod binary+ ((x <bigrat>) (y <int>))
    (let ((blah (mpq-init-set-si y 1)))
      (make-bigrat-fast (mpq-add-init (bigrat-value x) blah))))

  (defmethod binary+ ((x <int>) (y <bigrat>))
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

  (defmethod binary* ((x <bigrat>) (y <int>))
    (let ((blah (mpq-init-set-si y 1)))
      (make-bigrat-fast (mpq-mul-init (bigrat-value x) blah))))

  (defmethod binary* ((x <int>) (y <bigrat>))
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

  (defmethod binary- ((x <bigrat>) (y <int>))
    (let ((blah (mpq-init-set-si y 1)))
      (make-bigrat-fast (mpq-sub-init (bigrat-value x) blah))))

  (defmethod binary- ((x <int>) (y <bigrat>))
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

  (defmethod binary/ ((x <bigrat>) (y <int>))
    (let ((blah (mpq-init-set-si y 1)))
      (make-bigrat-fast (mpq-div-init (bigrat-value x) blah))))

  (defmethod binary/ ((x <int>) (y <bigrat>))
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

  (defmethod binary/ ((x <bigint>) (y <int>))
    (make <bigrat> value: (list x (make <bigint> value: y))))

  (defmethod binary/ ((x <int>) (y <bigint>))
    (make <bigrat> value: (list (make <bigint> value: x) y)))

;;;-----------------------------------------------------------------------------
;;; Comparison
;;;-----------------------------------------------------------------------------
  (defmethod binary= ((x <bigrat>) (y <bigrat>))
    (= 0 (mpq-cmp (bigrat-value x) (bigrat-value y))))

  (defmethod binary= ((x <bigrat>) (y <int>))
    (let ((blah (mpq-init-set-si y 1)))
      (= 0 (mpq-cmp (bigrat-value x) blah))))

  (defmethod binary= ((x <int>) (y <bigrat>))
    (let ((blah (mpq-init-set-si x 1)))
      (= 0 (mpq-cmp (bigrat-value y) blah))))

  (defmethod binary< ((x <bigrat>) (y <bigrat>))
    (= 0 (mpq-cmp (bigrat-value x) (bigrat-value y))))

  (defmethod binary< ((x <bigrat>) (y <int>))
    (let ((blah (mpq-init-set-si y 1)))
      (< 0 (mpq-cmp (bigrat-value x) blah))))

  (defmethod binary< ((x <int>) (y <bigrat>))
    (let ((blah (mpq-init-set-si x 1)))
      (< 0 (mpq-cmp (bigrat-value y) blah))))

;;;-----------------------------------------------------------------------------
;;; Misc
;;;-----------------------------------------------------------------------------
  ;(defun even? (x)
  ; (mpz-cmp-si (mpz-mod x 2) 0))

  ;(defun odd? (x) (not (even? x)))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
