;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: bignum
;;;  Authors: Danius Michaelides, Andreas Kind
;;; Description: bignums
;;;-----------------------------------------------------------------------------
(defmodule bigint
  (syntax (macros)
   import (level1 mpz)
   export (<bigint> bigintp bigint-value make-bigint-fast))

;;;-----------------------------------------------------------------------------
;;; Class definition
;;;-----------------------------------------------------------------------------
  (defclass <bigint> (<integer>)
    ((value accessor: bigint-value keyword: value: requiredp: t))
    predicate: bigintp)

  (defmethod initialize ((x <bigint>) inits)
    (call-next-method)
    (let ((val (bigint-value x)))
      (cond ((null? (objectp val))) ;;This gives sometimes as SIGV!
            ((bigintp val)
             ((setter bigint-value) x (mpz-init-set (bigint-value val))))
            ((intp val)
             ((setter bigint-value) x (mpz-init-set-si val)))
            ((doublep val)
             ((setter bigint-value) x (mpz-init-set-d val)))
            ((stringp val)
             ((setter bigint-value) x (mpz-init-set-str val 10)))
            (t
             (error "cannot allocate bigint for ~a" val)))
      x))

  (defun make-bigint-fast (ptr)
    (let ((res (primitive-allocate <bigint> 1)))
      ((setter primitive-ref) res 0 ptr)
      res))

  (defmethod generic-write ((r <bigint>) (s <stream>) )
   (let ((str (mpz-get-str 10 (bigint-value r))))
     (prin-string str (string-size str) s)))

;;;-----------------------------------------------------------------------------
;;; Binary +
;;;-----------------------------------------------------------------------------
  (defmethod binary+ ((x <bigint>) (y <bigint>))
    (make-bigint-fast (mpz-add-init (bigint-value x) (bigint-value y))))

  (defmethod binary+ ((x <bigint>) (y <int>))
    (make-bigint-fast (mpz-add-ui-init (bigint-value x) y)))

  (defmethod binary+ ((x <int>) (y <bigint>))
    (make-bigint-fast (mpz-add-ui-init (bigint-value y) x)))

;;;-----------------------------------------------------------------------------
;;; Binary -
;;;-----------------------------------------------------------------------------
  (defmethod binary- ((x <bigint>) (y <bigint>))
    (make-bigint-fast (mpz-sub-init (bigint-value x) (bigint-value y))))

  (defmethod binary- ((x <bigint>) (y <int>))
    (make-bigint-fast (mpz-sub-ui-init (bigint-value x) y)))

  (defmethod binary- ((x <int>) (y <bigint>))
    (make-bigint-fast (mpz-sub-init (mpz-init-set-si x) (bigint-value y))))

;;;-----------------------------------------------------------------------------
;;; Bianry *
;;;-----------------------------------------------------------------------------
  (defmethod binary* ((x <bigint>) (y <bigint>))
    (make-bigint-fast (mpz-mul-init (bigint-value x) (bigint-value y))))

  (defmethod binary* ((x <bigint>) (y <int>))
    (make-bigint-fast (mpz-mul-ui-init (bigint-value x) y)))

  (defmethod binary* ((x <int>) (y <bigint>))
    (make-bigint-fast (mpz-mul-ui-init (bigint-value y) x)))

;;;-----------------------------------------------------------------------------
;;; Comparison
;;;-----------------------------------------------------------------------------
  (defmethod binary= ((x <bigint>) (y <bigint>))
    (= (mpz-cmp (bigint-value x) (bigint-value y)) 0))

  (defmethod binary= ((x <bigint>) (y <int>))
    (= (mpz-cmp-si (bigint-value x) y) 0))

  (defmethod binary= ((x <int>) (y <bigint>))
    (= (mpz-cmp-si (bigint-value y) x) 0))

  (defmethod binary< ((x <bigint>) (y <bigint>))
    (< (mpz-cmp (bigint-value x) (bigint-value y)) 0))

  (defmethod binary< ((x <bigint>) (y <int>))
    (< (mpz-cmp-si (bigint-value x) y) 0))

  (defmethod binary< ((x <int>) (y <bigint>))
    (< (mpz-cmp (mpz-init-set-si x) (bigint-value y)) 0))

;;;-----------------------------------------------------------------------------
;;; Gcd etc.
;;;-----------------------------------------------------------------------------
  (defmethod binary-gcd ((x <bigint>) (y <bigint>))
    (make-bigint-fast (mpz-gcd-init (bigint-value x) (bigint-value y))))

  (defmethod binary-gcd ((x <bigint>) (y <int>))
     (let ((result (mpz-init)))
       (mpz-gcd-ui result (bigint-value x) y)
       (make-bigint-fast result)))

  (defmethod binary-gcd ((x <int>) (y <bigint>))
    (make-bigint-fast (mpz-gcd-init (mpz-init-set-si x) (bigint-value y))))

  (defmethod binary-mod ((x <bigint>) (y <bigint>))
    (make-bigint-fast (mpz-mod-init (bigint-value x) (bigint-value y))))

  (defmethod binary-mod ((x <bigint>) (y <int>))
    (let ((result (mpz-init)))
      (mpz-mod-ui result (bigint-value x) y)
      (make-bigint-fast result)))

  (defmethod binary-mod ((x <int>) (y <bigint>))
    (make-bigint-fast (mpz-mod-init (mpz-init-set-si x) (bigint-value y))))

  (defmethod binary% ((x <bigint>) (y <bigint>))
    (make-bigint-fast (mpz-tdiv-r-init (bigint-value x) (bigint-value y))))

  (defmethod binary% ((x <bigint>) (y <int>))
    (make-bigint-fast (mpz-tdiv-r-ui-init (bigint-value x) y)))

  (defmethod binary% ((x <int>) (y <bigint>))
    (let ((result (mpz-init)))
      (mpz-tdiv-r-ui result (bigint-value x) y)
      (make-bigint-fast result)))

;;;-----------------------------------------------------------------------------
;;; Division overflows to doubles
;;;-----------------------------------------------------------------------------
  (defmethod binary/ ((x <bigint>) (y <bigint>))
    (binary/ (convert x <double>) (convert y <double>)))

  (defmethod binary/ ((x <bigint>) (y <int>))
    (binary/ (convert x <double>) (convert y <double>)))

  (defmethod binary/ ((x <int>) (y <bigint>))
    (binary/ (convert x <double>) (convert y <double>)))

  (defmethod binary/ ((x <bigint>) (y <double>))
    (binary/ (convert x <double>) y))

  (defmethod binary/ ((x <double>) (y <bigint>))
    (binary/ x (convert y <double>)))

;;;-----------------------------------------------------------------------------
;;; Misc
;;;-----------------------------------------------------------------------------
  ;(defun evenp (x)
  ;  (mpz-cmp-si (mpz-mod x 2) 0))

  ;(defun oddp (x) (not (evenp x)))

  (defmethod zerop ((x <bigint>))
    (= (mpz-cmp-si (bigint-value x) 0) 0))

  (defmethod negate ((x <bigint>))
    (make-bigint-fast (mpz-neg-init (bigint-value x))))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
