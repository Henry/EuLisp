;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1

;;;  Authors: Andreas Kind, Julian Padget
;;; Description: double precision floats
;;;-----------------------------------------------------------------------------
(defmodule double
  (syntax (_telos0)
   import (telos compare number integer fpi string float double1)
   export (<double-float> <double> double?
           most-positive-double-float
           least-positive-double-float
           most-negative-double-float
           least-negative-double-float
           double-binary< double-binary=
           double-binary-gcd double-binary-lcm)
   expose (double1))

;;;-----------------------------------------------------------------------------
;;; Limits (-2^29  ... 2^29 - 1)
;;;-----------------------------------------------------------------------------
  (defextern get-double-float-max () <double> "eul_get_dbl_max")
  (defextern get-double-float-min () <double> "eul_get_dbl_min")
  (defextern get-double-float-epsilon () <double> "eul_get_dbl_epsilon")
  (defextern get-negative-double-float-epsilon () <double>
             "eul_get_neg_dbl_epsilon")

  (defconstant most-positive-double-float (get-double-float-max))
  (defconstant least-positive-double-float (get-double-float-epsilon))
  (defconstant most-negative-double-float (get-double-float-min))
  (defconstant least-negative-double-float (get-negative-double-float-epsilon))

;;;-----------------------------------------------------------------------------
;;; Comparison
;;;-----------------------------------------------------------------------------
  (defextern double-binary= (<double> <double>) ptr
    "eul_dbl_equal")

  (defextern double-binary< (<double> <double>) ptr
    "eul_dbl_less")

  (defmethod binary= ((x <double>) (y <double>))
    (double-binary= x y))

  (defmethod binary= ((x <int>) (y <double>))
    (double-binary= (int-as-double x) y))

  (defmethod binary= ((x <double>) (y <int>))
    (double-binary= x (int-as-double y)))

  (defmethod binary< ((x <double>) (y <double>))
    (double-binary< x y))

  (defmethod binary< ((x <int>) (y <double>))
    (double-binary< (int-as-double x) y))

  (defmethod binary< ((x <double>) (y <int>))
    (double-binary< x (int-as-double y)))

;;;-----------------------------------------------------------------------------
;;; Rounding ...
;;;-----------------------------------------------------------------------------
  (defmethod ceiling ((x <double>)) (double-ceiling x))
  (defmethod floor ((x <double>)) (double-floor x))
  (defmethod round ((x <double>)) (double-round x))
  (defmethod truncate ((x <double>)) (double-truncate x))

;;;-----------------------------------------------------------------------------
;;; Gcd and lcm
;;;-----------------------------------------------------------------------------
  (defmethod binary-gcd ((x <double>) (y <double>))
    (double-binary-gcd x y))

  (defmethod binary-gcd ((x <int>) (y <double>))
    (double-binary-gcd (int-as-double x) y))

  (defmethod binary-gcd ((x <double>) (y <int>))
    (double-binary-gcd x (int-as-double y)))

  (defun double-binary-gcd (x y)
    (cond ((double-binary< x y)
           (let ((p (double-binary* x (double-binary/ y x))))
             (if (double-binary= p y) x
               (double-binary-gcd (double-binary- y p) x))))
          ((double-binary< y x)
           (let ((p (double-binary* y (double-binary/ x y))))
             (if (double-binary= p x) y
               (double-binary-gcd (double-binary- x p) y))))
          (t x)))

  (defmethod binary-lcm ((x <double>) (y <double>))
    (double-binary-lcm x y))

  (defmethod binary-lcm ((x <int>) (y <double>))
    (double-binary-lcm (int-as-double x) y))

  (defmethod binary-lcm ((x <double>) (y <int>))
    (double-binary-lcm x (int-as-double y)))

  (defun double-binary-lcm (x y)
    (if (or (= x 0) (= y 0))
        0
      (double-binary/ (double-binary* x y) (double-binary-gcd x y))))

;;;-----------------------------------------------------------------------------
;;; Predicates
;;;-----------------------------------------------------------------------------
  (defmethod zero? ((x <double>)) (double-binary= x 0.0))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
