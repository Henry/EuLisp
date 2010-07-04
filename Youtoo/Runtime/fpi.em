;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;; Description: fixed precision integers (int)
;;;-----------------------------------------------------------------------------
(defmodule fpi
  (syntax (_telos0)
   import (telos compare number integer)
   export (<int> int?
           int-binary- int-binary* int-binary/ int-binary% int-binary-mod
           int-as-string
           most-positive-int
           most-negative-int))

;;;-----------------------------------------------------------------------------
;;; Class <int>
;;;-----------------------------------------------------------------------------
  (defprimclass <int> fpi-class (<integer>) ())

;;;-----------------------------------------------------------------------------
;;; Limits (-2^29 - 1  ... 2^29 - 1)
;;;-----------------------------------------------------------------------------
  (defconstant most-positive-int 536870911)
  (defconstant most-negative-int -536870911)

;;;-----------------------------------------------------------------------------
;;; Arithmetic
;;;-----------------------------------------------------------------------------
  (defmethod binary+ ((x <int>) (y <int>)) (int-binary+ x y))
  (defmethod binary= ((x <int>) (y <int>)) (if (int-binary= x y) t ()))
  (defmethod binary< ((x <int>) (y <int>)) (int-binary< x y))
  (defmethod binary- ((x <int>) (y <int>)) (int-binary- x y))
  (defmethod binary* ((x <int>) (y <int>)) (int-binary* x y))
  (defmethod binary/ ((x <int>) (y <int>)) (int-binary/ x y))
  (defmethod binary% ((x <int>) (y <int>)) (int-binary% x y))
  (defmethod binary-mod ((x <int>) (y <int>)) (int-binary-mod x y))

;;;-----------------------------------------------------------------------------
;;; Gcd and lcm
;;;-----------------------------------------------------------------------------
  (defmethod binary-gcd ((x <int>) (y <int>)) (int-binary-gcd x y))

  (defun int-binary-gcd (x y)
    (cond ((int-binary< x y)
           (let ((p (int-binary* x (int-binary/ y x))))
             (if (int-binary= p y) x
               (int-binary-gcd (int-binary- y p) x))))
          ((int-binary< y x)
           (let ((p (int-binary* y (int-binary/ x y))))
             (if (int-binary= p x) y
               (int-binary-gcd (int-binary- x p) y))))
          (t x)))

  (defmethod binary-lcm ((x <int>) (y <int>)) (int-binary-lcm x y))

  (defun int-binary-lcm (x y)
    (if (or (= x 0) (= y 0))
        0
      (int-binary/ (int-binary* x y) (int-binary-gcd x y))))

;;;-----------------------------------------------------------------------------
;;; Predicates
;;;-----------------------------------------------------------------------------
  (defmethod zero? ((x <int>)) (int-binary= x 0))

;;;-----------------------------------------------------------------------------
;;; Conversion
;;;-----------------------------------------------------------------------------
  (defgeneric (converter <int>) (x))
  (defextern int-as-string (<int>) <string> "eul_int_as_str")

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
