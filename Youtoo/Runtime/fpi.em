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
)  ;; End of module
;;;-----------------------------------------------------------------------------
