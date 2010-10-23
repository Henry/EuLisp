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
;;; Title: double precision floats
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule double
  (syntax (_telos0)
   import (telos
           compare
           number
           integer
           fpi
           string
           float
           double1)
   export (<double-float>
           <double>
           double?
           most-positive-double-float
           least-positive-double-float
           most-negative-double-float
           least-negative-double-float
           double-binary<
           double-binary=
           double-binary-gcd
           double-binary-lcm)
   expose (double1))

;;;-----------------------------------------------------------------------------
;;; Limits
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

(defmethod binary= ((x <fpi>) (y <double>))
  (double-binary= (int-as-double x) y))

(defmethod binary= ((x <double>) (y <fpi>))
  (double-binary= x (int-as-double y)))

(defmethod binary< ((x <double>) (y <double>))
  (double-binary< x y))

(defmethod binary< ((x <fpi>) (y <double>))
  (double-binary< (int-as-double x) y))

(defmethod binary< ((x <double>) (y <fpi>))
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

(defmethod binary-gcd ((x <fpi>) (y <double>))
  (double-binary-gcd (int-as-double x) y))

(defmethod binary-gcd ((x <double>) (y <fpi>))
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

(defmethod binary-lcm ((x <fpi>) (y <double>))
  (double-binary-lcm (int-as-double x) y))

(defmethod binary-lcm ((x <double>) (y <fpi>))
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
)  ;; End of module double
;;;-----------------------------------------------------------------------------
