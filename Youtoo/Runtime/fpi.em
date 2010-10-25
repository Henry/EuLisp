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
;;; Title: fixed precision integers (int)
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule fpi
  (syntax (_telos0)
   import (telos
           compare
           number
           integer)
   export (<fpi>
           fpi?
           fpi-binary-
           fpi-binary*
           fpi-binary/
           fpi-binary%
           fpi-binary-mod
           fpi-as-string
           most-positive-fpi
           most-negative-fpi))

;;;-----------------------------------------------------------------------------
;;; Class <fpi>
;;;-----------------------------------------------------------------------------
(defprimclass <fpi> fpi-class (<integer>) ())

;;;-----------------------------------------------------------------------------
;;; Limits (-2^29 - 1  ... 2^29 - 1)
;;;-----------------------------------------------------------------------------
;;***HGW doesn't work 32bit(defconstant most-positive-fpi 536870911)
;;***HGW doesn't work 32bit(defconstant most-negative-fpi -536870911)
(defconstant most-positive-fpi 53687091)
(defconstant most-negative-fpi -53687091)

;;;-----------------------------------------------------------------------------
;;; Arithmetic
;;;-----------------------------------------------------------------------------
(defmethod binary+ ((x <fpi>) (y <fpi>)) (fpi-binary+ x y))
(defmethod binary= ((x <fpi>) (y <fpi>)) (if (fpi-binary= x y) t ()))
(defmethod binary< ((x <fpi>) (y <fpi>)) (fpi-binary< x y))
(defmethod binary- ((x <fpi>) (y <fpi>)) (fpi-binary- x y))
(defmethod binary* ((x <fpi>) (y <fpi>)) (fpi-binary* x y))
(defmethod binary/ ((x <fpi>) (y <fpi>)) (fpi-binary/ x y))
(defmethod binary% ((x <fpi>) (y <fpi>)) (fpi-binary% x y))
(defmethod binary-mod ((x <fpi>) (y <fpi>)) (fpi-binary-mod x y))

;;;-----------------------------------------------------------------------------
;;; Gcd and lcm
;;;-----------------------------------------------------------------------------
(defmethod binary-gcd ((x <fpi>) (y <fpi>)) (fpi-binary-gcd x y))

(defun fpi-binary-gcd (x y)
  (cond ((fpi-binary< x y)
         (let ((p (fpi-binary* x (fpi-binary/ y x))))
           (if (fpi-binary= p y) x
             (fpi-binary-gcd (fpi-binary- y p) x))))
        ((fpi-binary< y x)
         (let ((p (fpi-binary* y (fpi-binary/ x y))))
           (if (fpi-binary= p x) y
             (fpi-binary-gcd (fpi-binary- x p) y))))
        (t x)))

(defmethod binary-lcm ((x <fpi>) (y <fpi>)) (fpi-binary-lcm x y))

(defun fpi-binary-lcm (x y)
  (if (or (= x 0) (= y 0))
      0
    (fpi-binary/ (fpi-binary* x y) (fpi-binary-gcd x y))))

;;;-----------------------------------------------------------------------------
;;; Predicates
;;;-----------------------------------------------------------------------------
(defmethod zero? ((x <fpi>)) (fpi-binary= x 0))

;;;-----------------------------------------------------------------------------
;;; Conversion
;;;-----------------------------------------------------------------------------
(defgeneric (converter <fpi>) (x))
(defextern fpi-as-string (<fpi>) <string> "eul_fpi_as_str")

;;;-----------------------------------------------------------------------------
)  ;; End of module fpi
;;;-----------------------------------------------------------------------------
