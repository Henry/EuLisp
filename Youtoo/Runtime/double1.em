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
;;;  Library: level-1
;;;  Authors: Andreas Kind, Julian Padget
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule double1
  (syntax (_telos0)
   import (telos
           number
           integer
           fpi
           float
           string)
   export (double-binary+
           double-binary-
           double-binary*
           double-binary/
           double-binary%
           double-binary-mod
           double-ceiling
           double-round
           double-floor
           double-truncate
           double-as-string
           fpi-as-double))

;;;-----------------------------------------------------------------------------
;;; Arithmetic
;;;-----------------------------------------------------------------------------
(defextern double-binary+ (<double-float> <double-float>) <double-float>
           "eul_dbl_sum")
(defextern double-binary- (<double-float> <double-float>) <double-float>
           "eul_dbl_difference")
(defextern double-binary* (<double-float> <double-float>) <double-float>
           "eul_dbl_product")
(defextern double-binary/ (<double-float> <double-float>) <double-float>
           "eul_dbl_quotient")
(defextern double-binary% (<double-float> <double-float>) <double-float>
           "eul_dbl_remainder")
(defextern double-binary-mod (<double-float> <double-float>) <fpi>
           "eul_dbl_mod")

(defmethod binary+ ((x <double-float>) (y <double-float>))
  (double-binary+ x y))
(defmethod binary+ ((x <fpi>) (y <double-float>))
  (double-binary+ (fpi-as-double x) y))
(defmethod binary+ ((x <double-float>) (y <fpi>))
  (double-binary+ x (fpi-as-double y)))
(defmethod binary- ((x <double-float>) (y <double-float>))
  (double-binary- x y))
(defmethod binary- ((x <fpi>) (y <double-float>))
  (double-binary- (fpi-as-double x) y))
(defmethod binary- ((x <double-float>) (y <fpi>))
  (double-binary- x (fpi-as-double y)))
(defmethod binary* ((x <double-float>) (y <double-float>))
  (double-binary* x y))
(defmethod binary* ((x <fpi>) (y <double-float>))
  (double-binary* (fpi-as-double x) y))
(defmethod binary* ((x <double-float>) (y <fpi>))
  (double-binary* x (fpi-as-double y)))
(defmethod binary/ ((x <double-float>) (y <double-float>))
  (double-binary/ x y))
(defmethod binary/ ((x <fpi>) (y <double-float>))
  (double-binary/ (fpi-as-double x) y))
(defmethod binary/ ((x <double-float>) (y <fpi>))
  (double-binary/ x (fpi-as-double y)))
(defmethod binary% ((x <double-float>) (y <double-float>))
  (double-binary% x y))
(defmethod binary% ((x <fpi>) (y <double-float>))
  (double-binary% (fpi-as-double x) y))
(defmethod binary% ((x <double-float>) (y <fpi>))
  (double-binary% x (fpi-as-double y)))
(defmethod binary-mod ((x <double-float>) (y <double-float>))
  (double-truncate (double-binary% x y)))
(defmethod binary-mod ((x <fpi>) (y <double-float>))
  (double-binary-mod (fpi-as-double x) y))
(defmethod binary-mod ((x <double-float>) (y <fpi>))
  (double-binary-mod x (fpi-as-double y)))

;;;-----------------------------------------------------------------------------
;;; Conversion
;;;-----------------------------------------------------------------------------
(defextern double-ceiling (<double-float>) <double-float> "eul_dbl_ceiling")
(defextern double-floor (<double-float>) <double-float> "eul_dbl_floor")
(defextern double-round (<double-float>) <fpi> "eul_dbl_round")
(defextern double-truncate (<double-float>) <fpi> "eul_dbl_truncate")
(defextern double-as-string (<double-float>) <string> "eul_dbl_as_str")
(defextern fpi-as-double (<fpi>) <double-float> "eul_fpi_as_dbl")
;;(defgeneric (converter <double-float>) (x))
(defmethod (converter <double-float>) ((x <double-float>)) x)
(defmethod (converter <double-float>) ((x <fpi>)) (fpi-as-double x))
(defmethod (converter <string>) ((x <double-float>)) (double-as-string x))
(defmethod (converter <fpi>) ((x <double-float>)) (double-round x))

;;;-----------------------------------------------------------------------------
)  ;; End of module double1
;;;-----------------------------------------------------------------------------
