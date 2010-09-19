;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;; Description: double precision floats
;;;-----------------------------------------------------------------------------
(defmodule double1
  (syntax (_telos0)
   import (telos number integer fpi float string)
   export (double-binary+ double-binary-
                          double-binary* double-binary/ double-binary% double-binary-mod
                          double-ceiling double-round double-floor double-truncate
                          double-as-string int-as-double))

;;;-----------------------------------------------------------------------------
;;; Arithmetic
;;;-----------------------------------------------------------------------------
(defextern double-binary+ (<double> <double>) <double>
           "eul_dbl_sum")
(defextern double-binary- (<double> <double>) <double>
           "eul_dbl_difference")
(defextern double-binary* (<double> <double>) <double>
           "eul_dbl_product")
(defextern double-binary/ (<double> <double>) <double>
           "eul_dbl_quotient")
(defextern double-binary% (<double> <double>) <double>
           "eul_dbl_remainder")
(defextern double-binary-mod (<double> <double>) <int>
           "eul_dbl_mod")

(defmethod binary+ ((x <double>) (y <double>))
  (double-binary+ x y))
(defmethod binary+ ((x <int>) (y <double>))
  (double-binary+ (int-as-double x) y))
(defmethod binary+ ((x <double>) (y <int>))
  (double-binary+ x (int-as-double y)))
(defmethod binary- ((x <double>) (y <double>))
  (double-binary- x y))
(defmethod binary- ((x <int>) (y <double>))
  (double-binary- (int-as-double x) y))
(defmethod binary- ((x <double>) (y <int>))
  (double-binary- x (int-as-double y)))
(defmethod binary* ((x <double>) (y <double>))
  (double-binary* x y))
(defmethod binary* ((x <int>) (y <double>))
  (double-binary* (int-as-double x) y))
(defmethod binary* ((x <double>) (y <int>))
  (double-binary* x (int-as-double y)))
(defmethod binary/ ((x <double>) (y <double>))
  (double-binary/ x y))
(defmethod binary/ ((x <int>) (y <double>))
  (double-binary/ (int-as-double x) y))
(defmethod binary/ ((x <double>) (y <int>))
  (double-binary/ x (int-as-double y)))
(defmethod binary% ((x <double>) (y <double>))
  (double-binary% x y))
(defmethod binary% ((x <int>) (y <double>))
  (double-binary% (int-as-double x) y))
(defmethod binary% ((x <double>) (y <int>))
  (double-binary% x (int-as-double y)))
(defmethod binary-mod ((x <double>) (y <double>))
  (double-truncate (double-binary% x y)))
(defmethod binary-mod ((x <int>) (y <double>))
  (double-binary-mod (int-as-double x) y))
(defmethod binary-mod ((x <double>) (y <int>))
  (double-binary-mod x (int-as-double y)))

;;;-----------------------------------------------------------------------------
;;; Conversion
;;;-----------------------------------------------------------------------------
(defextern double-ceiling (<double>) <double> "eul_dbl_ceiling")
(defextern double-floor (<double>) <double> "eul_dbl_floor")
(defextern double-round (<double>) <int> "eul_dbl_round")
(defextern double-truncate (<double>) <int> "eul_dbl_truncate")
(defextern double-as-string (<double>) <string> "eul_dbl_as_str")
(defextern int-as-double (<int>) <double> "eul_fpi_as_dbl")
;;(defgeneric (converter <double>) (x))
(defmethod (converter <double>) ((x <double>)) x)
(defmethod (converter <double>) ((x <int>)) (int-as-double x))
(defmethod (converter <string>) ((x <double>)) (double-as-string x))
(defmethod (converter <int>) ((x <double>)) (double-round x))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
