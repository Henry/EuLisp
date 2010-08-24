;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;; Description: elementary functions
;;;-----------------------------------------------------------------------------
(defmodule math
  (syntax (_telos0)
   import (telos double)
   export (pi acos asin atan atan2 cos sin tan cosh sinh tanh
              exp log log10 pow sqrt
              double-acos double-asin double-atan double-atan2 double-cos
              double-sin double-tan double-cosh double-sinh double-tanh
              double-exp double-log double-log10 double-pow double-sqrt))

;;;-----------------------------------------------------------------------------
;;; Pi
;;;-----------------------------------------------------------------------------
(defconstant pi 3.141592654)

;;;-----------------------------------------------------------------------------
;;; With a little external help ...
;;;-----------------------------------------------------------------------------
(defextern double-acos (<double>) <double> "acos")
(defextern double-asin (<double>) <double> "asin")
(defextern double-atan (<double>) <double> "atan")
(defextern double-atan2 (<double> <double>) <double> "atan2")
(defextern double-cos (<double>) <double> "cos")
(defextern double-sin (<double>) <double> "sin")
(defextern double-tan (<double>) <double> "tan")
(defextern double-cosh (<double>) <double> "cosh")
(defextern double-sinh (<double>) <double> "sinh")
(defextern double-tanh (<double>) <double> "tanh")
(defextern double-exp (<double>) <double> "exp")
(defextern double-log (<double>) <double> "log")
(defextern double-log10 (<double>) <double> "log10")
(defextern double-pow (<double> <double>) <double> "pow")
(defextern double-sqrt (<double>) <double> "sqrt")

;;;-----------------------------------------------------------------------------
;;; Generic elementary functions
;;;-----------------------------------------------------------------------------
(defgeneric acos ((x <double>)))
(defgeneric asin ((x <double>)))
(defgeneric atan ((x <double>)))
(defgeneric atan2 ((x <double>) (x <double>)))
(defgeneric cos ((x <double>)))
(defgeneric sin ((x <double>)))
(defgeneric tan ((x <double>)))
(defgeneric cosh ((x <double>)))
(defgeneric sinh ((x <double>)))
(defgeneric tanh ((x <double>)))
(defgeneric exp ((x <double>)))
(defgeneric log ((x <double>)))
(defgeneric log10 ((x <double>)))
(defgeneric pow ((x <double>) (x <double>)))
(defgeneric sqrt ((x <double>)))

;;;-----------------------------------------------------------------------------
;;; ... and the corresponding double float methods
;;;-----------------------------------------------------------------------------
(defmethod acos ((x <double>)) (double-acos x))
(defmethod asin ((x <double>)) (double-asin x))
(defmethod atan ((x <double>)) (double-atan x))
(defmethod atan2 ((x <double>) (y <double>)) (double-atan2 x y))
(defmethod cos ((x <double>)) (double-cos x))
(defmethod sin ((x <double>)) (double-sin x))
(defmethod tan ((x <double>)) (double-tan x))
(defmethod cosh ((x <double>)) (double-cosh x))
(defmethod sinh ((x <double>)) (double-sinh x))
(defmethod tanh ((x <double>)) (double-tanh x))
(defmethod exp ((x <double>)) (double-exp x))
(defmethod log ((x <double>)) (double-log x))
(defmethod log10 ((x <double>)) (double-log10 x))
(defmethod pow ((x <double>) (y <double>)) (double-pow x y))
(defmethod sqrt ((x <double>)) (double-sqrt x))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
