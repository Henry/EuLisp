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
;;; Title: elementary functions
;;;  Library: level-1
;;;  Authors: Andreas Kind, Julian Padget
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule math
  (syntax (_telos0)
   import (telos
           double)
   export (pi
           acos
           asin
           atan
           atan2
           cos
           sin
           tan
           cosh
           sinh
           tanh
           exp
           log
           log10
           pow
           sqrt
           double-acos
           double-asin
           double-atan
           double-atan2
           double-cos
           double-sin
           double-tan
           double-cosh
           double-sinh
           double-tanh
           double-exp
           double-log
           double-log10
           double-pow double-sqrt))

;;;-----------------------------------------------------------------------------
;;; Pi
;;;-----------------------------------------------------------------------------
(defconstant pi 3.141592654)

;;;-----------------------------------------------------------------------------
;;; With a little external help ...
;;;-----------------------------------------------------------------------------
(defextern double-acos (<double-float>) <double-float> "acos")
(defextern double-asin (<double-float>) <double-float> "asin")
(defextern double-atan (<double-float>) <double-float> "atan")
(defextern double-atan2 (<double-float> <double-float>) <double-float> "atan2")
(defextern double-cos (<double-float>) <double-float> "cos")
(defextern double-sin (<double-float>) <double-float> "sin")
(defextern double-tan (<double-float>) <double-float> "tan")
(defextern double-cosh (<double-float>) <double-float> "cosh")
(defextern double-sinh (<double-float>) <double-float> "sinh")
(defextern double-tanh (<double-float>) <double-float> "tanh")
(defextern double-exp (<double-float>) <double-float> "exp")
(defextern double-log (<double-float>) <double-float> "log")
(defextern double-log10 (<double-float>) <double-float> "log10")
(defextern double-pow (<double-float> <double-float>) <double-float> "pow")
(defextern double-sqrt (<double-float>) <double-float> "sqrt")

;;;-----------------------------------------------------------------------------
;;; Generic elementary functions
;;;-----------------------------------------------------------------------------
(defgeneric acos ((x <double-float>)))
(defgeneric asin ((x <double-float>)))
(defgeneric atan ((x <double-float>)))
(defgeneric atan2 ((x <double-float>) (x <double-float>)))
(defgeneric cos ((x <double-float>)))
(defgeneric sin ((x <double-float>)))
(defgeneric tan ((x <double-float>)))
(defgeneric cosh ((x <double-float>)))
(defgeneric sinh ((x <double-float>)))
(defgeneric tanh ((x <double-float>)))
(defgeneric exp ((x <double-float>)))
(defgeneric log ((x <double-float>)))
(defgeneric log10 ((x <double-float>)))
(defgeneric pow ((x <double-float>) (x <double-float>)))
(defgeneric sqrt ((x <double-float>)))

;;;-----------------------------------------------------------------------------
;;; ... and the corresponding double float methods
;;;-----------------------------------------------------------------------------
(defmethod acos ((x <double-float>)) (double-acos x))
(defmethod asin ((x <double-float>)) (double-asin x))
(defmethod atan ((x <double-float>)) (double-atan x))
(defmethod atan2 ((x <double-float>) (y <double-float>)) (double-atan2 x y))
(defmethod cos ((x <double-float>)) (double-cos x))
(defmethod sin ((x <double-float>)) (double-sin x))
(defmethod tan ((x <double-float>)) (double-tan x))
(defmethod cosh ((x <double-float>)) (double-cosh x))
(defmethod sinh ((x <double-float>)) (double-sinh x))
(defmethod tanh ((x <double-float>)) (double-tanh x))
(defmethod exp ((x <double-float>)) (double-exp x))
(defmethod log ((x <double-float>)) (double-log x))
(defmethod log10 ((x <double-float>)) (double-log10 x))
(defmethod pow ((x <double-float>) (y <double-float>)) (double-pow x y))
(defmethod sqrt ((x <double-float>)) (double-sqrt x))

;;;-----------------------------------------------------------------------------
)  ;; End of module math
;;;-----------------------------------------------------------------------------
