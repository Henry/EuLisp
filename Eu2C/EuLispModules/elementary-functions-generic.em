;;; Copyright 1994-2010 Fraunhofer ISST
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'Eu2C'
;;;-----------------------------------------------------------------------------
;;
;;  Eu2C is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;;  Title: the elementary functions module with defgenerics
;;;  Authors: E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------

(defmodule elementary-functions-generic
  (import (eulisp-kernel
           (only (<float>) float-i))
   syntax (eulisp-kernel)
   export
   (acos
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
    sqrt))

(defgeneric acos((f <float>)))
(defgeneric asin((f <float>)))
(defgeneric atan((f <float>)))
(defgeneric atan2 ((f1 <float>) (f2 <float>)))
(defgeneric cos((f <float>)))
(defgeneric sin((f <float>)))
(defgeneric tan((f <float>)))
(defgeneric cosh((f <float>)))
(defgeneric sinh((f <float>)))
(defgeneric tanh((f <float>)))
(defgeneric exp((f <float>)))
(defgeneric log((f <float>)))
(defgeneric log10((f <float>)))
(defgeneric pow ((f1 <float>) (f2 <float>)))
(defgeneric sqrt((f <float>)))

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------

(%annotate-function
  acos new-signature
  (((var0 var1)
    ((var var0) (atom? <float>))
    ((var var1) (atom? <float>)))))

(%annotate-function
  asin new-signature
  (((var0 var1)
    ((var var0) (atom? <float>))
    ((var var1) (atom? <float>)))))

(%annotate-function
  atan new-signature
  (((var0 var1)
    ((var var0) (atom? <float>))
    ((var var1) (atom? <float>)))))

(%annotate-function
  atan2 new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <float>))
    ((var var1) (atom? <float>))
    ((var var2) (atom? <float>)))))

(%annotate-function
  cos new-signature
  (((var0 var1)
    ((var var0) (atom? <float>))
    ((var var1) (atom? <float>)))))

(%annotate-function
  sin new-signature
  (((var0 var1)
    ((var var0) (atom? <float>))
    ((var var1) (atom? <float>)))))

(%annotate-function
  tan new-signature
  (((var0 var1)
    ((var var0) (atom? <float>))
    ((var var1) (atom? <float>)))))

(%annotate-function
  cosh new-signature
  (((var0 var1)
    ((var var0) (atom? <float>))
    ((var var1) (atom? <float>)))))

(%annotate-function
  sinh new-signature
  (((var0 var1)
    ((var var0) (atom? <float>))
    ((var var1) (atom? <float>)))))

(%annotate-function
  tanh new-signature
  (((var0 var1)
    ((var var0) (atom? <float>))
    ((var var1) (atom? <float>)))))

(%annotate-function
  exp new-signature
  (((var0 var1)
    ((var var0) (atom? <float>))
    ((var var1) (atom? <float>)))))

(%annotate-function
  log new-signature
  (((var0 var1)
    ((var var0) (atom? <float>))
    ((var var1) (atom? <float>)))))

(%annotate-function
  log10 new-signature
  (((var0 var1)
    ((var var0) (atom? <float>))
    ((var var1) (atom? <float>)))))

(%annotate-function
  pow new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <float>))
    ((var var1) (atom? <float>))
    ((var var2) (atom? <float>)))))

(%annotate-function
  sqrt new-signature
  (((var0 var1)
    ((var var0) (atom? <float>))
    ((var var1) (atom? <float>)))))


)
