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
;;; Title: Generic functions for float
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule float-generic
  (import (eulisp-kernel
           (only (<float>)
                 float-i))
   syntax (eulisp-kernel)
   export (ceiling
           floor
           round
           truncate))

;;;-----------------------------------------------------------------------------
;;; Generic functions
;;;-----------------------------------------------------------------------------
(defgeneric ceiling((f1 <float>)))
(defgeneric floor ((f1 <float>)))
(defgeneric round ((f1 <float>)))
(defgeneric truncate ((f1 <float>)))

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------
(%annotate-function
  ceiling new-signature
  (((var0 var1)
    ((var var0) (atom? <float>))
    ((var var1) (atom? <float>)))))

(%annotate-function
  floor new-signature
  (((var0 var1)
    ((var var0) (atom? <float>))
    ((var var1) (atom? <float>)))))

(%annotate-function
  round new-signature
  (((var0 var1)
    ((var var0) (atom? <int>))
    ((var var1) (atom? <float>)))))

(%annotate-function
  truncate new-signature
  (((var0 var1)
    ((var var0) (atom? <int>))
    ((var var1) (atom? <float>)))))

;;;-----------------------------------------------------------------------------
)  ;; End of module float-generic
;;;-----------------------------------------------------------------------------
