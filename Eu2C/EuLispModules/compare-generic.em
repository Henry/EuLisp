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
;;; Title: generic functions of module compare
;;;  Notes:
;;    This module provides some generic functions with its default EuLisp
;;    names. They are defined in compare-generic-i with names which appear as
;;    more readable C-identifiers in the generated C-Code.
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule compare-generic
  (import (eulisp-kernel)
   syntax (eulisp-kernel)
   import ((rename ((binary-lt binary<)
                    (binary-eq binary=))
                   compare-generic-i))
   expose ((rename ((binary-lt binary<)
                    (binary-eq binary=))
                   compare-generic-i)))

;;;-----------------------------------------------------------------------------
;;; type schemes for type inference
;;;-----------------------------------------------------------------------------
(%annotate-function
  binary< new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <null>))
    ((var var1) (atom? <object>))
    ((var var2) (atom? <object>)))
   ((var0 var1 var2)
    ((var var0) (atom? <object>))
    ((var var1) (var var0))
    ((var var2) (var var0)))))

;; Have to be extended for df-zero and df-one, if exist.
(%annotate-function
  binary= new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <null>))
    ((var var1) (atom? fpi-zero))
    ((var var2) (atom? (and (not fpi-zero) <number>))))
   ((var0 var1 var2)
    ((var var0) (atom? <null>))
    ((var var1) (atom? (and (not fpi-zero) <number>)))
    ((var var2) (atom? fpi-zero)))
   ((var0 var1 var2)
    ((var var0) (atom? <null>))
    ((var var1) (atom? fpi-one))
    ((var var2) (atom? (and (not fpi-one) <number>))))
   ((var0 var1 var2)
    ((var var0) (atom? <null>))
    ((var var1) (atom? (and (not fpi-one) <number>)))
    ((var var2) (atom? fpi-one)))
   ((var0 var1 var2)
    ((var var0) (atom? <null>))
    ((var var1) (atom? (and (not singleton) <number>)))
    ((var var2) (atom? (and (not singleton) <number>))))
   ((var0 var1 var2)
    ((var var0) (atom? <number>))
    ((var var1) (var var0))
    ((var var2) (var var0)))))

;; May be extended with singleton number types.

(%annotate-function
  equal new-signature
  (((var0 var1 var2)
    ((var var0) (atom? (and <object> (not <null>))))
    ((var var1) (atom? <object>))
    ((var var2) (var var1)))
   ((var0 var1 var2)
    ((var var0) (atom? <null>))
    ((var var1) (atom? <object>))
    ((var var2) (atom? <object>)))))

;;;-----------------------------------------------------------------------------
)  ;; End of module compare-generic
;;;-----------------------------------------------------------------------------
