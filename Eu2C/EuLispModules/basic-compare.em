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
;;; Title: Basic comparison function `eq'
;;;  Authors: Horst Friedrich
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule basic-compare
  (import (apply-level-1
           ti-sys-signatures)
   syntax (apply-level-1)
   export (eq
           t))

;;;-----------------------------------------------------------------------------
;;; Global constants
;;;-----------------------------------------------------------------------------
(defconstant t 't)

;;;-----------------------------------------------------------------------------
;;; eq
;;;-----------------------------------------------------------------------------
(defun eq (object1 object2)
  (if (%eq (%cast %signed-word-integer object1)
           (%cast %signed-word-integer object2))
      t
    ()))

;;;-----------------------------------------------------------------------------
;;; type schemes for used for type inference
;;;-----------------------------------------------------------------------------
(%annotate-function
  eq new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <object>))
    ((var var1) (atom? <object>))
    ((var var2) (atom? <object>)))))

;;;-----------------------------------------------------------------------------
)  ;; End of module basic-compare
;;;-----------------------------------------------------------------------------
