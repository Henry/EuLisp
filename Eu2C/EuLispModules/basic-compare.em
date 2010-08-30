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
;;;  Title:
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Horst Friedrich
;;;-----------------------------------------------------------------------------
(defmodule basic-compare

  (import (apply-level-1
           ti-sys-signatures          ; this allows declaration of signatures
           )
   syntax (apply-level-1)
   export (eq t nil)
   )

(defconstant nil () )
(defconstant t 't)

(defun eq (object1 object2)
  (if (%eq (%cast %signed-word-integer object1)
           (%cast %signed-word-integer object2))
      t
    nil ))

;;;-----------------------------------------------------------------------------
;;; type schemes for used for type inference
;;;-----------------------------------------------------------------------------

;; see also basic-list-0.am
(%annotate-function
  eq new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <object>))
    ((var var1) (atom? <object>))
    ((var var2) (atom? <object>)))))

); end of module basic-compare
