;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'EuXLisp'
;;;-----------------------------------------------------------------------------
;;
;;  EuXLisp is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  EuXLisp is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: EuLisp Level-0 eval module
;;;-----------------------------------------------------------------------------
;;;  Description:
;;    eval in the exported environment of the optionally specified module which
;;    defaults to the current module.
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule eval
  (syntax (syntax)
   import (root)
   export (eval/cm
           eval
           expand-syntax1
           expand-syntax))

(defun eval (x . mod)
  (if mod
      (let ((curmod (find-module (current-module))))
        (set-module (find-module (car mod)))
        (let ((res (eval/cm x)))
          (set-module curmod)
          res))
    (eval/cm x)))

;;;-----------------------------------------------------------------------------
)  ;; End of module eval
;;;-----------------------------------------------------------------------------
