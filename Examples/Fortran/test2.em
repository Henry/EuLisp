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
;;;  Library: fortran
;;;  Authors: Andreas Kind
;;; Description: Fortran interoperability
;;;  Compilation
;;   youtoo -c test2 -l level1
;;   f77 -c eul-f2.f
;;   youtoo test2 -l level1 -fff eul-f2 -recompile
;;;-----------------------------------------------------------------------------
(defmodule test2
  (syntax (macros)
   import (level1)
   export (bar))

;;;-----------------------------------------------------------------------------
;;; In-call from Forran
;;;-----------------------------------------------------------------------------
(defun bar (x y)
  (+ x y))

;;;-----------------------------------------------------------------------------
;;; Out-call to Fortran
;;;-----------------------------------------------------------------------------
(defextern foo (<int*> <double*>) <double>)
(print (foo (convert 42 <int*)1 (convert 1.0 <double*>)) nl)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
