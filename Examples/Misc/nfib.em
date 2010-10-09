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
;;; Title: Count fibonacci function calls
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;  Compilation:
;;    youtoo nfib -l level-0 -l math
;;;  Interpretation:
;;    (!> nfib)
;;;-----------------------------------------------------------------------------

(defmodule nfib
  (syntax (syntax-0)
   import (level-0 math)
   export (nfib))

(defun nfib (n)
  (if (< 1 n)
      (+ (nfib (- n 1))
         (nfib (- n 2))
         1)
    1))

(time-execution (nfib 35) stdout)

;;;-----------------------------------------------------------------------------
)  ;; End of module nfib
;;;-----------------------------------------------------------------------------
