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
;;;  Library: Misc
;;;  Authors: Andreas Kind
;;; Description: compute the sum up to 10000
;;;  Compilation
;;    youtoo sumTo -l level-0
;;;-----------------------------------------------------------------------------

(defmodule sumTo
  (syntax (syntax-0)
   import (level-0))

(defun sum (x res)
  (if (= x 0)
      res
    (sum (- x 1) (+ x res))))

(print (sum 10000 0) nl)

;;;-----------------------------------------------------------------------------
)  ;; End of module sumTo
;;;-----------------------------------------------------------------------------
