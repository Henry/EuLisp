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
;;; Title: TAKeuchi function from the Gabriel tests
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    Author:       Richard Gabriel
;;    Created:      12-Apr-85
;;    Modified:     12-Apr-85 09:58:18 (Bob Shaw)
;;                  22-Jul-87 (Will Clinger)
;;                  27-Jul-95 (Andreas Kind)
;;;  Compilation:
;;    youtoo tak -l level-0 -l math
;;;  Interpretation:
;;    (!> tak)
;;;-----------------------------------------------------------------------------

(defmodule tak
  (syntax (syntax-0)
   import (level-0
           math))

(defun tak (x y z)
  (if (null? (< y x))
      z
    (tak (tak (- x 1) y z)
         (tak (- y 1) z x)
         (tak (- z 1) x y))))

(print (tak 26 16 6) nl)

;;;-----------------------------------------------------------------------------
)  ;; End of module tak
;;;-----------------------------------------------------------------------------
