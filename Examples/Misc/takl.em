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
;;; Title: The TAKeuchi function using lists as counters
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    Author:       Richard Gabriel
;;    Created:      12-Apr-85
;;    Modified:     12-Apr-85 10:07:00 (Bob Shaw)
;;                  22-Jul-87 (Will Clinger)
;;                  27-Jul-95 (Andreas Kind)
;;;  Compilation:
;;    youtoo takl -l level-0 -l math
;;;  Interpretation:
;;    (!> takl)
;;;-----------------------------------------------------------------------------

(defmodule takl
  (syntax (syntax-0)
   import (level-0
           math))

(defun listn (n)
  (if (null? (= 0 n))
      (cons n (listn (- n 1)))
    ()))

(deflocal |26l| (listn 26))
(deflocal |16l| (listn 16))
(deflocal |6l| (listn 6))

(defun mas (x y z)
  (if (null? (shorter? y x))
      z
    (mas (mas (cdr x) y z)
         (mas (cdr y) z x)
         (mas (cdr z) x y))))

(defun shorter? (x y)
  (and y (or (null? x)
             (shorter? (cdr x)
                       (cdr y)))))

(time-execution (mas |26l| |16l| |6l|) stdout)

;;;-----------------------------------------------------------------------------
)  ;; End of module takl
;;;-----------------------------------------------------------------------------
