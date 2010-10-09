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
;;; Title: Arithmetic test
;;;  Maintainer: Henry G. Weller
;;;  Compilation:
;;    youtoo arith1 -l level-0 -l math
;;;  Interpretation:
;;    (!> arith1)
;;;-----------------------------------------------------------------------------

(defmodule arith1
  (syntax (syntax-0)
   import (level-0
           math))

(defconstant *max* 100001.1)

(defun test (x y)
  (if (= x *max*)
      x
    (test (- x (+ (* y 2) (/ x (abs y))))
          (- y (+ (* x 2) (/ y (abs x)))))))

(defun run ()
  (test 1.1 1.1))

(time-execution (run) stdout)

;;;-----------------------------------------------------------------------------
)  ;; End of module arith1
;;;-----------------------------------------------------------------------------
