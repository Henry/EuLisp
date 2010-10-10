;;; Copyright 1994 Russell Bradford
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
;;; Title: Test rename in defmodule
;;;  Authors: Russell Bradford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule rename
  (import ((rename ((defun procedure)
                    (setq :=)
                    (cons setq)
                    (let bind)
                    (+ plus)
                    (lambda church)
                    (car cdr)
                    (cdr car))
                   level-0)))

(procedure foo (x)
           (bind ((y 1))
                 (:= y (plus y 1))
                 (setq x y)))

(procedure bar (x)
           (church (y) (plus x y)))

;;;-----------------------------------------------------------------------------
)  ;; End of module rename
;;;-----------------------------------------------------------------------------
