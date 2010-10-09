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
;;; Title: Memory test
;;;  Maintainer: Henry G. Weller
;;;  Compilation:
;;    youtoo mem -l level-0
;;;  Interpretation:
;;    (!> mem)
;;;-----------------------------------------------------------------------------

(defmodule mem
  (syntax (syntax-0)
   import (level-0))

(defconstant *lifetime* 100)
(defconstant *blocksize* 1000)

(deflocal *vec* (make <vector> size: *lifetime*))

(defun foo (i j)
  (if (< i *lifetime*)
      (progn
        ((setter vector-ref) *vec* i (make <vector> size: *blocksize*))
        (foo (+ i 1) j))
    (if (< 0 j)
        (foo 0 (- j 1))
      ())))

(defun run ()
  (foo 0 1000))

(time-execution (run) stdout)

;;;-----------------------------------------------------------------------------
)  ;; End of module mem
;;;-----------------------------------------------------------------------------
