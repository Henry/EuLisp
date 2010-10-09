;;; Copyright 2003 T. Kurt Bond
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
;;; Title: Just some odds and ends, not intended to be compilable.
;;;  Authors: T. Kurt Bond
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule misc
  (syntax (syntax-0)
   import (level-0))

;; Given the arity of a lambda with rest args and the actual arguments
;; return a list with the fixed arguments and the rest arguments separated.
(defun fixed-and-rest (n l)
  (let ((n (- n 1)))                  ;last of arity is rest arg.
    (let loop ((i 0) (fixed ()) (l l))
         (cond
           ((>= i n)
            (list (reverse fixed) l))
           (t
            (loop (+ i 1) (cons (car l) fixed) (cdr l)))))))

;;;-----------------------------------------------------------------------------
)  ;; End of module misc
;;;-----------------------------------------------------------------------------
