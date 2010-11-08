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
;;; Title: Vector test
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;  Compilation:
;;    youtoo vec -l level-0
;;;  Interpretation:
;;    (!> vec)
;;;-----------------------------------------------------------------------------

(defmodule vec
  (syntax (syntax-0)
   import (level-0))

(deflocal *size* 1000000)
(deflocal *vec1* ())
(deflocal *vec2* ())

(defun allocate-vectors ()
  (setq *vec1* (make <vector> size: *size*))
  (setq *vec2* (make <vector> size: *size*))
  (fill-vector *vec1*))

(defun fill-vector (vec)
  (let ((n (size vec)))
    (letfuns
     ((loop (i)
            (if (< n i)
                ()
              (progn
                ((setter vector-ref) vec (- i 1) i)
                (loop (+ i 1))))))
     (loop 1))))

(defun copy (i)
  (if (= i *size*)
      ()
    (let ((j (vector-ref *vec1* i)))
      ((setter vector-ref) *vec2* i j)
      (copy j))))

(defun run ()
  (copy 0)
  (copy 0)
  (copy 0)
  (copy 0)
  (copy 0)
  (copy 0)
  (copy 0)
  (copy 0)
  (copy 0)
  (copy 0))

(allocate-vectors)
(time-execution (run) stdout)

;;;-----------------------------------------------------------------------------
)  ;; End of module vec
;;;-----------------------------------------------------------------------------
