;;; Copyright 1994-2010 Fraunhofer ISST
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'Eu2C'
;;;-----------------------------------------------------------------------------
;;
;;  Eu2C is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: EL-in-CL: standard module vector
;;;  Description:
;;    make-vector, vector-ref and (setf (vector-ref ..) ..)
;;;  Authors: Horst Friedrich
;;;-----------------------------------------------------------------------------

#module vector
(import (eulisp-kernel
         (only (make-array
                svref
                svset
                vector
                equal
                +
                <
                elt
                length)
               common-lisp)
         (rename ((vectorp cl:vectorp))
                 (only (vectorp)
                       common-lisp))
         copy-generic)
 syntax (eulisp-kernel
         (only (defsetf)
               common-lisp))
 export (make-vector
         make-initialized-vector
         vector-ref
         vector?
         equal
         deep-copy
         shallow-copy))

(defun make-vector (size)
  (make-array size))

(defun vector? (v)
  (cl:vectorp v))

(defmacro make-initialized-vector objs ; *ak*
  `(vector ,@objs))

(defun vector-ref (vec idx)
  (svref vec idx))

(defsetf vector-ref (vec idx) (new-value)
  `(setf (svref ,vec ,idx) ,new-value))

(make-eulisp-class vector array)

(defun init-new-vector-deep (source-vec dest-vec len index)
  (if (< index len)
      (progn
        (setf (elt dest-vec index)
              (deep-copy (elt source-vec index)))
        (init-new-vector-deep source-vec dest-vec len (+ index 1)))
    dest-vec))

(defmethod deep-copy ((vec <vector>))
  (let ((len (length vec)))
    (init-new-vector-deep vec
                          (make-vector len)
                          len
                          0)))

(defun init-new-vector (source-vec dest-vec len index)
  (if (< index len)
      (progn
        (setf (elt dest-vec index) (elt source-vec index))
        (init-new-vector source-vec dest-vec len (+ index 1)))
    dest-vec))

(defmethod shallow-copy ((vec <vector>))
  (let ((len (length vec)))
    (init-new-vector vec
                     (make-vector len)
                     len
                     0)))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
