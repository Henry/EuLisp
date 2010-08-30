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
;;;  Title: Some useful Common-Lisp Functions
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------
(defmodule cl-ext
  (import (eulisp-level-0)
   syntax (eulisp-level-0)
   export (caar rplaca rplacd remove-if)
   )

(defun caar (l)
  (car (car l)))

(defun rplaca (x y)
  ((setter car) x y)
  x)

(defun rplacd (x y)
  ((setter cdr) x y)
  x)

(defmacro prog1 forms
  `(let ((_x_ ,(car forms)))
     ,@(cdr forms)
     _x_))

(defun remove-if (predicate list)
  (cond ((null? list) ())
        ((predicate (car list))
         (remove-if predicate (cdr list)))
        (t
         (cons (car list)
               (remove-if predicate (cdr list))))))

(defmacro setf (access new-value)
  `((,setter ,(car access)) ,(car (cdr access))
    ,@(cdr (cdr access))
    ,new-value))

(defmacro dotimes (var-spec . body)
  `(let ((_end_ ,(car (cdr var-spec))))
     (labels ((loop (,(car var-spec))
                    (when (,< ,(car var-spec) _end_)
                          ,@body
                          (loop (,+ ,(car var-spec) 1)))))
             (loop 0))))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
