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
;;;  Title: EL-in-CL: dynamic bindings for Level1
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module dynamic

(import (eulisp-kernel
         symbol
         list
         (only (caar cadar) common-lisp))

 syntax (eulisp-kernel
         ;;list-ext
         (only (declare special push)
               common-lisp)
         (rename ((defvar cl:defvar))
                 common-lisp)))

;;defined macros: defvar dynamic dynamic-let dynamic-setq

(defun make-dynamic-id (id)
  (make-eulisp-symbol id))

(defmacro defvar (id init)
  `(progn
     (cl:defvar ,(make-dynamic-id id))
     (setq ,(make-dynamic-id id) ,init)))

(defmacro dynamic (id) (make-dynamic-id id))

(cl:defvar *dynamics* nil)

(defun make-and-collect-dynamic-id (id)
  (let ((dyn-id (make-dynamic-id id)))
    (push dyn-id *dynamics*)
    dyn-id))

(defmacro dynamic-let (vars . body)
  (let ((*dynamics* nil))
    `(let ,(make-dynamic-let-vars vars)
       (declare (special ,@*dynamics*))
       ,@body)))

(defun make-dynamic-let-vars (vars)
  (cond ((null? vars) nil)
        ((symbolp (car vars))
         (cons (make-and-collect-dynamic-id (car vars))
               (make-dynamic-let-vars (cdr vars))))
        (t (cons `(,(make-and-collect-dynamic-id (caar vars)) ,(cadar vars))
                 (make-dynamic-let-vars (cdr vars))))))

(defmacro dynamic-setq (var form)
  `(setq ,(make-dynamic-id var) ,form))

#module-end
