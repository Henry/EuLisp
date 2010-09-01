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
;;;  Title: EL-in-CL: The kernel of EuLisp
;;;  Description:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module eulisp-kernel
(import ((only (first
                cons
                list
                rest
                t)
               common-lisp)
         (rename ((null cl:null)
                  (symbolp cl:symbolp)
                  (defun cl:defun)
                  (defmacro cl:defmacro))
                 common-lisp)
         el-modules)
 syntax (el-modules
         (only (&rest
                cond
                progn
                setq
                defvar)
               common-lisp)
         (rename ((lambda cl:lambda)
                  (compile cl:compile))
                 common-lisp))
 expose (el-modules
         (only
          (t
           quote
           setq
           setf
           if
           cond
           and
           or
           progn
           let
           let*
           labels
           flet
           unwind-protect
           next-method?
           call-next-method
           funcall
           apply)
          common-lisp)))

;;;-----------------------------------------------------------------------------
;;; <object>: the root of the inheritance tree
;;;-----------------------------------------------------------------------------

(make-eulisp-class object t)

;;;-----------------------------------------------------------------------------
;;; Defining Forms
;;;-----------------------------------------------------------------------------

(export-syntax defmacro
               deflocalmacro
               defun
               lambda
               deflocal
               defconstant
               defgeneric
               defmethod)

(cl:defun make-cl-lambda-list (el-lambda-list)
          (cond ((cl:null el-lambda-list) ())
                ((cl:symbolp el-lambda-list) (list '&rest el-lambda-list))
                (t (cons (first el-lambda-list)
                         (make-cl-lambda-list (rest el-lambda-list))))))

(cl:defmacro defmacro (name lambda-list . body)
             `(progn (export-syntax ,name)
                     (cl:defmacro ,name ,(make-cl-lambda-list lambda-list)
                                  ,@body)))

(cl:defmacro deflocalmacro (name lambda-list . body)
             `(cl:defmacro ,name ,(make-cl-lambda-list lambda-list) ,@body))

(cl:defmacro defun (name lambda-list . body)
             `(cl:defun ,name ,(make-cl-lambda-list lambda-list) ,@body))

(cl:defmacro lambda (lambda-list . body)
             `#'(cl:lambda ,(make-cl-lambda-list lambda-list),@body))

(cl:defmacro deflocal (name value)
             `(defvar ,name ,value))

(cl:defmacro defconstant (name value)
             `(defvar ,name ,value))

;;***HGW in CL generic functions dot not support specialisers
;;***HGW (cl:defmacro defgeneric (name lambda-list)
;;***HGW              `(cl:defgeneric ,name ,(make-cl-lambda-list lambda-list)))

;;***HGW this is a hacked version to filter-out the specialisers
(cl:defmacro defgeneric (name lambda-list)
             `(cl:defgeneric ,name ,(make-cl-generic-fun-lambda-list lambda-list)))
(cl:defun make-cl-generic-fun-lambda-list (el-lambda-list)
          (cl:mapcar (cl:lambda (arg) (cl:if (cl:listp arg) (cl:car arg) arg))
                     (make-cl-lambda-list el-lambda-list)))
;;***HGW Maybe the proper equivalent is to macro expand to something like
;; (defgeneric name (arg1 arg2)
;;   (:method ((arg1 t) (arg2 t))
;;            (error "Generic function is restricted to (~{~a~^, ~})"
;;                   specialisers))
;;   (:method ((arg1 specialiser1) (arg2 specialiser2))
;;            (error "Generic function is unimplemented, but supported for
;; (~{~a~^, ~})." specialisers)))

(cl:defmacro defmethod (name lambda-list . body)
             `(cl:defmethod ,name ,(make-cl-lambda-list lambda-list) ,@body))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
