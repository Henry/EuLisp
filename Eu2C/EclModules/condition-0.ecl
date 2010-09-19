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
;;; Title: el-conditions in cl
;;;  Description:
;;    fake of the EL-Condition system
;;;  Problems:
;;    system rised conditions cannot have a continuation
;;    here one can only accept a condition
;;    Error and cerror not exported because of package conflicts in n-1 files
;;;  Authors: E. Ulrich Kriegel
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

#module condition-0
(import (eulisp-kernel
         (only (class-of)
               common-lisp) ;;to make ingo's emulator happy
         (rename ((signal cl:signal)
                  (error cl:error)
                  (make-condition cl:make-condition)
                  (make-instance cl:make-instance)
                  (mapcar cl:mapcar)
                  (append cl:append)
                  (gensym cl:gensym)
                  (find-class cl:find-class)
                  (condition cl:condition)
                  (block cl:block)
                  (return-from cl:return-from)
                  (subtypep cl:subtypep)
                  (format cl:format)
                  (division-by-zero cl:division-by-zero))
                 (only (apply
                        error
                        mapcar
                        gensym
                        signal
                        make-condition
                        make-instance
                        append
                        find-class condition
                        block
                        subtypep
                        return-from
                        format
                        subtypep
                        class-of)
                       common-lisp)))
 syntax (eulisp-kernel
         (rename ((defmacro cl:defmacro)
                  (handler-bind cl:handler-bind))
                 (only (handler-bind
                        defmacro)
                       common-lisp)))
 export (let/cc
          with-handled-conditions
          defcondition
          ;;error cerror no-handler
          condition-message))


;;;-----------------------------------------------------------------------------
;;; faked eulisp version
;;;-----------------------------------------------------------------------------
(defmacro defcondition (name supers slots)
  `(defconstant ,name (cl:defclass ,name ,supers ,slots )))

(defmacro with-handled-conditions
  (condition-variable-action-list . forms)
  (let* ((handler-bind-list
          (cl:mapcar (lambda(x)
                       (let* ((condition (cl:car x))
                              (lambda-list (if (cl:null (cl:car(cl:cdr x)))
                                               (cl:list (cl:gensym)(cl:gensym))
                                             (cl:car (cl:cdr x))))
                              (forms (cl:cdr (cl:cdr x)))
                              (cond-var (cl:car lambda-list))
                              (lambda1
                               (cl:append (cl:list 'lambda lambda-list) forms)))

                         (cl:list
                          condition
                          (cl:append
                           (cl:list 'lambda
                                    (cl:list cond-var))
                           (cl:list
                            (cl:list 'funcall lambda1
                                     cond-var
                                     (cl:list 'if (cl:list 'cl:subtypep (cl:list 'cl:class-of cond-var) (cl:list
                                                                                                         'cl:find-class ''<condition>))
                                              (cl:list
                                               'condition-continuation cond-var) '())))))

                         ))
                     condition-variable-action-list)))
    `(cl:handler-bind ,handler-bind-list ,@forms)))

(defmacro let/cc (name . forms)
  (let ((tmp (cl:gensym)))
    `(cl:block ,name
               (let ((,name (lambda(,tmp)(cl:return-from ,name ,tmp))))
                 ,@forms))))

;; local macro definitions


;;definitions and init-forms

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
