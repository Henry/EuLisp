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
;;;  Title: Syntax definitions and other basic definitions for eval
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module eval-basic

(import (eulisp1
         lzs
         el2lzs-error
         (only (find-module) el2lzs-main)
         (only (mapcar mapc assoc rest first append char-code)
               common-lisp))

 syntax (eulisp1
         (only (push) common-lisp))

 export (*global-variables*
         initialize-variables))

;;;-----------------------------------------------------------------------------
;;; syntax for declaring system functions
;;;-----------------------------------------------------------------------------

(defmacro declare-system-functions functions
  `(list ,@(mapcar (lambda (fun)
                     `(cons ',(make-eulisp-symbol fun)
                            #',fun))
                   functions)))

;;;-----------------------------------------------------------------------------
;;; basics for the variable environment
;;;-----------------------------------------------------------------------------

(defvar variable-environment () )
;;; holds the current bindings of local lexical variables in an a-list of the form
;;; ((a-lzs-local-static . value) ...).

(deflocal *global-variables* () )
;;; holds the current values of global lexical variables after the value was
;;; changed by setq; until the value is changed the current value must be taken
;;; from the annotation initial-value
;;; the values are stored in an a-list ((a-lzs-global-static . value) ...)

;;; The forms in body are executed with variables in vars bound to the
;;; corresponding values of the LZS-forms in values (computed by eval) and the
;;; variable in rst (unless it is () ) bound to the list of values not used for
;;; the other variables. The macro can be used for handling function
;;; applications and let*-forms of the LZS.

(defmacro with-new-values (vars rst values eval-function . body)
  ;; eval-function is applied to every value in values and the result is taken
  ;; for the binding; eval-function may be (), then no evaluation occurs and the
  ;; values are taken as they are
  `(dynamic-let ((variable-environment (dynamic variable-environment)))
                (initialize-variables ,vars ,rst ,values ,eval-function)
                ,@body))

(defun initialize-variables (vars rst values eval-function)
  (cond ((null vars) (initialize-rest rst values eval-function))
        ((null values) (eval-error-too-few-arguments))
        (t (push (cons (first vars)
                       (if eval-function
                           (funcall eval-function (first values))
                         (first values)))
                 (dynamic variable-environment))
           (initialize-variables (rest vars) rst
                                 (rest values) eval-function))))

(defun initialize-rest (rst values eval-function)
  (cond ((and (null rst) values)
         (eval-error-too-many-arguments))
        ((null rst))
        (t (push (cons rst
                       (if eval-function
                           (mapcar eval-function values)
                         values))
                 (dynamic variable-environment)))))

#module-end
