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
;;;  Title: macros for compiler conditions
;;;-----------------------------------------------------------------------------

#module compiler-conditions
(import (eulisp0
         (only (get-option
                check-options)
               option-lists)
         (rename ((format cl:format)
                  (defclass cl:defclass)
                  (mapcar cl:mapcar)
                  (make-instance cl:make-instance)
                  (error cl:error))
                 (only (format
                        defclass
                        setf
                        make-instance
                        error)
                       common-lisp)))
 syntax (eulisp0)
 export (compiler-error
         define-compiler-condition))


;; local macro definitions
(defmacro define-compiler-condition (name supers format . options)
  `(defcondition ,name ,supers
                 ((message-format :initform ,format :accessor message-format)
                  (message-format-options :initform ',options
                                          :accessor message-format-options))))

;; Create a dummy compiler-condition so that the message-format and
;; message-format-options are known to the compiler
(define-compiler-condition <dummy-compiler-condition> (<condition>)
  "dummy-compiler-condition")

(defun compiler-error (condition-class continuation . option-list)
  (compiler-error-dispatch (cl:make-instance condition-class)
                           continuation
                           option-list))

(defgeneric compiler-error-dispatch (condition continuation option-list))

(defmethod compiler-error-dispatch
  ((condition-instance <condition>) continuation option-list)
  (let ((fmt (message-format condition-instance))
        (options (message-format-options condition-instance)))
    ;;check option list consistency
    (option-lists::check-options options nil nil option-list)
    (let ((fmt-args (cl:mapcar (lambda(x)
                                 (option-lists::get-option x option-list nil))
                               options)))
      ;;      (setf (condition-message condition-instance)
      ;;            (apply #'cl:format nil
      ;;                   (cons fmt fmt-args )))
      ;;      (signal condition-instance continuation)
      (apply #'cl:format t (cons fmt fmt-args) );;only a hack
      (cl:error "~%compiler error signalled for condition ~s~%"
                condition-instance)
      )))

(defmethod compiler-error-dispatch (condition-instance continuation option-list)
  (signal condition-instance continuation)
  (cl:error "~%compiler error signalled for condition ~s~%" condition-instance))

#module-end
