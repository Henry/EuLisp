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
;;;  Title: Transformation of %annotate-forms to LZS
;;;  Description:
;;    This module defines the transformation of %annotate-forms of TAIL to the LZS.
;;    %annotate-function: provide information about a function
;;    %annotate-class:    provide information about a class
;;    %annotate-binding:  provide information about a global variable or a constant
;;    The handlers for the keywords of %annotate-... are defined in the a-list
;;    *annotate-...-handlers*. The handlers are functions of three arguments:
;;    1. the object (%annotate-function: instance of LZS-class <fun>
;;                                       %annotate-class: instance of LZS-class <class-def>
;;                                       %annotate-binding: instance of LZS-class <global-static>
;;                                       or <named-const>)
;;    2. the annotate-keyword
;;    3. the form after the keyword (a list or a symbol in most cases)
;;;  Documentation:
;;;  Notes:
;;    A distinction is made between %annotate-function, %annotate-class and
;;    %annotate-binding to make error detection (no function/class-object, bad
;;                                                  keyword) a bit simpler.
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module annotate
(import ((except (assoc) eulisp1)
         lzs
         el2lzs-main
         el2lzs-error
         accessors
         (only (set-special-function
                set-special-class
                set-special-binding
                provide-compiler-info)
               apply-funs)
         (only (set-interpreter call) lzs-eval)
         (only (set-read-side-effect set-write-side-effect) side-effects-h)
         (only (new-signature extend-signature
                              comp-signature renew-signature) ti-signature)
         (only (set-std-discr-fun) generic-dispatch)
         (only (assoc cddr cadr second third remove-if-not mapcar)
               common-lisp))
 syntax (eulisp1
         el2lzs-main
         (only (push) common-lisp))
 export (get-saved-annotations)
 )

;;;-----------------------------------------------------------------------------
;;; Handler
;;;-----------------------------------------------------------------------------

(defun set-inline (fun key value)
  (setf (?inline fun) value))

(defun set-reduce (fun key value)
  ;;
  ;; syntax of value:
  ;;                   ( /binary-function/
  ;;                     /one-argument-translation/
  ;;                     /zero-argument-translation/
  ;;                     /translation-type/)
  ;; /one-argument-translation/ = self | t | (/function/ /args/*)
  ;; args = /constant/ | self
  ;; /zero-argument-translation/ = /constant/ | error
  ;; /translation-type/ = acc | logical | select1 | select2
  ;;
  (setf (car value) (get-function (find-in-lex-env (car value))))
  (if (consp (car (cdr value)))
      (setf (car (car (cdr value)))
            (get-function (find-in-lex-env (car (car (cdr value))))))
    ())
  (setf (?reduce fun) value))

;;;-----------------------------------------------------------------------------
;;; simple setting of different annotations
;;;-----------------------------------------------------------------------------

(defun set-code-identifier (obj key value)
  (cond ((eq key ^code-identifier)
         (setf (?code-identifier obj) value))
        ((eq key ^type-identifier) ;needed for classes
         (setf (?type-identifier obj) value))
        ))

(defun set-setter (fun key setter)
  (setf (?setter fun) (find-in-lex-env setter)))

;;;-----------------------------------------------------------------------------
;;; Handler functions for %annotate-function
;;;-----------------------------------------------------------------------------
;;; If the save-original-value-flag is t then the value of the option is saved
;;; and later retrieved to put it into the .def-file as it was originally given.

(deflocal *annotate-function-handlers*
  `(;(keyword function save-original-value-flag)
    (,^interpreter ,#'set-interpreter t )
    (,^inline ,#'set-inline () )
    (,^read-location ,#'set-read-side-effect () )
    (,^write-location ,#'set-write-side-effect () )
    (,^new-signature ,#'new-signature () )
    (,^extend-signature ,#'extend-signature () )
    (,^renew-signature ,#'renew-signature () )
    (,^comp-signature ,#'comp-signature () )
    (,^is-standard-discriminator ,#'set-std-discr-fun t )
    (,^reduce ,#'set-reduce () )
    (,^init-function ,#'set-module-init-function () )
    (,^is-special-function ,#'set-special-function t )
    (,^code-identifier ,#'set-code-identifier ())
    (,^setter ,#'set-setter ())
    ))

(deflocal *annotate-class-handlers*
  `(;(keyword function save-original-value-flag)
    (,^is-special-class ,#'set-special-class t )
    (,^code-identifier ,#'set-code-identifier ())
    (,^type-identifier ,#'set-code-identifier ())
    ))

(deflocal *annotate-binding-handlers*
  `(;(keyword function save-original-value-flag)
    (,^is-special-binding ,#'set-special-binding t )
    ))

;;;-----------------------------------------------------------------------------
;;; TAIL: %annotate-function
;;;-----------------------------------------------------------------------------
;;; syntax: (%annotate-function <function-id> {<key> <description>}*)
;;; <function-id> : global function binding
;;; <key> : symbol
;;; <description> : any literal
;;; %describe-function provides some informations about functions, which are needed
;;; by the compiler but are not computable

(defgeneric get-function (lzs-object))

(defmethod get-function ((object <fun>)) object)
(defmethod get-function ((object <defined-named-const>))
  (if (eq (?value object) ^unknown)
      (error-invalid-object-for-annotate "function")
    (get-function (?value object))))
(defmethod get-function (object)
  (error-invalid-object-for-annotate "function"))

(deftranssyn (%annotate-function id . options) (whole-form))

(deftransdef (%annotate-function id . options)
  (with-defining-form
   (transdef-%annotate (get-function (find-in-lex-env id))
                       options
                       *annotate-function-handlers*)))

;;;-----------------------------------------------------------------------------
;;; TAIL: %annotate-class
;;;-----------------------------------------------------------------------------
;;; syntax: (%annotate-class <function-id> {<key> <description>}*)
;;; <function-id> : global function binding
;;; <key> : symbol
;;; <description> : any literal
;;; %describe-function provides some informations about functions, which are needed
;;; by the compiler but are not computable

(defgeneric get-class (lzs-object))
(defmethod get-class ((object <class-def>)) object)
(defmethod get-class ((object <defined-named-const>))
  (if (eq (?value object) ^unknown)
      (error-invalid-object-for-annotate "class")
    (get-class (?value object))))
(defmethod get-class (object)
  (error-invalid-object-for-annotate "class"))

(deftranssyn (%annotate-class id . options) (whole-form))

(deftransdef (%annotate-class id . options)
  (with-defining-form
   (transdef-%annotate (get-class (find-in-lex-env id))
                       options
                       *annotate-class-handlers*)))

;;;-----------------------------------------------------------------------------
;;; TAIL: %annotate-binding
;;;-----------------------------------------------------------------------------
;;; syntax: (%annotate-binding <var-or-const-id> {<key> <description>}*)
;;; <var-or-const-id> : global binding defined by deflocal or defconstant
;;; <key> : symbol
;;; <description> : any literal
;;; %describe-binding provides some informations about variables and constants,
;;; which are needed by the compiler but are not computable

(deftranssyn (%annotate-binding id . options) (whole-form))

(deftransdef (%annotate-binding id . options)
  (with-defining-form
   (transdef-%annotate (find-in-lex-env id)
                       options
                       *annotate-binding-handlers*)))

;;;-----------------------------------------------------------------------------
;;; common stuff for %annotate-....
;;;-----------------------------------------------------------------------------

(deflocal *saved-annotations* ())

(defun transdef-%annotate (object options handler-table)
  (if (null? options)
      ()
    (let* ((key (car options))
           (option (cadr options))
           (handler (find-annotate-handler key handler-table
                                           object option)))
      (when handler (funcall handler object key option))
      (transdef-%annotate object (cddr options) handler-table))))

(defun find-annotate-handler (key handler-table object option)
  (let ((entry (assoc key handler-table)))
    (if entry
        (progn
          (when (third entry) ; save original form for .def-file?
                (push (list object key option)
                      *saved-annotations*))
          (second entry))
      (progn
        (error-invalid-key-for-annotate key)
        ()))))

(defun get-saved-annotations (object)
  (mapcar #'cdr
          (remove-if-not (lambda (entry)
                           (eq (car entry) object))
                         *saved-annotations*)))

;;;-----------------------------------------------------------------------------
;;; %provide-compiler-info
;;;-----------------------------------------------------------------------------

(deftranssyn (%provide-compiler-info . options) (whole-form))

(deftransdef (%provide-compiler-info . options)
  (with-defining-form
   (labels ((traverse-options (options)
                              (unless (null? options)
                                      (provide-compiler-info (car options)
                                                             (cadr options))
                                      (traverse-options (cddr options)))))
           (traverse-options options))
   ))

#module-end
