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
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module c-typing
(import (eulisp0
         list-ext
         lzs
         accessors
         representation ;whc-classes
         lzs-mop
         expand-literal
         tail-module
         messages
         (only (<tempvar>) mzs) ; to avoid compiler errors
         (only (svref
                vector
                make-instance)
               common-lisp))
 syntax (eulisp0
         dynamic
         class-ext)
 export (type-expr-for-c
         type-args-for-c
         <cast>
         ?expression
         get-type
         result-type
         function-signature
         global-var-type
         is-pointer))

;;;-----------------------------------------------------------------------------
;;; switches and variables
;;;-----------------------------------------------------------------------------
(defvar *no-cast-if-compatible-representation* t)
(defvar *function* nil)

;;;-----------------------------------------------------------------------------
;;; Classes
;;;-----------------------------------------------------------------------------
(defstandardclass <cast> ()
  (type :reader :initarg)
  (expression :reader :initarg))

;;;-----------------------------------------------------------------------------
;;; Main Functions
;;;-----------------------------------------------------------------------------
(defun type-expr-for-c (type expr)
  (let ((expr-type (get-type expr)))
    (cond ((and (eq type %function)             ; this must be removed if the type
                (eq expr-type %function))       ; specifier (%function <return>)
           ;; is implemented
           (make-instance <cast>
                          :type type
                          :expression expr))
          ((eq expr-type type) expr)
          ((and (dynamic *no-cast-if-compatible-representation*)
                (compatible-representation-p type (?representation type)
                                             expr-type (?representation expr-type)))
           expr)
          ((or (is-subclass expr-type type)
               (is-subclass type expr-type))
           ;; this is the only case where different types are not a type error and where
           ;; an explicit cast must be inserted because C has no subtypes/subclasses as
           ;; in EL
           (make-instance <cast> :type type
                          :expression expr))

          (t (write-message-conditional
              3
              "internal compiler error"
              "~%Error~@[ in function ~A~]: ~
               ~%mismatching types: required ~A, given ~A~
               ~%trying a cast to ~A for ~/EXPR/"
              (if (dynamic *function*)
                  (?identifier (dynamic *function*))
                nil)
              (?identifier type)
              (?identifier expr-type)
              (?identifier type)
              expr)
             (make-instance <cast> :type type
                            :expression expr)
             ))))

(defun is-subclass (class superclass)
  (member class (~class-precedence-list superclass)))

(defgeneric compatible-representation-p (class1 rep1 class2 rep2))

(defmethod compatible-representation-p (class1 rep1 class2 rep2)
  nil)

(defmethod compatible-representation-p (class1 (rep1 <%direct>)
                                               class2 rep2)
  (setq class1
        (~slot-description-type (car (~class-slot-descriptions class1))))
  (compatible-representation-p class1 (?representation class1)
                               class2 rep2))

(defmethod compatible-representation-p (class1 rep1
                                               class2 (rep2 <%direct>))
  (setq class2
        (~slot-description-type (car (~class-slot-descriptions class2))))
  (compatible-representation-p class1 rep1
                               class2 (?representation class2)))

(defmethod compatible-representation-p (class1 (rep1 <%pointer>)
                                               class2 (rep2 <%pointer-to-void>))
  t)

(defmethod compatible-representation-p (class1 (rep1 <%pointer-to-void>)
                                               class2 (rep2 <%pointer>))
  t)

(defgeneric get-type (expr))

(defmethod get-type (expr)
  (?class (expand-literal expr)))

;;*5*
(defmethod get-type ((var <static>))
  (var-type var))

(defmethod get-type ((var <tempvar>))
  ;;!!! it is an error if a tempvar appears
  (var-type var))

(defmethod get-type ((var <var-ref>))
  (var-type (?var var)))

(defgeneric var-type (var))
(defmethod var-type (var) %object) ; handles bad and not yet recognized cases
(defmethod var-type ((var <var>))
  (or (?type var) %object))

(defmethod get-type ((call <app>))
  (if (special-sys-fun-p (?function call))
      (result-type call)   ;; use the inferred type because of
    ;; overloading
    (result-type (?function call))))    ; use the declared type

(defmethod get-type ((const <named-const>))
  (or (?type const) %object))

(defmethod get-type ((form <setq-form>))
  (get-type (?location form)))

(defmethod get-type ((form <get-slot-value>))
  (~slot-description-type (?slot form)))

(defmethod get-type ((form <set-slot-value>))
  (~slot-description-type (?slot form)))

(defun result-type (expr)
  (svref (or (?type-descr expr)
             (range-and-domain expr)
             (vector %object %object %object %object %object %object %object
                     %object %object))
         0))

(defun type-args-for-c (signature args)
  (type-args-1 signature args 1))

(defun type-args-1 (signature args i)
  (if (null? args) nil
    (cons (type-expr-for-c (svref signature i)
                           (first args))
          (type-args-1 signature (rest args) (+ i 1)))))

(defun function-signature (fun)
  (or (?type-descr fun)
      ;; if the type-descr is (), which is an error *9*
      (range-and-domain fun)
      (vector %object %object %object %object %object %object %object %object %object)))

(defun range-and-domain (fun) ;*9*
  (map (lambda (type)
         (if (named-const-p type)
             (?value type)
           type))
       (?range-and-domain fun)))

(defun global-var-type (var)
  (or (?type var)
      (setf (?type var)
            (if (and (named-const-p var)
                     (null? (eq (?value var) ^unknown)))
                (~class-of (?value var))
              %object))))

(defgeneric is-pointer (representation-or-class))

(defmethod is-pointer ((class <class-def>))
  (is-pointer (?representation class)))

(defmethod is-pointer ((representation <%pointer>)) t)

(defmethod is-pointer (representation) nil)


#module-end
