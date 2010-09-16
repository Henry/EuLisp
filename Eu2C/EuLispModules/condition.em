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
;;;  Title: EuLisp Level-0 condition module
;;;  Authors: E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------

(defmodule condition
  (import (tail
           eulisp-kernel
           function
           standard-generic-function
           (only (make)
                 object-0-i)
           formatted-io
           stream
           (only (equal)
                 compare)
           condition-i)
   syntax (tail
           function
           standard-generic-function
           condition-ii)
   export (<condition>
           <general-condition>
           <invalid-operator>
           <domain-condition>
           <cannot-update-setter>
           <no-setter>
           <range-condition>
           <environment-condition>
           <arithmetic-condition>
           <division-by-zero>
           <conversion-condition>
           <no-converter>
           <thread-condition>
           <thread-already-started>
           <wrong-thread-continuation>
           <wrong-condition-class>
           <telos-condition>
           <no-next-method>
           <non-congruent-lambda-lists>
           <incompatible-method-domain>
           <no-applicable-method>
           <method-domain-clash>
           ;;functions and macros
           condition-message
           signal
           error
           cerror
           defcondition-error-string))

;;;-----------------------------------------------------------------------------
;;; A very simple defcondition
;;;-----------------------------------------------------------------------------
(defconstant defcondition-error-string
  "Superclass in defcondition is not a subclass of <condition>")

(defmacro defcondition (condition-class-name
                        super-class-name
                        slots . init-options)
  `(progn (if (%subclass? (if ,super-class-name ,super-class-name <condition>)
                          <condition>)
              ()
            (error <condition>
                   defcondition-error-string))
          (%define-standard-class (,condition-class-name <class>)
            ,(if super-class-name super-class-name <condition>)
            ,slots
            representation pointer-to-struct
            allocation multiple-type-card
            ,@init-options)))

;;;-----------------------------------------------------------------------------
;;; with-handler
;;;-----------------------------------------------------------------------------
(defmacro with-handler (handler-fun . protected-forms)
  `(dynamic-let ((shadowed-default-handler
                  (dynamic dynamic-default-signal-handler))
                 (dynamic-default-signal-handler
                  (lambda(condition continuation)
                    (,handler-fun condition continuation)
                    (let ((active-handler
                           (dynamic shadowed-default-handler )))
                      (active-handler
                       condition continuation )))))
                ,@protected-forms))

;;;-----------------------------------------------------------------------------
;;; Execution condition
;;;-----------------------------------------------------------------------------
;;<general-condition> already defined in module condition-i

(%define-standard-class (<invalid-operator> <class>)
  <general-condition>
  ((invalid-operator type <object>
                     default ()
                     keyword invalid-operator:
                     accessor invalid-operator))
  representation pointer-to-struct
  allocation multiple-type-card)


(%define-standard-class (<range-condition> <class> )
  <general-condition>
  ((argument type <object>
             default ()
             keyword argument:
             accessor argument))
  representation pointer-to-struct
  allocation multiple-type-card)

(%define-standard-class (<cannot-update-setter> <class> )
  <general-condition>
  ((accessor type <object>
             default ()
             keyword accessor:
             accessor accessor)
   (updater type <object>
            default ()
            keyword updater:
            accessor updater))
  representation pointer-to-struct
  allocation multiple-type-card)

(%define-standard-class (<no-setter> <class> )
  <general-condition>
  ((object type <object>
           default ()
           keyword object:
           accessor object))
  representation pointer-to-struct
  allocation multiple-type-card)

(%define-standard-class (<domain-condition> <class> )
  <general-condition>
  ((result type <object>
           default ()
           keyword result:
           accessor result))
  representation pointer-to-struct
  allocation multiple-type-card)

;;;-----------------------------------------------------------------------------
;;; Environment condition
;;;-----------------------------------------------------------------------------
(define-condition-class <environment-condition> <condition> )

;;;-----------------------------------------------------------------------------
;;; Arithmetic condition
;;;-----------------------------------------------------------------------------
;;condition <arithmetic-condition> already defined in module condition-i

(define-condition-class <division-by-zero> <arithmetic-condition>)

;;;-----------------------------------------------------------------------------
;;; Conversion condition
;;;-----------------------------------------------------------------------------
(%define-standard-class (<conversion-condition> <class> )
  <condition>
  ((source type <object>
           default ()
           keyword source:
           accessor source)
   (target-class type <object>
                 default ()
                 keyword target-class:
                 accessor target-class))
  representation pointer-to-struct
  allocation multiple-type-card)

(define-condition-class <no-converter> <conversion-condition> )

;;;-----------------------------------------------------------------------------
;;; Stream condition
;;;-----------------------------------------------------------------------------
;; located in stream-i

;;;-----------------------------------------------------------------------------
;;; Read error
;;;-----------------------------------------------------------------------------
;; located in stream-i

;;;-----------------------------------------------------------------------------
;;; Thread condition
;;;-----------------------------------------------------------------------------
(%define-standard-class (<thread-condition> <class> )
  <condition>
  ((current-thread type <object>
                   default ()
                   keyword current-thread:
                   accessor current-thread)
   (thread type <object>
           default ()
           keyword thread:
           accessor thread))
  representation pointer-to-struct
  allocation multiple-type-card)

(define-condition-class <thread-already-started> <thread-condition>)

(define-condition-class <wrong-thread-continuation> <thread-condition> )

(define-condition-class <wrong-condition-class> <thread-condition> )

;;;-----------------------------------------------------------------------------
;;; Telos condition
;;;-----------------------------------------------------------------------------
;; condition <telos-condition> already defined in mmodule condition-i

(%define-standard-class (<no-next-method> <class> )
  <telos-condition>
  ((method type <object>
           default ()
           keyword method:
           accessor no-next-method-method)
   (operand-list type <object>
                 default ()
                 keyword operand-list:
                 accessor operand-list))
  representation pointer-to-struct
  allocation multiple-type-card)

(%define-standard-class (<non-congruent-lambda-lists> <class> )
  <telos-condition>
  ((generic type <object>
            default ()
            keyword generic:
            accessor non-congruent-lambda-lists-generic)
   (method type <object>
           default ()
           keyword method:
           accessor non-congruent-lambda-lists-method))
  representation pointer-to-struct
  allocation multiple-type-card)

(%define-standard-class (<incompatible-method-domain> <class> )
  <telos-condition>
  ((generic type <object>
            default ()
            keyword generic:
            accessor incompatible-method-domain-generic)
   (method type <object>
           default ()
           keyword method:
           accessor incompatible-method-domain-method))
  representation pointer-to-struct
  allocation multiple-type-card)

;;condition <no-applicable-method> already defined in module condition-i

(%define-standard-class (<method-domain-clash> <class> )
  <telos-condition>
  ((generic type <object>
            default ()
            keyword generic:
            accessor method-domain-clash-generic)
   (methods type <list>
            default ()
            keyword methods:
            accessor methods))
  representation pointer-to-struct
  allocation multiple-type-card)

;;;-----------------------------------------------------------------------------
;;; define generic fcn and methods for accessors called method and generic
;;;-----------------------------------------------------------------------------
(defgeneric method (condition))

(defmethod method((c <no-next-method>))
  (no-next-method-method c))

(defmethod method((c <non-congruent-lambda-lists>))
  (non-congruent-lambda-lists-method c))

(defmethod method((c <incompatible-method-domain>))
  (incompatible-method-domain-method c))

(defgeneric generic(condition))

(defmethod generic((c <non-congruent-lambda-lists>))
  (non-congruent-lambda-lists-generic c))

(defmethod generic((c <incompatible-method-domain>))
  (incompatible-method-domain-generic c))

(defmethod generic((c <method-domain-clash>))
  (method-domain-clash-generic c))

;;;-----------------------------------------------------------------------------
;;; Establish dynamic-default-signal-handler
;;;-----------------------------------------------------------------------------
(defun error-handler-with-format (condition continuation)
  (format 't "~%-----------------------A Condition has been signaled------------------~%message: ~a~%"
          (condition-message condition))
  (if continuation
      (progn
        (format 't
                "Enter <return> to exit~%      cont to continue with continuation~%> ")
        (if (equal "cont" (read-line stdin))
            (progn
              (format 't "~%Enter return-value:")
              (continuation (read)))
          (progn (format 't "~%EXIT ")
                 (c-exit #%i2)
                 condition)))
    (progn (c-exit #%i2)
           condition)))

(defun returning-signal-handler(condition continuation))

(defglobal dynamic-default-signal-handler returning-signal-handler)

;;;-----------------------------------------------------------------------------
;;; Define signal working  with dynamic-default-signal-handler
;;;-----------------------------------------------------------------------------
(defun signal-dynamic (condition continuation . thread)
  ;;threads not yet handled
  (let ((handler (dynamic dynamic-default-signal-handler)))
    (handler condition continuation)))

;;;-----------------------------------------------------------------------------
;;; Set final values for signal-function error and cerror
;;;-----------------------------------------------------------------------------
(setq signal signal-dynamic)

(defun cerror-with-format (condition-class error-message . init-args)
  (let ((condition (apply make condition-class
                          message: error-message
                          init-args)))
    (let/cc cerror-fixed-up
      (signal condition cerror-fixed-up)
      (error-handler-with-format condition cerror-fixed-up))))

(setq cerror cerror-with-format)

(defun error-with-signal-dynamic (condition-class error-message . init-args)
  (let ((condition (apply make condition-class
                          message: error-message
                          init-args)))
    (signal-dynamic condition ())
    (error-handler-with-format condition ())))

(setq error error-with-signal-dynamic)

;;;-----------------------------------------------------------------------------
;;; type schemes for type inference
;;;-----------------------------------------------------------------------------
(%annotate-function cerror-with-format new-signature
  (((var0 var1 var2 var3)
    ((var var0) (atom? <null>))
    ((var var1) (atom? <class>))
    ((var var2) (atom? <string>))
    ((var var3) (atom? <list>)))))

(%annotate-function error-with-signal-dynamic new-signature
  (((var0 var1 var2 var3)
    ((var var0) (atom? <null>))
    ((var var1) (atom? <class>))
    ((var var2) (atom? <string>))
    ((var var3) (atom? <list>)))))

(%annotate-function signal-dynamic new-signature
  (((var0 var1 var2 var3)
    ((var var0) (atom? <null>))
    ((var var1) (atom? <condition>))
    ((var var2) (atom? <object>)) ; supposed to be <continuation>
    ((var var3) (atom? <list>)))))

(%annotate-function error-handler-with-format new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <object>))
    ((var var1) (atom? <condition>))
    ((var var2) (atom? <object>))))) ; supposed to be <continuation>

;;;-----------------------------------------------------------------------------
)  ;; End of module condition
;;;-----------------------------------------------------------------------------
