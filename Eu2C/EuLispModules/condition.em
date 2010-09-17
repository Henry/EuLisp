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
;;; Title: EuLisp Level-0 condition module
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
           condition-i
           (only (equal)
                 compare))
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
           cerror))

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
;; <general-condition> defined in condition-i
;;;-----------------------------------------------------------------------------
(defcondition <invalid-operator> <general-condition>
  ((invalid-operator type <object>
                     default ()
                     keyword invalid-operator:
                     accessor invalid-operator)))

(defcondition <range-condition> <general-condition>
  ((argument type <object>
             default ()
             keyword argument:
             accessor argument)))

(defcondition <cannot-update-setter> <general-condition>
  ((accessor type <object>
             default ()
             keyword accessor:
             accessor accessor)
   (updater type <object>
            default ()
            keyword updater:
            accessor updater)))

(defcondition <no-setter> <general-condition>
  ((object type <object>
           default ()
           keyword object:
           accessor object)))

(defcondition <domain-condition> <general-condition>
  ((result type <object>
           default ()
           keyword result:
           accessor result)))

;;;-----------------------------------------------------------------------------
;;; Environment condition
;;;-----------------------------------------------------------------------------
(defcondition <environment-condition> () ())

;;;-----------------------------------------------------------------------------
;;; Arithmetic condition
;; <arithmetic-condition> defined in condition-i
;;;-----------------------------------------------------------------------------

(defcondition <division-by-zero> <arithmetic-condition> ())

;;;-----------------------------------------------------------------------------
;;; Conversion condition
;;;-----------------------------------------------------------------------------
(defcondition <conversion-condition> <condition>
  ((source type <object>
           default ()
           keyword source:
           accessor source)
   (target-class type <object>
                 default ()
                 keyword target-class:
                 accessor target-class)))

(defcondition <no-converter> <conversion-condition> ())

;;;-----------------------------------------------------------------------------
;;; Stream condition --- located in stream-i
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;; Read error located --- in stream-i
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;; Thread condition
;;;-----------------------------------------------------------------------------
(defcondition <thread-condition> <condition>
  ((current-thread type <object>
                   default ()
                   keyword current-thread:
                   accessor current-thread)
   (thread type <object>
           default ()
           keyword thread:
           accessor thread)))

(defcondition <thread-already-started> <thread-condition> ())

(defcondition <wrong-thread-continuation> <thread-condition> ())

(defcondition <wrong-condition-class> <thread-condition> ())

;;;-----------------------------------------------------------------------------
;;; Telos condition
;; <telos-condition> defined in condition-i
;; <no-applicable-method> defined in condition-i
;;;-----------------------------------------------------------------------------

(defcondition <no-next-method> <telos-condition>
  ((method type <object>
           default ()
           keyword method:
           accessor no-next-method-method)
   (operand-list type <object>
                 default ()
                 keyword operand-list:
                 accessor operand-list)))

(defcondition <non-congruent-lambda-lists> <telos-condition>
  ((generic type <object>
            default ()
            keyword generic:
            accessor non-congruent-lambda-lists-generic)
   (method type <object>
           default ()
           keyword method:
           accessor non-congruent-lambda-lists-method)))

(defcondition <incompatible-method-domain> <telos-condition>
  ((generic type <object>
            default ()
            keyword generic:
            accessor incompatible-method-domain-generic)
   (method type <object>
           default ()
           keyword method:
           accessor incompatible-method-domain-method)))

(defcondition <method-domain-clash> <telos-condition>
  ((generic type <object>
            default ()
            keyword generic:
            accessor method-domain-clash-generic)
   (methods type <list>
            default ()
            keyword methods:
            accessor methods)))

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
