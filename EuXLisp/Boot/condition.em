;;; condition.em
;;; Euscheme code Copyright (c) 1994 Russell Bradford

(defmodule condition
  (import (root telos)
   export (defcondition
            condition?
            condition-message
            condition-value
            <condition>
            <telos-condition>
            <telos-error>
            <telos-general-error>
            <no-applicable-method>
            <no-next-method>
            <incompatible-method-domain>
            <telos-bad-ref>
            <arithmetic-condition>
            <arithmetic-error>
            <error>
            <general-error>
            <bad-type>
            <unbound-error>
            <compilation-error>
            <compilation-general-error>
            <macro-error>
            <socket-error>
            <syntax-error>
            <user-interrupt>))

(defclass <condition> ()
  ((message reader: condition-message
            ;;            default: "no message"
            keyword: message:))
  predicate: condition?
  abstract?: t)

(defcondition <error> <condition>
  ((value reader: error-value
          keyword: value:))
  abstract?: t)

(defcondition <general-error> <error> ())

(defcondition <telos-condition> <condition>
  ()
  abstract?: t)

(defcondition <telos-error> <telos-condition>
  ((value reader: telos-error-value
          keyword: value:))
  abstract?: t)

(defcondition <telos-general-error> <telos-error> ())
(defcondition <no-applicable-method> <telos-error> ())
(defcondition <no-next-method> <telos-error> ())
(defcondition <incompatible-method-domain> <telos-error> ())

(defcondition <telos-bad-ref> <telos-error>
  ((expected-type default: <class>)))

(defcondition <arithmetic-condition> <condition>
  ()
  abstract?: t)

(defcondition <arithmetic-error> <arithmetic-condition>
  ((value default: "no-value")))

(defcondition <bad-type> <error>
  ((expected-type default: <class>)))

(defcondition <unbound-error> <error> ())

(defcondition <compilation-error> <error>
  ()
  abstract?: t)

(defcondition <compilation-general-error> <compilation-error> ())
(defcondition <macro-error> <compilation-error> ())
(defcondition <user-interrupt> <condition> ())
(defcondition <socket-error> <error> ())
(defcondition <syntax-error> <error> ())

; reusable conditions for run-time errors
(deflocal general-error (make <general-error>))
(deflocal telos-error (make <telos-general-error>))
(deflocal telos-bad-ref (make <telos-bad-ref>))
(deflocal no-applicable-method-error (make <no-applicable-method>))
(deflocal no-next-method-error (make <no-next-method>))
(deflocal incompatible-method (make <incompatible-method-domain>))
(deflocal bad-type-error (make <bad-type>))
(deflocal unbound-error (make <unbound-error>))
(deflocal arith-error (make <arithmetic-error>))
(deflocal user-interrupt (make <user-interrupt>))
(deflocal compilation-error (make <compilation-general-error>))
(deflocal macro-error (make <macro-error>))
(deflocal socket-error (make <socket-error>))
(deflocal syntax-error (make <syntax-error>))

;;;-----------------------------------------------------------------------------
)  ;;  End of module condition
;;;-----------------------------------------------------------------------------
