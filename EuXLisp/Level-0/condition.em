;;; Copyright 1994 Russell Bradford
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'EuXLisp'
;;;-----------------------------------------------------------------------------
;;
;;  EuXLisp is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  EuXLisp is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: EuLisp Level-0 condition module
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule condition
  (import (root
           telos)
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
            <syntax-expansion-error>
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
(defcondition <syntax-expansion-error> <compilation-error> ())
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
(deflocal syntax-expansion-error (make <syntax-expansion-error>))
(deflocal socket-error (make <socket-error>))
(deflocal syntax-error (make <syntax-error>))

;;;-----------------------------------------------------------------------------
)  ;;  End of module condition
;;;-----------------------------------------------------------------------------
