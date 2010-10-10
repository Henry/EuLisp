;;; Copyright 1995 Tracy Gardner & University of Bath
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;
;;  Youtoo is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: Condition Element
;;;  Library: ops5
;;;  Authors: Tracy Gardner
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------

(defmodule cond-el-1
  (syntax (syntax-0
           macros-tag)
   import (level-0
           basic
           cond-el-gf
           prod-gf
           wm-gf
           ops5-def
           ops-out)
   export (make-pos-join-ce
           make-pos-njoin-ce
           make-neg-join-ce
           make-neg-njoin-ce
           make-neg-from-njoin-ce
           make-pos-from-njoin-ce
           ce-c-tests
           ce-v-tests
           ce-j-tests
           set-ce-c-tests
           set-ce-v-tests
           set-ce-j-tests
           ce-class-name
           <condition-element>
           <pos-join-ce>
           <neg-join-ce>
           <pos-njoin-ce>
           <neg-njoin-ce>
           ce-num-matched
           set-ce-num-matched
           ce-matches
           set-ce-matches
           ce-prods
           set-ce-prods
           ce-rating
           set-ce-rating
           ce-id
           set-ce-id))

(print "### cond-el-1.em" nl)

;;;-----------------------------------------------------------------------------
;;; <condition-element>
;;;-----------------------------------------------------------------------------
(defclass <condition-element> ()
  ((ce-class-name                 ; class of WM-Element involved in condition
    keyword:  ce-class-name:
    reader:   ce-class-name)
   (prods                      ; productions which use this cond-el
    keyword:  prods:
    default:  ()
    reader:   ce-prods
    writer:   set-ce-prods)
   (c-tests                    ; constant tests in condition element
    default:  ()
    keyword:  c-tests:
    reader:   ce-c-tests
    writer:   set-ce-c-tests)
   (v-tests                    ; (non-join) variable tests in
    default:  ()               ; condition element
    keyword:  v-tests:
    reader:   ce-v-tests
    writer:   set-ce-v-tests)
   (num-matched                ; number of WM-Elements which currently
    default:  0                ; pass the tests
    reader:   ce-num-matched
    writer:   set-ce-num-matched)
   (matches                    ; WM-Elements that match
    default:  ()
    reader:   ce-matches
    writer:   set-ce-matches)
   (rating
    default:  0
    reader:   rating
    writer:   set-ce-rating)
   (ce-id
    reader: ce-id
    writer: set-ce-id))
  constructor: (make-condition-element ce-class-name:))

(defmethod ce-rating ((ce <condition-element>))
  (rating ce))

;;;-----------------------------------------------------------------------------
;;; generic-prin
;  Print out a <condition-element> is a useful format.
;;;-----------------------------------------------------------------------------
(defmethod generic-print ((ce <condition-element>) (s <stream>))
  (sformat ops-out "Condition Element: ~a ~a ~a~% Prods: "
           (class-of ce) (ce-id ce) (ce-class-name ce))
  (do
   (lambda (x)
     (sformat ops-out " ~a" (p-name x)))
   (ce-prods ce))
  (sformat ops-out " ~%")
  ;;    (sformat ops-out "Constant Tests: ~a~%" (ce-c-tests ce))
  ;;    (sformat ops-out "Variable Tests: ~a~%" (ce-v-tests ce))
  ;;    (when (or (eql (class-of ce) <pos-join-ce>)
  ;;            (eql (class-of ce) <neg-join-ce>))
  ;;        (sformat ops-out "Join-Var Tests: ~a~%" (ce-j-tests ce))
  ;;        (sformat ops-out "Join Var Vals: ~a~%" (ce-jv-vals ce)))
  ;    (sformat ops-out "~% Matching Timestamps: ")
  ;    (do
  ;     (lambda (x)
  ;       (sformat ops-out " ~a" (car x)))
  ;     (ce-matches ce))
  )

(defclass <pos-join-ce> <condition-element>
  ((j-tests                    ; join-variable tests in condition element
    default: ()
    reader: pos-j-tests
    writer: set-pos-j-tests)
   (jv-vals                    ; table containing value-ts pairs for
    default: ()                ; each join variable
    reader: pos-jv-vals
    writer: set-pos-jv-vals))
  constructor: (make-pos-join-ce ce-class-name:)
  constructor: (make-pos-from-njoin-ce ce-class-name:
                                       c-tests: v-tests:))
(defclass <pos-njoin-ce> <condition-element> ()
  constructor: (make-pos-njoin-ce ce-class-name:))

(defclass <neg-join-ce> <condition-element>
  ((j-tests                    ; join-variable tests in condition element
    default:  ()
    reader: neg-j-tests
    writer: set-neg-j-tests)
   (jv-vals                    ; table containing value-ts pairs for
    default: ()                ; each join variable
    reader: neg-jv-vals
    writer: set-neg-jv-vals))
  constructor: (make-neg-join-ce ce-class-name:)
  constructor: (make-neg-from-njoin-ce ce-class-name:
                                       c-tests: v-tests:))

(defclass <neg-njoin-ce> <condition-element> ()
  constructor: (make-neg-njoin-ce ce-class-name:))

;; Generic function to access pos-j-tests and neg-j-tests
;; transparently
(defmethod ce-j-tests ((ce <pos-join-ce>))
  (pos-j-tests ce))

(defmethod ce-j-tests ((ce <neg-join-ce>))
  (neg-j-tests ce))

(defmethod set-ce-j-tests ((ce <pos-join-ce>) x)
  (set-pos-j-tests ce x))

(defmethod set-ce-j-tests ((ce <neg-join-ce>) x)
  (set-neg-j-tests ce x))

;; Generic function to access pos-jv-vals and neg-jv-vals
;; transparently
(defmethod ce-jv-vals ((ce <pos-join-ce>))
  (pos-jv-vals ce))

(defmethod ce-jv-vals ((ce <neg-join-ce>))
  (neg-jv-vals ce))

(defmethod set-ce-jv-vals ((ce <pos-join-ce>) x)
  (set-pos-jv-vals ce x))

(defmethod set-ce-jv-vals ((ce <neg-join-ce>) x)
  (set-neg-jv-vals ce x))

;;;-----------------------------------------------------------------------------
)  ;; End of module cond-el-1
;;;-----------------------------------------------------------------------------
