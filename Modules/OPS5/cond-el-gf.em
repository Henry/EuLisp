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
;;; Title: Generic functions for condition element
;;;  Library: ops5
;;;  Authors: Tracy Gardner
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------

(defmodule cond-el-gf
  (syntax (syntax-0
           macros-tag)
   import (level-0
           basic)
   export (ce-j-tests
           set-ce-j-tests
           ce-jv-vals
           set-ce-jv-vals
           ce-rating
           is-satisfied
           match-insert-ce
           match-remove-ce
           make-ce-manager
           cond-els
           set-cond-els
           insert-new-ce
           <ce-manager>
           print-ces
           next-id
           set-next-id
           match-insert
           match-remove))

;;  (print "### cond-el-gf" nl)

;;;-----------------------------------------------------------------------------
;;; Generic function to access pos-j-tests and neg-j-tests transparently
;;;-----------------------------------------------------------------------------
(defgeneric ce-j-tests (ce))
(defgeneric set-ce-j-tests (ce x))

;;;-----------------------------------------------------------------------------
;;; Generic function to access pos-jv-vals and neg-jv-vals transparently
;;;-----------------------------------------------------------------------------
(defgeneric ce-jv-vals (ce))
(defgeneric set-ce-jv-vals (ce x))
(defgeneric ce-rating (ce))

;;;-----------------------------------------------------------------------------
;;; is-satisfied
;;;-----------------------------------------------------------------------------
(defgeneric is-satisfied (ce))
(defgeneric match-insert-ce (wme ce cr-manager))
(defgeneric match-remove-ce (ts ce cr-manager))

;;;-----------------------------------------------------------------------------
;;; <ce-manager>
;;;-----------------------------------------------------------------------------
(defclass <ce-manager> ()
  ((next-id
    default: 1
    reader: next-id
    writer: set-next-id)
   (cond-els
    default: NIL
    reader:  cond-els
    writer:  set-cond-els))
  constructor: (make-ce-manager))
(defgeneric insert-new-ce (ce-manager new-ce prod))
(defgeneric print-ces (ce-manager))
(defgeneric match-insert (ce-manager wme cr-manager))
(defgeneric match-remove (ce-manager wme cr-manager))

;;;-----------------------------------------------------------------------------
)  ;; End of module cond-el-gf
;;;-----------------------------------------------------------------------------
