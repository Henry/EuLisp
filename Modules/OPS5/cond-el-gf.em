;;;-----------------------------------------------------------------------------
;;;                 OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;; File   : cond-el-gf.em
;;; Date   : 24 Jul 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Description: Generic functions for cond-el.em
;;;-----------------------------------------------------------------------------
(defmodule cond-el-gf
    (syntax (macros macros-tag)
     import (level1 basic))

  ;;  (print "### cond-el-gf")

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

  (export ce-j-tests set-ce-j-tests
          ce-jv-vals set-ce-jv-vals
          ce-rating is-satisfied
          match-insert-ce match-remove-ce
          make-ce-manager cond-els set-cond-els
          insert-new-ce <ce-manager> print-ces
          next-id set-next-id
          match-insert match-remove)

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
