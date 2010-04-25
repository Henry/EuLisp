;;;-----------------------------------------------------------------------------
;;;                 OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;; File   : wm-gf.em
;;; Date   : 24 Jul 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Description: Generic functions for wmorking memory.
;;;-----------------------------------------------------------------------------
(defmodule wm-gf
    (syntax (macros macros-tag)
     import (level1 basic))

  (print "### wm-gf.em")

  (defgeneric wme-class-name (wme))
  (defgeneric wme-attrib-vals (wme))
  (defgeneric wme-timestamp (wme))

  (export wme-attrib-vals
   wme-class-name
   wme-timestamp)

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
