;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;                 OPS5 for EuLisp System 'youtoo'
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; File   : wm-gf.em
;;; Date   : 24 Jul 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Desc.  : Generic functions for wmorking memory.
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defmodule wm-gf
;;; Uncomment this block to run under youtoo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BEGIN_YOUTOO
(syntax (macros macros-tag) 
import (level1 basic)) 
;;; END_YOUTOO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Uncomment this block to run under youtoo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BEGIN_EUSCHEME
;;  (import (level0))
;;; END_EUSCHEME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (print "### wm-gf.em")
 (defgeneric wme-class-name (wme))
 (defgeneric wme-attrib-vals (wme))
 (defgeneric wme-timestamp (wme))
   (export wme-attrib-vals
           wme-class-name
           wme-timestamp)
) ;; module: wm-gf
