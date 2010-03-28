;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;                 OPS5 for EuLisp System 'youtoo'
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; File   : ops5-def.em
;;; Date   : 24 Jul 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Desc.  : Definitions for main ops5 module.
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defmodule ops5-def
;;; Uncomment this block to run under youtoo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BEGIN_YOUTOO
(syntax (macros macros-tag) 
import (level1 basic cond-el-gf wm conflict)) 
;;; END_YOUTOO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Uncomment this block to run under euscheme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BEGIN_EUSCHEME
;;  (import (level0 cond-el-gf wm conflict))
;;; END_EUSCHEME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (print "### ops5-def")
  (defclass <ops5-system> ()
    ((ce-manager
      reader:  ce-manager
      writer:  set-ce-manager)
     (cr-manager
      default: (make-cr-manager 'mea)
      reader:  cr-manager
      writer:  set-cr-manager)
     (wm-manager
      default: (make-wm-manager)
      reader:  wm-manager
      writer:  set-wm-manager)))
  
  (export ce-manager set-ce-manager
          cr-manager set-cr-manager
          wm-manager set-wm-manager
          <ops5-system>)
) ;; module: ops5-def
