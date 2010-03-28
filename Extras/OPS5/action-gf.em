;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;                 OPS5 for EuLisp System 'youtoo'
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; File   : action-gf.em
;;; Date   :  8 Aug 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Desc.  : 
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defmodule action-gf
;;; Uncomment this block to run under youtoo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BEGIN_YOUTOO
(syntax (macros macros-tag) 
import (level1 basic)) 
;;; END_YOUTOO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Uncomment this block to run under euscheme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BEGIN_EUSCHEME
;;  (import (level0))
;;; END_EUSCHEME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defclass <action> ()
    ())
  (defclass <halt-action> (<action>)
    ()
    constructor: (make-halt-action))
  (defgeneric execute ((action <action>) pi 
                       wm-manager ce-manager cr-manager))
  (defgeneric get-ts (ce-num prod ce-ts))
  (export execute <action> <halt-action> make-halt-action get-ts)
) ;; module: action-gf
