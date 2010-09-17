;;;-----------------------------------------------------------------------------
;;;                 OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;; File   : action-gf.em
;;; Date   :  8 Aug 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Description:
;;;-----------------------------------------------------------------------------
(defmodule action-gf
  (syntax (macros macros-tag)
   import (level1 basic))

(defclass <action> ()
  ())

(defclass <halt-action> <action>
  ()
  constructor: (make-halt-action))

(defgeneric execute ((action <action>) pi
                     wm-manager ce-manager cr-manager))

(defgeneric get-ts (ce-num prod ce-ts))

(export execute <action> <halt-action> make-halt-action get-ts)

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
