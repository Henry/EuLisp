;;;-----------------------------------------------------------------------------
;;;                 OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;; File   : action-gf.em
;;; Date   :  8 Aug 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Description:
;;;-----------------------------------------------------------------------------
(defmodule action-gf
  (syntax (syntax-0 macros-tag)
   import (level-0 basic))

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
)  ;; End of module
;;;-----------------------------------------------------------------------------
