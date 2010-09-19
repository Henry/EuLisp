;;;-----------------------------------------------------------------------------
;;;                 OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;; File   : prod-gf.em
;;; Date   : 24 Jul 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Description: Generic functions for production
;;;-----------------------------------------------------------------------------
(defmodule prod-gf
  (syntax (macros macros-tag)
   import (level1 basic))
(print "### prod-gf")

(defgeneric p-name (object))
(defgeneric create-prod-insts (prod ts ce join-tests cr-manager))
(defgeneric remove-prod-insts (prod ts ce cr-manager))
(defgeneric add-cond-el (prod ce))
(defgeneric fire (prod wm-manager ce-manager cr-manager))

(export p-name add-cond-el fire
 create-prod-insts remove-prod-insts)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
