;;;-----------------------------------------------------------------------------
;;;                 OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;; File   : ops5-def.em
;;; Date   : 24 Jul 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Description: Definitions for main ops5 module.
;;;-----------------------------------------------------------------------------
(defmodule ops5-def
  (syntax (syntax-0 macros-tag)
   import (level-0 basic cond-el-gf wm conflict))

(print "### ops5-def" nl)

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

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
