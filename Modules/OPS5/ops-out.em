;;;-----------------------------------------------------------------------------
;;;                 OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;; File   : ops-out.em
;;; Date   : 17 Aug 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Description: Output file.
;;;-----------------------------------------------------------------------------
(defmodule ops-out
  (syntax (macros macros-tag)
   import (level1 basic))

;;  (deflocal ops-out (make <output-port> filename: "gcd.out"))
(deflocal ops-out stdout)
(export ops-out)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
