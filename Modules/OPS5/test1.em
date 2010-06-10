;;;-----------------------------------------------------------------------------
;;;                 OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;; File   : ops5.em
;;; Date   : 20 Jul 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Description: test module
;;;-----------------------------------------------------------------------------
(defmodule test1
  (syntax (macros)
   import (level1 ops5))

  (if (< *argc* 2)
      (format stderr "*** USAGE: ~a <ops5-file>\n" (vector-ref *argv* 0))
    (ops-load (make <symbol> name: (vector-ref *argv* 1))))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
