;;;-----------------------------------------------------------------------------
;;;                 OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;; File   : ops5.em
;;; Date   : 20 Jul 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Description: test module
;;;-----------------------------------------------------------------------------
(defmodule test1
  (syntax (syntax-0)
   import (level-0 ops5))

(if (< *argc* 2)
    (sformat stderr "*** USAGE: ~a <ops5-file>\n" (vector-ref *argv* 0))
  (ops-load (make <symbol> name: (vector-ref *argv* 1))))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
