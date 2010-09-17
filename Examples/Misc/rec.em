;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: misc
;;;  Authors: Andreas Kind
;;;  Description: recursion test
;;;  Compilation: ../youtoo rec -l level-0
;;;  Interpretation: (!> rec)
;;; -----------------------------------------------------------------------

(defmodule rec
  (syntax (macros)
   import (level-0)
   export (run))

(defun test (f g n)
  (if (= n 0)
      f
    (let ((m (- n 1)))
      ((f g f m) f g m)
      ((g f g m) g f m)
      g)))

(defun run ()
  (test test test 12))

(time-execution (run) stdout)

)  ; end of module
