;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; Module: benchmark 'nfib'
;;;-----------------------------------------------------------------------------
;;;  Authors: Andreas Kind
;;;  Description: counting fibonacci function calls
;;;  Compilation
;;  youtoo nfib -l level-0 -l math
;;;  Interpretation: (!> nfib)
;;;-----------------------------------------------------------------------------
(defmodule nfib
  (syntax (macros)
   import (level-0 math)
   export (nfib))

(defun nfib (n)
  (if (< 1 n)
      (+ (nfib (- n 1))
         (nfib (- n 2))
         1)
    1))

(time-execution (nfib 35) stdout)

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
