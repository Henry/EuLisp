;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: misc
;;;  Authors: Andreas Kind
;;; Description: little stress test
;;;  Compilation
;;    youtoo append -l level0
;;;  Interpretation: (!> append)
;;;-----------------------------------------------------------------------------
(defmodule append
  (syntax (macros)
   import (level0)
   export (f))

(defun f (n)
  (if (= n 0)
      '(@)
    (append (f (- n 1)) (f (- n 1)))))

(f 16)

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
