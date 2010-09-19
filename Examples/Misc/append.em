;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: misc
;;;  Authors: Andreas Kind
;;; Description: little stress test
;;;  Compilation
;;    youtoo append -l level-0
;;;  Interpretation: (!> append)
;;;-----------------------------------------------------------------------------
(defmodule append
  (syntax (macros)
   import (level-0)
   export (f))

(defun f (n)
  (if (= n 0)
      '(@)
    (append (f (- n 1)) (f (- n 1)))))

(f 16)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
