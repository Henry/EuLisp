;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: Misc
;;;  Authors: Andreas Kind
;;;  Description: compute the sum up to 10000
;;;  Compilation: (youtoo sumTo -l boot)
;;;    (above was -l level1 ???)
;;; -----------------------------------------------------------------------
(defmodule sumTo
  (syntax (macros)
   import (boot))
  (defun sum (x res)
    (if (= x 0)
        res
      (sum (- x 1) (+ x res))))
  (print (sum 10000 0))
)  ; end of module
