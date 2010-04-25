;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: test
;;;  Authors: Andreas Kind
;;; Description: faculty function in EuLisp
;;;-----------------------------------------------------------------------------
(defmodule fact
  (syntax (macros)
   import (level1)
   export (fact))

  (defun fact (x)
    (labels
     ((loop (xx res)
            (if (< xx 2)
                res
              (loop (- xx 1) (* res xx)))))
     (loop x 1)))

  (print (fact 10))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
