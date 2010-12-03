;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: test
;;; Description: Simple test for `eval'
;;;-----------------------------------------------------------------------------

(defmodule eval-test
  (syntax (macros)
   import (level-1
           (only (init-eval eval)
                 eval)
           eval-user
           eval-user2)
   export (end))

(print "Starting eval" nl)
(init-eval 'eval-test)

(deflocal end "not end")

(eval (read lispin () ()))
(eval (read lispin () ()))
(eval (read lispin () ()))

(print end nl)

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
