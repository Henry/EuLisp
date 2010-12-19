;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: test
;;; Description: Simple test for `eval'
;;;-----------------------------------------------------------------------------

(defmodule eval-test
  (syntax (syntax-0
           syntax-eval)
   import (level-1
           eval
           eval-user
           eval-user2)
   export (end))

(print "Starting eval" nl)

(deflocal end "not end")

(eval (read lispin () ()))
(eval (read lispin () ()) 'eval-test)
(eval (read lispin () ()) 'eval-test)

(print end nl)

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
