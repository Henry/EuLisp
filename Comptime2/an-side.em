;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: side effect analyses
;;;-----------------------------------------------------------------------------
(defmodule an-side
  (syntax (_macros)
   import (i-all sx-obj)
   export (compute-captured-vars))

;;;-----------------------------------------------------------------------------
;;; Set a flag when a local-static-var is once captured (free in a lambda)
;;;-----------------------------------------------------------------------------
  (defgeneric compute-captured-vars (node))

  (defmethod compute-captured-vars ((node <module>))
    (do1-list compute-captured-vars (module-named-lambdas? node))
    (do1-list compute-captured-vars (module-anonymous-lambdas? node)))

  (defmethod compute-captured-vars ((node <lambda>))
    (let* ((args (append (fun-args? node) (lambda-delegated-vars? node)))
           (bindings (lambda-binding-refs? node))
           (objs (map1-list binding-obj? bindings))
           (vars (select-list local-static-var? objs)))
      (do1-list (lambda (var)
            (and (null (member1-list var args))
                 (progn
                   (notify0 "var ~a captured" (var-name? var))
                   (local-static-var-captured! var t))))
          vars)))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
