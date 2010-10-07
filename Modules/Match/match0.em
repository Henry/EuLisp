;; match0.em -- syntax-0 for match.em
(defmodule match0
  (syntax (syntax-0)
   import (level-0))

(defmacro letrec (inits . body)
  `(let (,@(map (lambda (init) `(,(car init) '())) inits))
     ,@(map (lambda (init) `(setq ,(car init) ,(cadr init))) inits)
     ,@body))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
