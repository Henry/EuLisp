;; match0.em -- macros for match.em
(defmodule match0
  (syntax (macros)
   import (level1))

(defmacro letrec (inits . body)
  `(let (,@(map (lambda (init) `(,(car init) '())) inits))
     ,@(map (lambda (init) `(setq ,(car init) ,(cadr init))) inits)
     ,@body))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
