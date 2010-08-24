(defmodule test1
  (syntax (macros)
   import (level1 serial))

(defun foo (x) x)

(defun foo1 (x) (print x))
(defun foo2 (x) (print x) (print x))

(defun foo3 (x) (print x) (prin x))
(format "$$$ foo ~a\n" (eul_lambda_refs foo))
(format "$$$ foo1 ~a\n" (eul_lambda_refs foo1))
(format "$$$ foo2 ~a\n" (eul_lambda_refs foo2))
(format "$$$ foo3 ~a\n" (eul_lambda_refs foo3))
(format "$$$ + ~a\n" (eul_lambda_refs +))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
