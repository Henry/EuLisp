;;; EuLysses header
(defmodule test1
  (syntax (macros)
   import (level1 serial))
  (defun foo (x) x)
  (defun foo1 (x) (print x))
  (defun foo2 (x) (print x) (print x))
  (defun foo3 (x) (print x) (prin x))
  (format t "$$$ foo ~a\n" (eul_lambda_refs foo))
  (format t "$$$ foo1 ~a\n" (eul_lambda_refs foo1))
  (format t "$$$ foo2 ~a\n" (eul_lambda_refs foo2))
  (format t "$$$ foo3 ~a\n" (eul_lambda_refs foo3))
  (format t "$$$ + ~a\n" (eul_lambda_refs +))
)