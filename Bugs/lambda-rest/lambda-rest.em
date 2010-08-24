(defmodule lambda-rest
  (syntax (macros)
   import (level1))
(defun f args1 (cons 'args1 args1))

(defun g (a . rest) (cons 'a-rest (list a rest)))
;; This works.
(format t "~s~%" (f 1 2 3 4 5))
;; This works.
(format t "~s~%" (g 1 2 3 4 5))
;; This doesn't.
(format t "~s~%" ((lambda args2 (cons 'args2 args2)) 1 2 3 4))
;; This doesn't.
(format t "~s~%" ((named-lambda name args3 (cons 'args3 args3)) 1 2 3 4))
;; This works.
(format t "~s~%" ((let ((f (lambda args4 (cons 'args4 args4)))) f) 1 2 3 4))
;; This doesn't.
(format t "~s~%" ((lambda args2.5 (cons 'args2.5 args2.5)) 1 2 3 4))
;; This works.
(format t "~s~%" ((lambda (f) (f 1 2 3 4)) (lambda args5 args5)))
;; This works.
(format t "~s~%" (((lambda () (lambda args6 args6))) 1 2 3 4))
;; This doesn't compile.
;;(format t "~s~%" ((lambda (bomb) (cons 'bomb bomb)) 1 2 3))
)
