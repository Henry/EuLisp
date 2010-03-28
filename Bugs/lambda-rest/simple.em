(defmodule simple
  (syntax (macros)
   import (level1))
  (defconstant f (lambda fargs fargs))
  (print (f 61 62 63 64))
  (print (list 31 32 33 34))
  (print (let ((a (list 51 52 53 54))) a)) ; for comparison
  (print ((named-lambda e1 args (list 'e1 args)) 11 12 13 14)) ; error
  (print ((lambda args (list 'e1.5 args)) 11 12 13 14)) ; error
  (print ((named-lambda e2 (first . rest)
            (list 'e2-first.rest first rest)) 41 42 43 44)) ; error 
  ;; How does passing the list look?
  (print ((named-lambda e3 args (list 'e3 args)) 71 72 (list 73 74)))
  (print ((named-lambda c1 (n) (list 'n n)) 1)) ; for comparison
  (print (((named-lambda w1 ()
             (named-lambda o1 args (list 'o1 args))))
          21 22 23 24)) ; ok
) ; end of simple.em
