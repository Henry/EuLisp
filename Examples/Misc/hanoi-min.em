(defmodule hanoi-min
  (syntax (syntax-0)
   import (level-0)
   export (hanoi-min))

(defun dohanoi (n to from using)
  (when (> n 0)
        (dohanoi (- n 1) using from to)
        (print "move " from " --> " to nl)
        (dohanoi (- n 1) to using from)))

(defun hanoi-min (n)
  (dohanoi n 3 1 2))

(hanoi-min 3)

;;;-----------------------------------------------------------------------------
)  ;; End of module hanoi-min
;;;-----------------------------------------------------------------------------
