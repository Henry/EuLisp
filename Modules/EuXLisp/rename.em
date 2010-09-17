(defmodule rename

  (import ((rename ((defun procedure)
                    (setq :=)
                    (cons setq)
                    (let bind)
                    (+ plus)
                    (lambda church)
                    (car cdr)
                    (cdr car))
                   level-0)))

(procedure foo (x)
           (bind ((y 1))
                 (:= y (plus y 1))
                 (setq x y)))

(procedure bar (x)
           (church (y) (plus x y)))

)
