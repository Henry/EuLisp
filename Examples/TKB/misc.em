;; Just some odds and ends, not intended to be compilable.
(defmodule misc
  (syntax (macros)
   import (level1))

;; Given the arity of a lambda with rest args and the actual arguments
;; return a list with the fixed arguments and the rest arguments separated.
(defun fixed-and-rest (n l)
  (let ((n (- n 1)))                  ;last of arity is rest arg.
    (let loop ((i 0) (fixed ()) (l l))
         (cond
           ((>= i n)
            (list (reverse fixed) l))
           (t
            (loop (+ i 1) (cons (car l) fixed) (cdr l)))))))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
