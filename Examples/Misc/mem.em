(defmodule mem
  (syntax (macros)
   import (level-0))

(defconstant *lifetime* 100)
(defconstant *blocksize* 1000)

(deflocal *vec* (make <vector> size: *lifetime*))

(defun foo (i j)
  (if (< i *lifetime*)
      (progn
        ((setter vector-ref) *vec* i (make <vector> size: *blocksize*))
        (foo (+ i 1) j))
    (if (< 0 j)
        (foo 0 (- j 1))
      ())))

(defun run ()
  (foo 0 1000))

(time-execution (run) stdout)

;;;-----------------------------------------------------------------------------
)  ;; End of module mem
;;;-----------------------------------------------------------------------------
