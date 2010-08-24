(defmodule vec
  (syntax (macros)
   import (level0))

(deflocal *size* 1000000)
(deflocal *vec1* ())
(deflocal *vec2* ())

(defun allocate-vectors ()
  (setq *vec1* (make <vector> size: *size*))
  (setq *vec2* (make <vector> size: *size*))
  (fill-vector *vec1*))

(defun fill-vector (vec)
  (let ((n (size vec)))
    (labels
     ((loop (i)
            (if (< n i)
                ()
              (progn
                ((setter vector-ref) vec (- i 1) i)
                (loop (+ i 1))))))
     (loop 1))))

(defun copy (i)
  (if (= i *size*)
      ()
    (let ((j (vector-ref *vec1* i)))
      ((setter vector-ref) *vec2* i j)
      (copy j))))

(defun run ()
  (copy 0)
  (copy 0)
  (copy 0)
  (copy 0)
  (copy 0)
  (copy 0)
  (copy 0)
  (copy 0)
  (copy 0)
  (copy 0))

(allocate-vectors)
(time-execution (run) stdout)

)  ; end of module
