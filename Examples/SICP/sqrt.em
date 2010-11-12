(defmodule sqrt
  (syntax (syntax-0)
   import ((except (sqrt)
                   level-0)))

(defun square (x) (* x x))

(defun sqrt-iter (guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun average (x y)
  (/ (+ x y) 2))

(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))

(defun sqrt (x)
  (sqrt-iter 1.0 x))

(print (sqrt 9) nl)
(print (sqrt (+ 100 37)) nl)
(print (sqrt (+ (sqrt 2) (sqrt 3))) nl)
(print (square (sqrt 1000)) nl)

;;;-----------------------------------------------------------------------------
)  ;; End of module sqrt
;;;-----------------------------------------------------------------------------
