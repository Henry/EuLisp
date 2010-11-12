;;; CODE FROM CHAPTER 1
;;; OF
;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS
;;; Converted from Scheme to EuLisp

;;; Examples from the book are commented out with ;: so that they
;;;  are easy to find and so that they will be omitted if you evaluate a
;;;  chunk of the file (programs with intervening examples) in EuLisp.

;;; BEWARE: Although the whole file can be loaded into EuLisp,
;;;  don't expect the programs to work if you do so.  For example,
;;;  the redefinition of + in exercise 1.9 wreaks havoc with the
;;;  last version of square defined here.


;;; SECTION 1.1.1

;;;  interpreter examples

;: 486

;: (+ 137 349)
;: (- 1000 334)
;: (* 5 99)
;: (/ 10 5)
;: (+ 2.7 10)

;: (+ 21 35 12 7)
;: (* 25 4 12)

;: (+ (* 3 5) (- 10 6))

;: (+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))

;: (+ (* 3
;:       (+ (* 2 4)
;:          (+ 3 5)))
;:    (+ (- 10 7)
;:       6))


;;; SECTION 1.1.2

;: (deflocal size 2)
;: size
;: (* 5 size)

;: (deflocal pi 3.14159)
;: (deflocal radius 10)
;: (* pi (* radius radius))
;: (deflocal circumference (* 2 pi radius))
;: circumference


;;; SECTION 1.1.3

;: (* (+ 2 (* 4 6))
;:    (+ 3 5 7))


;;; SECTION 1.1.4

(defun square (x) (* x x))

;: (square 21)
;: (square (+ 2 5))
;: (square (square 3))

(defun sum-of-squares (x y)
  (+ (square x) (square y)))

;: (sum-of-squares 3 4)

(defun f (a)
  (sum-of-squares (+ a 1) (* a 2)))

;: (f 5)


;;; SECTION 1.1.5

;: (f 5)
;: (sum-of-squares (+ 5 1) (* 5 2))
;: (+ (square 6) (square 10))
;: (+ (* 6 6) (* 10 10))
;: (+ 36 100)

;: (f 5)
;: (sum-of-squares (+ 5 1) (* 5 2))
;: (+    (square (+ 5 1))      (square (* 5 2))  )
;: (+    (* (+ 5 1) (+ 5 1))   (* (* 5 2) (* 5 2)))
;: (+         (* 6 6)             (* 10 10))
;: (+           36                   100)
;:                     136


;;; SECTION 1.1.6

(defun abs (x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(defun abs (x)
  (cond ((< x 0) (- x))
        (else x)))

(defun abs (x)
  (if (< x 0)
      (- x)
      x))

;: (and (> x 5) (< x 10))

(defun >= (x y)
  (or (> x y) (= x y)))

(defun >= (x y)
  (not (< x y)))


;;; EXERCISE 1.1
;: 10

;: (+ 5 3 4)

;: (- 9 1)

;: (/ 6 2)

;: (+ (* 2 4) (- 4 6))

;: (deflocal a 3)

;: (deflocal b (+ a 1))

;: (+ a b (* a b))

;: (= a b)

;: (if (and (> b a) (< b (* a b)))
;:     b
;:     a)

;: (cond ((= a 4) 6)
;:       ((= b 4) (+ 6 7 a))
;:       (else 25))

;: (+ 2 (if (> b a) b a))

;: (* (cond ((> a b) a)
;:       ((< a b) b)
;:       (else -1))
;:    (+ a 1))

;;; EXERCISE 1.4
(defun a-plus-abs-b (a b)
  ((if (> b 0) + -) a b))

;;; EXERCISE 1.5
(defun p ()) ((p))

(defun test (x y)
  (if (= x 0)
      0
      y))

;: (test 0 (p))


;;; SECTION 1.1.7

(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))

(defun average (x y)
  (/ (+ x y) 2))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun sqrt-iter (guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(defun sqrt (x)
  (sqrt-iter 1.0 x))


;: (sqrt 9)
;: (sqrt (+ 100 37))
;: (sqrt (+ (sqrt 2) (sqrt 3)))
;: (square (sqrt 1000))


;;; EXERCISE 1.6
(defun new-if (predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;: (new-if (= 2 3) 0 5)

;: (new-if (= 1 1) 0 5)

(defun sqrt-iter (guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))


;;; SECTION 1.1.8

(defun square (x) (* x x))

(defun double (x) (+ x x))

(defun square (x)
  (exp (double (log x))))


;;;  As in 1.1.7
(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun sqrt-iter (guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(defun sqrt (x)
  (sqrt-iter 1.0 x))


;;;   Block-structured
(defun sqrt (x)
  (letfuns
      ((good-enough? (guess x)
                     (< (abs (- (square guess) x)) 0.001))
       (improve (guess x)
                (average guess (/ x guess)))
       (sqrt-iter (guess x)
                  (if (good-enough? guess x)
                      guess
                    (sqrt-iter (improve guess x) x)))))
  (sqrt-iter 1.0 x))

;;;   Taking advantage of lexical scoping
(defun sqrt (x)
  (letfuns
      ((good-enough? (guess)
                     (< (abs (- (square guess) x)) 0.001))
       (improve (guess)
                (average guess (/ x guess)))
       (sqrt-iter (guess)
                  (if (good-enough? guess)
                      guess
                    (sqrt-iter (improve guess)))))
    (sqrt-iter 1.0)))

;;; SECTION 1.2.1

;;;   Recursive
(defun factorial (n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))


;;;   Iterative
(defun fact-iter (product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(defun factorial (n)
  (fact-iter 1 1 n))

;;;   Iterative, block-structured (from footnote)
(defun factorial (n)
  (letfuns
      ((iter (product counter)
             (if (> counter n)
                 product
               (iter (* counter product)
                     (+ counter 1)))))
    (iter 1 1)))


;;; EXERCISE 1.9
(defun + (a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(defun + (a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

;;; EXERCISE 1.10
(defun A (x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;: (A 1 10)

;: (A 2 4)

;: (A 3 3)

(defun f (n) (A 0 n))

(defun g (n) (A 1 n))

(defun h (n) (A 2 n))

(defun k (n) (* 5 n n))


;;; SECTION 1.2.2

;;;   Recursive
(defun fib (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;;;   Iterative
(defun fib-iter (a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(defun fib (n)
  (fib-iter 1 0 n))


;;;   Counting change
(defun first-denomination (kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(defun cc (amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(defun count-change (amount)
  (cc amount 5))

;: (count-change 100)


;;; SECTION 1.2.3

;;; EXERCISE 1.15
(defun cube (x) (* x x x))

(defun p (x) (- (* 3 x) (* 4 (cube x))))

(defun sine (angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))


;;; SECTION 1.2.4

;;;   Linear recursion
(defun expt (b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))


;;;   Linear iteration
(defun expt-iter (b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                (- counter 1)
                (* b product))))

(defun expt (b n)
  (expt-iter b n 1))

;;;   Logarithmic iteration
(defun even? (n)
  (= (% n 2) 0))

(defun fast-expt (b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


;;; EXERCISE 1.17
(defun * (a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

;;; EXERCISE 1.19
(defun fib-iter (a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   ??FILL-THIS-IN?? ; compute p'
                   ??FILL-THIS-IN?? ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(defun fib (n)
  (fib-iter 1 0 0 1 n))

;;; SECTION 1.2.5

(defun gcd (a b)
  (if (= b 0)
      a
      (gcd b (% a b))))


;;; SECTION 1.2.6

;;;   prime?

(defun divides? (a b)
  (= (% b a) 0))

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun prime? (n)
  (= n (smallest-divisor n)))


;;;   fast-prime?

(defun expmod (base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (% (square (expmod base (/ exp 2) m))
                    m))
        (else
         (% (* base (expmod base (- exp 1) m))
                    m))))

(defun fermat-test (n)
  (letfuns
      ((try-it (a)
               (= (expmod a n n) a)))
    (try-it (+ 1 (random (- n 1))))))

(defun fast-prime? (n times)
  (cond ((= times 0) t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else ())))


;;; EXERCISE 1.22
(defun report-prime (elapsed-time)
  (print " *** " elapsed-time nl))

(defun start-prime-test (n start-time)
  (if (prime? n)
      (report-prime (- (element (cpu-time) 0) start-time))
    (print nl)))

(defun timed-prime-test (n)
  (print nl n)
  (start-prime-test n (element (cpu-time) 0)))

;;; EXERCISE 1.25
(defun expmod (base exp m)
  (% (fast-expt base exp) m))

;;; EXERCISE 1.26
(defun expmod (base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (% (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (% (* base (expmod base (- exp 1) m))
                    m))))

;;; SECTION 1.3

(defun cube (x) (* x x x))

;;; SECTION 1.3.1

(defun sum-integers (a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(defun sum-cubes (a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(defun pi-sum (a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(defun sum (term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


;;;   Using sum

(defun inc (n) (+ n 1))

(defun sum-cubes (a b)
  (sum cube a inc b))

;: (sum-cubes 1 10)


(defun identity (x) x)

(defun sum-integers (a b)
  (sum identity a inc b))

;: (sum-integers 1 10)


(defun pi-sum (a b)
  (letfuns
      ((pi-term (x)
                (/ 1.0 (* x (+ x 2))))
       (pi-next (x)
                (+ x 4)))
    (sum pi-term a pi-next b)))

;: (* 8 (pi-sum 1 1000))


(defun integral (f a b dx)
  (letfuns
      ((add-dx (x) (+ x dx)))
    (* (sum f (+ a (/ dx 2)) add-dx b)
       dx)))

;: (integral cube 0 1 0.01)

;: (integral cube 0 1 0.001)


;;; EXERCISE 1.32
;: (accumulate combiner null-value term a next b)

;;; SECTION 1.3.2

(defun pi-sum (a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(defun integral (f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(defun plus4 (x) (+ x 4))

(deflocal plus4 (lambda (x) (+ x 4)))

;: ((lambda (x y z) (+ x y (square z))) 1 2 3)


;;;   Using let

(defun f (x y)
  (letfuns
      ((f-helper (a b)
                 (+ (* x (square a))
                    (* y b)
                    (* a b))))
    (f-helper (+ 1 (* x y))
              (- 1 y))))

(defun f (x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(defun f (x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;: (+ (let ((x 3))
;:      (+ x (* x 10)))
;:    x)

;: (let ((x 3)
;:       (y (+ x 2)))
;:   (* x y))


;;; EXERCISE 1.34
(defun f (g)
  (g 2))

;: (f square)

;: (f (lambda (z) (* z (+ z 1))))


;;; SECTION 1.3.3

;;;   Half-interval method

(defun close-enough? (x y)
  (< (abs (- x y)) 0.001))

(defun search (f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(defun half-interval-method (f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error <condition>
                  (fmt "Values are not of opposite sign ~a ~a" a b))))))


;: (half-interval-method sin 2.0 4.0)

;: (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
;:                       1.0
;:                       2.0)


;;;   Fixed points

(deflocal tolerance 0.00001)

(defun fixed-point (f first-guess)
  (letfuns
      ((close-enough? (v1 v2)
                      (< (abs (- v1 v2)) tolerance))
       (try (guess)
            (let ((next (f guess)))
              (if (close-enough? guess next)
                  next
                (try next)))))
    (try first-guess)))


;: (fixed-point cos 1.0)

;: (fixed-point (lambda (y) (+ (sin y) (cos y)))
;:              1.0)


(defun sqrt (x)
  (fixed-point (lambda (y) (/ x y))
               1.0))

(defun sqrt (x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))


;;; EXERCISE 1.37
;: (cont-frac (lambda (i) 1.0)
;:            (lambda (i) 1.0)
;:            k)


;;; SECTION 1.3.4

(defun average-damp (f)
  (lambda (x) (average x (f x))))

;: ((average-damp square) 10)

(defun sqrt (x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(defun cube-root (x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))


;;;   Newton's method
(deflocal dx 0.00001)

(defun deriv (g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(defun cube (x) (* x x x))

;: ((deriv cube) 5)

(defun newton-transform (g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(defun newtons-method (g guess)
  (fixed-point (newton-transform g) guess))


(defun sqrt (x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))


;;;   Fixed point of transformed function

(defun fixed-point-of-transform (g transform guess)
  (fixed-point (transform g) guess))

(defun sqrt (x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(defun sqrt (x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))


;;; EXERCISE 1.40
;: (newtons-method (cubic a b c) 1)


;;; EXERCISE 1.41
;: (((double (double double)) inc) 5)


;;; EXERCISE 1.42
;: ((compose square inc) 6)


;;; EXERCISE 1.43
;: ((repeated square 2) 5)
