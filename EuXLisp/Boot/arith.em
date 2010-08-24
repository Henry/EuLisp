;;; arith.em
;;; Euscheme code Copyright (c) 1994 Russell Bradford

;;; many of these inlined for special cases

(defmodule arith
  (import (root macros0)
   export (binary+ binary- binary* binary/ binary% binary-mod
                   binary-gcd gcd abs + - * / % mod pow zero? quotient remainder))

(deflocal %+ +)

(defgeneric binary+ (a b))

(defmethod binary+ ((a <number>) (b <number>))
  (%+ a b))

(defun + args
  (cond ((null? args) 0)
        ((null? (cdr args)) (car args))
        ((null? (cddr args)) (binary+ (car args) (cadr args)))
        (t (apply + (binary+ (car args) (cadr args)) (cddr args)))))

(deflocal %- -)

(defgeneric binary- (a b))

(defmethod binary- ((a <number>) (b <number>))
  (%- a b))

(defun - ( arg . args)
  (cond ((null? args) (unary- arg))
        ((null? (cdr args)) (binary- arg (car args)))
        (t (apply - (binary- arg (car args)) (cdr args)))))

(defgeneric unary- (a))

(defmethod unary- ((a <number>)) (%- a))

(deflocal %* *)

(defgeneric binary* (a b))

(defmethod binary* ((a <number>) (b <number>))
  (%* a b))

(defun * args
  (cond ((null? args) 1)
        ((null? (cdr args)) (car args))
        ((null? (cddr args)) (binary* (car args) (cadr args)))
        (t (apply * (binary* (car args) (cadr args)) (cddr args)))))

(deflocal %/ /)

(defgeneric binary/ (a b))

(defmethod binary/ ((a <number>) (b <number>))
  (%/ a b))

(defun / (arg . args)
  (cond ((null? args) (unary/ arg))
        ((null? (cdr args)) (binary/ arg (car args)))
        (t (apply / (binary/ arg (car args)) (cdr args)))))

(defgeneric unary/ (a))

(defmethod unary/ ((a <number>)) (%/ a))

(defgeneric binary% (a b))

(defmethod binary% ((a <int>) (b <int>))
  (remainder a b))

(defun % (arg . args)
  (cond ((null? args) arg)
        ((null? (cdr args)) (binary% arg (car args)))
        (t (apply % (binary% arg (car args)) (cdr args)))))

(defgeneric binary-mod (a b))

(defmethod binary-mod ((a <int>) (b <int>))
  (remainder a b))

(defun mod (arg . args)
  (cond ((null? args) arg)
        ((null? (cdr args)) (binary-mod arg (car args)))
        (t (apply mod (binary-mod arg (car args)) (cdr args)))))

(deflocal %gcd gcd)

(defun gcd ( n . args)
  (cond ((null? args) (abs n))
        ((null? (cdr args)) (binary-gcd n (car args)))
        (t (binary-gcd n (apply gcd args)))))

(defgeneric binary-gcd (a b))

;; generally very inefficient
;; requires methods on zero? and remainder
(defmethod binary-gcd ((a <object>) (b <object>))
  (if (zero? b)
      a
    (binary-gcd b (remainder a b))))

(defmethod binary-gcd ((a <int>) (b <int>))
  (%gcd a b))

(deflocal %abs abs)

(defgeneric abs (a))

(defmethod abs ((a <number>))
  (%abs a))

(defgeneric pow ((a <number>) (b <number>)))

(defmethod pow ((a <number>) (b <number>))
  (expt a b))

(defmethod pow ((a <integer>) (b <integer>))
  (cond ((>= b 0)
         (cond ((= b 0) 1)
               ((= b 1) a)
               (t (int-pow a b 1))))
        ((= a 1) 1)
        ((= a -1) (if (even? b) 1 -1))
        (t (expt a b))))

(defun int-pow (a b sofar)
  (cond ((> b 1)
         (int-pow (* a a) (quotient b 2) (if (odd? b) (* a sofar) sofar)))
        ((= b 1) (* a sofar))
        (t sofar)))

(deflocal %zero? zero?)

(defgeneric zero? (a))

(defmethod zero? ((a <number>))
  (%zero? a))

(deflocal %quotient quotient)

(defgeneric quotient (a b))

(defmethod quotient ((a <integer>) (b <integer>))
  (%quotient a b))

(deflocal %remainder remainder)

(defgeneric remainder (a b))

(defmethod remainder ((a <integer>) (b <integer>))
  (%remainder a b))

)
