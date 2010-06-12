;;; arith.em
;;; Euscheme code Copyright (c) 1994 Russell Bradford

;;; many of these inlined for special cases

(defmodule arith

    (import (root)
     export ( binary+ binary- unary- binary* binary/ unary/ binary%
              binary-gcd gcd abs + - * / % pow zero? quotient remainder))

  (deflocal %+ +)

  (define-generic (binary+ a b))

  (define-method (binary+ (a <number>) (b <number>))
                 (%+ a b))

  (define (+ . args)
          (cond ((null? args) 0)
                ((null? (cdr args)) (car args))
                ((null? (cddr args)) (binary+ (car args) (cadr args)))
                (t (apply + (binary+ (car args) (cadr args)) (cddr args)))))

  (deflocal %- -)

  (define-generic (binary- a b))

  (define-method (binary- (a <number>) (b <number>))
                 (%- a b))

  (define (- arg . args)
          (cond ((null? args) (unary- arg))
                ((null? (cdr args)) (binary- arg (car args)))
                (t (apply - (binary- arg (car args)) (cdr args)))))

  (define-generic (unary- a))

  (define-method (unary- (a <number>)) (%- a))

  (deflocal %* *)

  (define-generic (binary* a b))

  (define-method (binary* (a <number>) (b <number>))
                 (%* a b))

  (define (* . args)
          (cond ((null? args) 1)
                ((null? (cdr args)) (car args))
                ((null? (cddr args)) (binary* (car args) (cadr args)))
                (t (apply * (binary* (car args) (cadr args)) (cddr args)))))

  (deflocal %/ /)

  (define-generic (binary/ a b))

  (define-method (binary/ (a <number>) (b <number>))
                 (%/ a b))

  (define (/ arg . args)
          (cond ((null? args) (unary/ arg))
                ((null? (cdr args)) (binary/ arg (car args)))
                (t (apply / (binary/ arg (car args)) (cdr args)))))

  (define-generic (unary/ a))

  (define-method (unary/ (a <number>)) (%/ a))

  (define-generic (binary% a b))

  (define-method (binary% (a <fpi>) (b <fpi>))
                 (remainder a b))

  (define (% arg . args)
          (cond ((null? args) arg)
                ((null? (cdr args)) (binary% arg (car args)))
                (t (apply % (binary% arg (car args)) (cdr args)))))

  (deflocal %gcd gcd)

  (define (gcd n . args)
          (cond ((null? args) (abs n))
                ((null? (cdr args)) (binary-gcd n (car args)))
                (t (binary-gcd n (apply gcd args)))))

  (define-generic (binary-gcd a b))

  ;; generally very inefficient
  ;; requires methods on zero? and remainder
  (define-method (binary-gcd (a <object>) (b <object>))
                 (if (zero? b)
                     a
                   (binary-gcd b (remainder a b))))

  (define-method (binary-gcd (a <fpi>) (b <fpi>))
                 (%gcd a b))

  (deflocal %abs abs)

  (define-generic (abs a))

  (define-method (abs (a <number>))
                 (%abs a))

  (define-generic (pow (a <number>) (b <number>)))

  (define-method (pow (a <number>) (b <number>))
                 (expt a b))

  (define-method (pow (a <integer>) (b <integer>))
                 (cond ((>= b 0)
                        (cond ((= b 0) 1)
                              ((= b 1) a)
                              (t (int-pow a b 1))))
                       ((= a 1) 1)
                       ((= a -1) (if (even? b) 1 -1))
                       (t (expt a b))))

  (define (int-pow a b sofar)
          (cond ((> b 1)
                 (int-pow (* a a) (quotient b 2) (if (odd? b) (* a sofar) sofar)))
                ((= b 1) (* a sofar))
                (t sofar)))

  (deflocal %zero? zero?)

  (define-generic (zero? a))

  (define-method (zero? (a <number>))
                 (%zero? a))

  (deflocal %quotient quotient)

  (define-generic (quotient a b))

  (define-method (quotient (a <integer>) (b <integer>))
                 (%quotient a b))

  (deflocal %remainder remainder)

  (define-generic (remainder a b))

  (define-method (remainder (a <integer>) (b <integer>))
                 (%remainder a b))

  )
