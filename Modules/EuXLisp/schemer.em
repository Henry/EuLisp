;;; Copyright 1994 Russell Bradford
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'EuXLisp'
;;;-----------------------------------------------------------------------------
;;
;;  EuXLisp is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  EuXLisp is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: Definitions of a scheme-like language implementation
;;;  Authors: Russell Bradford
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    call/cc may be weird
;;    syntax of various objects may be different (e.g., symbols)
;;;-----------------------------------------------------------------------------

(defmodule schemer
  (import ((rename
            ((setq set!)
             (map-list map)
             (member-list member)
             (reverse-list reverse))
            root)
           macros)
   export (or
           and
           else
           quasiquote
           begin
           if
           quote
           case
           lambda
           set!
           cond
           let
           unquote
           define
           let*
           unquote-splicing
           delay
           letrec

           not
           boolean?
           eqv?
           eq?
           equal?
           pair?
           cons
           car
           cdr
           set-car!
           set-cdr!
           caar
           cadr
           cdar
           cddr
           caaar
           caadr
           cadar
           caddr
           cdaar
           cdadr
           cddar
           cdddr
           caaaar
           caaadr
           caadar
           caaddr
           cadaar
           cadadr
           caddar
           cadddr
           cdaaar
           cdaadr
           cdadar
           cdaddr
           cddaar
           cddadr
           cdddar
           cddddr
           null?
           list?
           list
           length
           append
           reverse
           list-tail
           list-ref
           memq
           memv
           member
           assq
           assv
           assoc
           symbol?
           symbol->string
           string->symbol
           number?
           complex?
           real?
           rational?
           integer?
           exact?
           inexact?
           =
           <
           >
           <=
           >=
           zero?
           positive?
           negative?
           odd?
           even?
           max
           min
           +
           *
           -
           /
           abs
           quotient
           remainder
           modulo
           gcd
           lcm
           ;;   numerator
           ;;   denominator
           floor
           ceiling
           truncate
           round
           ;;   rationalize
           exp
           log
           sin
           cos
           tan
           asin
           acos
           atan
           sqrt
           expt
           ;;   make-rectangular
           ;;   make-polar
           ;;   real-part
           ;;   imag-part
           ;;   magnitude
           ;;   angle
           ;;   exact->inexact
           ;;   inexact->exact
           number->string
           string->number
           char?
           char=?
           char<?
           char>?
           char<=?
           char>=?
           char-ci=?
           char-ci<?
           char-ci>?
           char-ci<=?
           char-ci>=?
           char-alphabetic?
           char-numeric?
           char-whitespace?
           char-upper-case?
           char-lower-case?
           char->integer
           integer->char
           char-upcase
           char-downcase
           string?
           make-string
           string
           string-length
           string-ref
           string-set!
           string=?
           string-ci=?
           string<?
           string>?
           string<=?
           string>=?
           string-ci<?
           string-ci>?
           string-ci<=?
           string-ci>=?
           substring
           string-append
           string->list
           list->string
           ;;   string-copy
           ;;   string-fill!
           vector?
           make-vector
           vector
           vector-length
           vector-ref
           vector-set!
           vector->list
           list->vector
           ;;   vector-fill!
           procedure?
           apply
           map
           for-each
           force
           call-with-current-continuation
           call/cc
           call-with-input-file
           call-with-output-file
           input-stream?
           output-stream?
           current-input-stream
           current-output-stream
           ;;   with-input-from-file
           ;;   with-output-to-file
           open-input-file
           open-output-file
           close-input-stream
           close-output-stream
           read
           read-char
           peek-char
           eof-object?
           char-ready?
           write
           display
           newline
           write-char
           load
           transcript-on
           transcript-off))

(defmacro do (vars test . body)
  `(let loop
     ,(map (lambda (v)
             (list (car v) (cadr v)))
           vars)
     (if ,(car test)
         (begin
          ,@(cdr test))
       (begin
        ,@body
        (loop ,@(map (lambda (v)
                       (if (null? (cddr v))
                           (car v)
                         (caddr v)))
                     vars))))))


(defmacro case (expr . clauses)
  `(let ((sym ,expr))
     (cond ,@(map (lambda (x)
                    (cond ((eq? (car x) 'else)
                           x)
                          ((atom? (car x))
                           `((eqv? sym ',(car x)) ,@(cdr x)))
                          (else
                           `((memv sym ',(car x)) ,@(cdr x)))))
                  clauses))))

;; modulo
(define (modulo a b)
        (let ((ans (remainder a b)))
          (if (>= b 0)
              (if (< ans 0)
                  (+ ans b)
                ans)
            (if (< ans 0)
                ans
              (+ ans b)))))

;; lcm
(define (lcm arg . rest)
        (cond ((null? rest) (abs arg))
              ((null? (cdr rest))
               (abs (/ (* arg (car rest)) (gcd arg (car rest)))))
              (else (lcm arg (apply lcm rest)))))

;; char-ci<=?
(define (char-ci<=? a b)
        (or (char-ci<? a b)
            (char-ci=? a b)))

;; char-ci>=?
(define (char-ci>=? a b)
        (or (char-ci>? a b)
            (char-ci=? a b)))

;; char-alphabetic?
(define (char-alphabetic? a)
        (and (char-ci>=? a #\a)
             (char-ci<=? a #\z)))

;; char-numeric?
(define (char-numeric? a)
        (and (char>=? a #\0)
             (char<=? a #\9)))

;; char-whitespace?
(define (char-whitespace? a)
        (not (null? (memv a '(#\space #\tab #\newline #\formfeed #\return)))))

;; char-upper-case?
(define (char-upper-case? a)
        (and (char>=? a #\A)
             (char<=? a #\Z)))

;; char-lower-case?
(define (char-lower-case? a)
        (and (char>=? a #\a)
             (char<=? a #\z)))

(define upper-lower (- (char->integer #\a) (char->integer #\A)))

;; char-upcase
(define (char-upcase a)
        (if (char-lower-case? a)
            (integer->char (- (char->integer a) upper-lower))
          a))

;; char-downcase
(define (char-downcase a)
        (if (char-upper-case? a)
            (integer->char (+ (char->integer a) upper-lower))
          a))

;; string
(define (string . args)
        (list->string args))

;;;-----------------------------------------------------------------------------
)  ;; End of module schemer
;;;-----------------------------------------------------------------------------
