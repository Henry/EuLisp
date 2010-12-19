;;; Copyright 1997 A. Kind & University of Bath
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;
;;  Youtoo is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: Test match
;;;  Library: match
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    See match.em
;;;-----------------------------------------------------------------------------

(defmodule test-match
  (syntax (syntax-0
           match
           test-match-syntax)
   import (level-0
           match-support))

(defun f1 (l)
  (match l
         (() 'nil)
         (1  'one)
         ("hello" 'hello)
         ((1 2) 'const-1-2)
         ((('z c d) a b) (list 'nested-z a b c d))
         (('z . b) (list 'z-prefix b))
         ((a b) (list 'var-a-b a b))
         ((a . b) (list 'var-a.b a b))
         (#(a b) (list 'vect-a-b a b))
         (#(#(c d) a b) (list 'vect-nested a b c d))
         (#(a b c ...) (list 'vect-ellip a b c))
         (_ 'anything-else)))

(defconstant fact
  (match-lambda
   (1 1)
   (n (* n (fact (- n 1))))))

(defconstant match-read
  (match-lambda*
   (()
    '(match-read default-port default-eof-object))
   ((port)
    `(match-read ,port default-eof-object))
   ((port eof-object)
    `(match-read ,port ,eof-object))))

(defun main ()
  (print-test (f1 '()))
  (print-test (f1 1))
  (print-test (f1 '(1)))
  (print-test (f1 '(1 2)))
  (print-test (f1 '(a b)))
  (print-test (f1 '(a b c d e f)))
  (print-test (f1 "hello"))
  (print-test (f1 '(z 2)))
  (print-test (f1 '((z 3 4) 1 2)))
  (print-test (f1 '((z 3 4 5) 1 2)))
  (print-test (f1 #(1 2)))
  (print-test (f1 #(1 2 3 4 5)))
  (print-test (f1 #(#(3 4) 1 2)))
  (print-test (f1  (fact 6)))
  (print-test (f1  (match-read)))
  (print-test (f1  (match-read 'port)))
  (print-test (f1  (match-read 'port 'eof-object)))
  (print-test (f1 (match '(let ((a 1)
                                (b 2)
                                (c 3))
                            (m a)
                            (m b)
                            (m c)
                            (list a b c))
                         (('let ((a b) ...) body ...)
                          (list vars: a  vals: b body: body))
                         (_ 'no-match))))
  (print-test (match '(let3 ((a b c) (d e f)) body1 body2)
                     (('let3 ((a b c) ...) body ...) (list (list a b c) body))
                     (_ 'no-match)))
  )

(main)

;;;-----------------------------------------------------------------------------
)  ;; End of module test-match
;;;-----------------------------------------------------------------------------
