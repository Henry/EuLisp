;;; Copyright 2010 Henry G. Weller and Stefan Israelsson Tampe
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
;;; Title: Test smatch
;;;  Library: smatch
;;;  Authors: Andreas Kind, Henry G. Weller and Stefan Israelsson Tampe
;;;  Maintainer: Henry G. Weller and Stefan Israelsson Tampe
;;;  Description:
;;    See README.org or README.html
;;;-----------------------------------------------------------------------------

(defmodule test-smatch
  (syntax (syntax-0
           smatch
           test-smatch-macros)
   import (level-0
           eval))

(defun f1 (l)
  (smatch l
    (() 'nil)
    (1  'one)
    ("hello" 'hello)
    ((1 2) 'const-1-2)
    ((('z c d) a b) (list 'nested-z a b c d))
    (('z . b) (list 'z-prefix b))
    ((and a (1 (set s) 3)) (s 12) a)
    ((? (lambda (x) (binary= x '(a b))) a b) (list 'var-?a-b a b))
    ((a b) (list 'var-a-b a b))
    ((a . b) (list 'var-a.b a b))
    (#(a b) (list 'vect-a-b a b))
    (#(#(c d) a b) (list 'vect-nested a b c d))
    (#(a b c ...) (list 'vect-ellip a b c))
    (_ 'anything-else)))

(deflocal a 1)
(deflocal b 1)

(defun f2 (l)
  (smatch l
    ((`,a b ... c d) (list 'var-ab...cd `,a b c d))
    (_ 'anything-else)))

(defun f3 (l)
  (smatch l
    ((a *** ((quote +) . l)) (list 'a***+l a))
    (_ 'anything-else)))

(defconstant fact
  (match-lambda
   (1 1)
   (n (* n (fact (- n 1))))))

(defmatchfun fact2
  (1 1)
  (n (* n (fact (- n 1)))))

(defmatchfun setit
  (n n))

(defmatchfun (setter setit)
  ((a (set s)) (s a)))

(deflocal m '(1 2))

(defun test ()
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
  (print-test (f1 '(1 2 3)))
  (print-test (f2 '(1 2 3 4 5 6 7)))
  (print-test (f3 '(* (+ 1 2))))
  (print-test (f1 #(1 2)))
  (print-test (f1 #(1 2 3 4 5)))
  (print-test (f1 #(#(3 4) 1 2)))
  (print-test (fact 6))
  (print-test (fact2 6))
  (print-test ((setter setit) m))
  (print-test (f1 (smatch '(let ((a 1)
                                 (b 2)
                                 (c 3))
                             (m a)
                             (m b)
                             (m c)
                             (list a b c))
                    (('let ((a b) ...) body ...)
                     (list vars: a  vals: b body: body))
                    (_ 'no-match))))
  (print-test (smatch '(let3 ((a b c) (d e f)) body1 body2)
                (('let3 ((a b c) ...) body ...) (list (list a b c) body))
                (_ 'no-match))))
(print "m = " m nl)
(test)
(print "m = " m nl)
(print  "m = " (smatch m ((get s) (s))) nl)
(print "Test match from outer context "
       (let ((A 1)) (smatch 1 ((unquote A) 'ok))) nl)


;;;-----------------------------------------------------------------------------
)  ;; End of module test-smatch
;;;-----------------------------------------------------------------------------
