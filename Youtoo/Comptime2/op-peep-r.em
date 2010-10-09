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
;;; Title: peep-hole optimization rules
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule op-peep-r
  (syntax (_macros
           _op-peep0)
   import (i-all
           op-peep))

;;;-----------------------------------------------------------------------------
;;; Null
;;;-----------------------------------------------------------------------------
(simple-rule ((null) (null)) ())

;;;-----------------------------------------------------------------------------
;;; Function calls
;;;-----------------------------------------------------------------------------
(simple-rule ((call-operator nargs) (return n))
             ((tail-call-operator nargs n)))

(simple-rule ((tail-call-operator nargs n) (return m))
             ((tail-call-operator nargs n)))

;;;-----------------------------------------------------------------------------
;;; Clearing the stack
;;;-----------------------------------------------------------------------------
(simple-rule ((nobble 0)) ())

(guarded-rule ((nobble n))
              (< 255 n)
              ((nobble 255)
               (nobble (let ((m (- n 255)))
                         (if (< 255 m)
                             255
                           (if (< m 0) m 0))))
               (nobble (let ((m (- n (* 2 255))))
                         (if (< 255 m)
                             255
                           (if (< m 0) m 0))))
               (nobble (let ((m (- n (* 3 255))))
                         (if (< 255 m)
                             255
                           (if (< m 0) m 0))))))

(guarded-rule ((nobble n) (return m))
              (< (+ n m) 256)
              ((return (+ n m))))

(guarded-rule ((nobble n) (nobble m))
              (< (+ n m) 256)
              ((nobble (+ n m))))

;;;-----------------------------------------------------------------------------
;;; Jumps
;;;-----------------------------------------------------------------------------
(simple-rule ((null) (branch-nil label)) ((branch-true label)))
(simple-rule ((null) (branch-true label))((branch-nil label)))
(simple-rule ((branch lab) (return n)) ((branch lab)))
(guarded-rule ((branch lab1) (label lab2)) (eq lab1 lab2) ())

;;;-----------------------------------------------------------------------------
;;; Stack access
;;;-----------------------------------------------------------------------------
(simple-rule ((stack-ref 0)) ((stack-ref0)))
(simple-rule ((stack-ref 1)) ((stack-ref1)))
(simple-rule ((stack-ref 2)) ((stack-ref2)))
(simple-rule ((stack-ref n) (pop1)) ())

;;;-----------------------------------------------------------------------------
;;; Display access
;;;-----------------------------------------------------------------------------
(simple-rule ((display-ref n m) (pop1)) ())

;;;-----------------------------------------------------------------------------
;;; Register access
;;;-----------------------------------------------------------------------------
(simple-rule ((register-ref *) (pop1)) ())

;;;-----------------------------------------------------------------------------
;;; List access
;;;-----------------------------------------------------------------------------
(simple-rule ((car) (car)) ((caar)))
(simple-rule ((cdr) (car)) ((cadr)))
(simple-rule ((car) (cdr)) ((cdar)))
(simple-rule ((cdr) (cdr)) ((cddr)))
(simple-rule ((cdr) (cdr) (car)) ((caddr)))
(simple-rule ((cdr) (cdr) (cdr) (car)) ((cadddr)))

;;;-----------------------------------------------------------------------------
)  ;; End of module op-peep-r
;;;-----------------------------------------------------------------------------
