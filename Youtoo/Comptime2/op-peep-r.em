;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: peep-hole optimization rules
;;;-----------------------------------------------------------------------------
(defmodule op-peep-r
  (syntax (_macros _op-peep0)
   import (i-all op-peep))
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
)  ;; end of module
;;;-----------------------------------------------------------------------------
