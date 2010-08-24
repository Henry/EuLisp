;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: peep-hole optimization
;;;-----------------------------------------------------------------------------
(defmodule op-peep
  (syntax (_macros _op-peep0)
   import (i-all)
   export (add-rule peep-hole-optimize))

(defun peep-hole-optimize (l)
  (labels
   ((loop (ll)
          (cond ((null? (cdr ll))
                 l)
                ((apply-rule (cdr ll) ll)
                 (loop ll))
                (t
                 (loop (cdr ll))))))
   (if (null? *peephole*) l
     (loop (cons () l)))))

;;;-----------------------------------------------------------------------------
;;; Add rules
;;;-----------------------------------------------------------------------------
(deflocal *rule-table* (make <table>))

(defun add-rule (rule guard rewrite n)
  (let* ((key (car (car rule)))
         (other-rules (table-ref *rule-table* key)))
    ((setter table-ref) *rule-table* key
     (cons (list rule guard rewrite n) other-rules)))
  rule)

;;;-----------------------------------------------------------------------------
;;; Apply rule
;;;-----------------------------------------------------------------------------
(defun apply-rule (code update)
  (labels
   ((loop (l)
          (if (null? l) ()
            (let* ((rule-entry (car l))
                   (rule (car rule-entry))
                   (guard (car (cdr rule-entry)))
                   (rewrite (car (cdr (cdr rule-entry))))
                   (res (match-rule rule code)))
              (if (and res
                       (if guard
                           (apply guard (car res))
                         t))
                  (let ((new (apply rewrite (car res)))
                        (rule-size (car (cdr (cdr (cdr rule-entry))))))
                    (notify0 "[~a => ~a]" rule new)
                    ((setter cdr) update
                     (append new (list-drop code rule-size)))
                    t)
                (loop (cdr l)))))))
   (loop (table-ref *rule-table* (car (car code))))))

;;;-----------------------------------------------------------------------------
;;; Match rule
;;;-----------------------------------------------------------------------------
(defun match-rule (rule code)
  (labels
   ((loop (r l res)
          (if (null? r)
              (list res)
            (if (null? l) ()
              (let ((x (match-pattern (car r) (car l))))
                (and x
                     (loop (cdr r) (cdr l)
                           (append res (car x)))))))))
   (loop rule code ())))

(defun match-pattern (r l)
  ;; Assume equal length
  (labels
   ((loop (rr ll)
          (if (null? rr)
              ;; Return actual params if match; could be ()!
              (list (cdr l))
            (let ((x1 (car rr))
                  (x2 (car ll)))
              (if (symbol? x1)
                  (loop (cdr rr) (cdr ll))
                (if (binary= x1 x2)
                    (loop (cdr rr) (cdr ll))
                  ()))))))
   (and (eq (car r) (car l))
        (loop (cdr r) (cdr l))))))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;; Test
;;;-----------------------------------------------------------------------------
(peep-hole-optimize
 '((a b) (null) (null) (stack-ref 0) (cdr)
   (nobble 1) (c d e) (branch foo1) (label foo1)))
