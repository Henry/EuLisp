;; File   : Eulisp input file; conflict.em
;; Date   : 31 Jan 1995
;; Author : Tracy Gardner
;; Description: Conflict set and conflict resolution classes for OPS5
;;          implementation.

(defmodule conflict
  (syntax (syntax-0 macros-tag)
   import (level-0 basic merge prod-gf ops-out))

(print "### conflict" nl)

(defclass <conflict-set> ()
  ((prod-insts
    default: ()
    reader:  prod-insts
    writer:  set-prod-insts))

  constructor: (make-conflict-set))

(defclass <prod-instantiation> ()
  ((prod                  ; production of which this is an instantiation
    keyword: prod:
    reader:  pi-prod)
   (bindings
    keyword: bindings:
    reader:  pi-bindings
    writer:  set-pi-bindings)
   (timestamps
    keyword: timestamps:
    reader:  timestamps
    writer:  set-timestamps)
   (ce-ts-list
    keyword: ce-ts-list:
    reader:  ce-ts-list)
   (rating
    keyword: rating:
    reader:  rating)
   (first-ts
    keyword: first-ts:
    reader:  first-ts))
  constructor: (make-prod-instantiation prod: bindings:
                                        timestamps: ce-ts-list:
                                        rating: first-ts:))

(defmethod generic-print ((pi <prod-instantiation>) (s <stream>))
  (print "Production Instantiation:" nl)
  (sformat ops-out "First-ts: ~a Rating: ~a~% Ts: ~a~%"
           (first-ts pi) (rating pi) (timestamps pi))
  (sformat ops-out "Prod: ~a~% Bindings: ~a~%" (p-name (pi-prod pi))
           (pi-bindings pi)))

(defclass <cr-manager> ()
  ((conflict-set
    default: (make-conflict-set)
    reader:  cs)
   (strategy
    keyword: strategy:
    reader:  strategy
    writer:  set-strategy))
  constructor: (make-cr-manager strategy:))

(defun print-cr (cr)
  (print "Conflict Set:" nl)
  (do
   (lambda (x) (print x nl))
   (prod-insts (cs cr)))
  ())

(defgeneric cs-insert ((cr-man <cr-manager>)
                       (prod-inst <prod-instantiation>))
  method: (((cr-man <cr-manager>)
            (prod-inst <prod-instantiation>))
           ;;(print "cs-insert" nl)
           ; sort timestamps into decreasing order
           (set-timestamps prod-inst (merge-sort
                                      (timestamps prod-inst)
                                      (lambda (x y) (> x y))))
           (set-prod-insts (cs cr-man)
                           (cons prod-inst (prod-insts (cs cr-man))))))

(defun remove-by-timestamp (cr-man ts)
  ;;(sformat ops-out "remove-by-timestamp: ~a~%" ts)
  (let ((c-set (cs cr-man)))
    (set-prod-insts c-set
                    (accumulate
                     (lambda (a p)
                       (if (member ts (timestamps p))
                           a
                         (cons p a)))
                     ()
                     (prod-insts c-set)))))

(defun remove-by-prod (cr-man prod0)
  (let ((c-set (cs cr-man)))
    (set-prod-insts c-set
                    (accumulate
                     (lambda (a p)
                       (if (eql prod0 (pi-prod p))
                           a
                         (cons p a)))
                     ()
                     (prod-insts c-set)))))

(defun remove-by-bindings (cr-man tests prod0)
  ;;(format "remove-by-bindings: ~a" tests)
  (let ((c-set (cs cr-man)))
    ;;(print c-set nl)
    (set-prod-insts c-set
                    (accumulate
                     (lambda (a p)
                       (if (eql prod0 (pi-prod p))
                           (progn ;;(format "Trying to remove: ~a~%" p)
                             ;;(print (size tests) nl)
                             (if
                                 (accumulate
                                  (lambda (fail test)
                                    ;;(print (cadadr test) nl)
                                    (let* ((var (cadadr test))
                                           (tmp (member-alist var (pi-bindings p)))
                                           (val (if tmp (cdr tmp) 'NIL)))
                                      ;;(format "Test: ~a ~a ~a~%"
                                      ;; (car test) (caadr test) val)
                                      (if (test-succeeds (car test)
                                                         (caadr test)
                                                         val)
                                          fail ;;(progn (print "SUCC ") fail)
                                        ())))
                                  't
                                  tests)
                                 a ;;(progn (print "PI Removed" nl) a)
                               (cons p a)))
                         (cons p a)))
                     ()
                     (prod-insts c-set)))
    ;;(print c-set nl)
    c-set))

(defun set-cr-strategy (cr-man strat)
  (set-strategy (cr-man) strat))

(defun fire-prod-inst (cr-manager wm-manager ce-manager)
  ;; (print "OPS5: Conflict Resolution Process initiated" nl)
  ;; (print-cr cr-manager)
  (let* ((cset (prod-insts (cs cr-manager)))
         (prod-inst
          (if (null? cset)
              (progn (print "OPS5: conflict set empty: goodbye" nl)
                     ())
            (if (binary= (strategy cr-manager) 'mea)
                (select-mea cset)
              (select-lex cset)))))
    (set-prod-insts (cs cr-manager)
                    (list-remove prod-inst cset))
    (when prod-inst (fire prod-inst wm-manager ce-manager cr-manager))))

(defun select-mea (cs)
  (let ((best-mea (find-best-mea cs)))
    ;; (sformat ops-out "Best: ~a"  best-mea)
    (cond
      ((= (size best-mea) 1) (car best-mea))
      (t (select-lex best-mea)))))

(defun find-best-mea (cs)
  (accumulate
   (lambda (best next)
     (let ((best-ts (first-ts (car best)))
           (next-ts (first-ts next)))
       ;;(sformat ops-out "best: ~a next: ~a~%" best-ts next-ts)
       (cond
         ((< next-ts best-ts) best)
         ((= next-ts best-ts) (cons next best))
         (t (list next)))))
   (list (car cs))
   (cdr cs)))

(defun select-lex (cs)
  ;;(print "select-lex" nl)
  (let ((best-lex (find-best-lex 0 cs)))
    ;;(sformat ops-out "Best lex: ~a~%" best-lex)
    (car
     (cond
       ((= (size best-lex) 1) best-lex)
       (t ; need to go by rating
        ;;(print "Go by rating" nl)
        (accumulate
         (lambda (best next)
           (let ((best-rating (rating (car best)))
                 (next-rating (rating next)))
             (cond
               ((< next-rating best-rating) best)
               ((= next-rating best-rating) (cons next best))
               (t (list next)))))
         (list (car best-lex))
         (cdr best-lex)))))))

(defun find-best-lex (elt c-set)
  (let ((new-c-set
         (accumulate
          (lambda (best next)
            ;; (format "Best: ~a Next: ~a Elt: ~a~%"
            ;;      best next elt)
            (cond
              ((<= (size (timestamps next)) elt) best)
              ((null? best) (list next))
              (t
               (let ((best-ts (element (timestamps (car best)) elt))
                     (next-ts (element (timestamps next) elt)))
                 ;;(format "best-ts: ~a next-ts: ~a~%" best-ts next-ts)
                 (cond
                   ((< next-ts best-ts) best)
                   ((= next-ts best-ts) (cons next best))
                   (t (list next)))))))
          ()
          c-set)))
    (cond
      ;;(print new-c-set nl)
      ((null? new-c-set) c-set) ; members of c-set all equal so far
      ((= (size new-c-set) 1) new-c-set) ; one dominating prod-inst
      (t (find-best-lex (+ elt 1) new-c-set)))))

;;;-----------------------------------------------------------------------------
;;; test-succeeds
;;;-----------------------------------------------------------------------------
(defun test-succeeds (x pred y)
  ;;(format "test-succeeds: ~a ~a ~a~%" x pred y)
  (let ((res (cond
               ((list? y) (labels ((find-success (val val-list)
                                                 (cond
                                                   ((null? val-list) ())
                                                   ((eql val (car val-list)) t) ; only pred allowed
                                                   (t (find-success val (cdr val-list))))))
                                  (find-success x y)))
               ((eql pred '<=>) (binary= (class-of x) (class-of y)))
               ((not (eql (class-of x) (class-of y))) ())
               ((eql pred '=)   (eql x y))
               ((eql pred '<>)  (not (eql x y)))
               ((eql pred '<)   (< x y))
               ((eql pred '>)   (> x y))
               ((eql pred '<=)  (<= x y))
               ((eql pred '>=)  (>= x y))
               (t (format "Error: Unknown predicate: ~a~%" pred)))))
    ;;(format "res: ~a~%" res)
    res))

(export make-prod-instantiation make-cr-manager set-cr-strategy
 remove-by-prod remove-by-timestamp remove-by-bindings
 cs-insert <cr-manager> <prod-instantiation>
 fire-prod-inst ce-ts-list pi-bindings pi-prod
 set-pi-bindings)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
