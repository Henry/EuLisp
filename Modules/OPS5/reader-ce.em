;;;-----------------------------------------------------------------------------
;;;                 OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;; File   : reader-ce.em
;;; Date   : 19 Jul 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Description: Functions for creating condition elements.
;;;-----------------------------------------------------------------------------
(defmodule reader-ce
  (syntax (macros macros-tag)
   import (level1 basic reader-vars cond-el-1 cond-el-2 tests ops-out))

(print "### reader-ce")

;;;-----------------------------------------------------------------------------
;;; read-ce
; Process a condition element
;;;-----------------------------------------------------------------------------
(defun read-ce (is-neg attrib-table ce join-vars)
  ;;(sformat ops-out "read-ce: ~a neg? ~a ~a~%" ce is-neg join-vars)
  (let* ((class-name (car ce))
         (new-ce (if is-neg
                     (make-neg-njoin-ce class-name)
                   (make-pos-njoin-ce class-name))))
    (let ((res-ce (make-tests attrib-table new-ce
                              (cdr ce) join-vars ())))
      res-ce)))

;;;-----------------------------------------------------------------------------
;;; make-tests
; compiles the tests from a condition element
;;;-----------------------------------------------------------------------------
(defun make-tests (attrib-table ce tests-in join-vars curr-attrib)
  ;;(sformat ops-out "tests-in: ~a~%" tests-in)
  (if (null? tests-in)
      ce
    (let* ((attrib (if curr-attrib curr-attrib (car tests-in)))
           (tests  (if curr-attrib tests-in (cdr tests-in)))
           (first  (car tests)))
      ;;(format "first: ~a attrib: ~a~%" first attrib)
      (cond
        ((eql first '{)
              (make-tests attrib-table ce (cdr tests) join-vars attrib))
         ((eql first '})
         (make-tests attrib-table ce (cdr tests) join-vars ()))
        ((is-ops5-pred first)
         ;;(format "next: ~a~%" (cadr tests))
         (let* ((next (cadr tests))
                (type (if (is-constant next) 'CONSTANT
                        (if (member next join-vars) 'JOIN
                          'NON-JOIN)))
                (test (make-test attrib first next))
                (ce-after (insert-test ce attrib-table type test)))
           (make-tests attrib-table ce-after (cddr tests)
                       join-vars curr-attrib)))
        ((eql first '<<)
         (labels
          ((loop (tests-left vals)
                 ;;(sformat ops-out "tests-left: ~a  vals: ~a~%" tests-left vals)
                 (cond
                   ((eql '>> (car tests-left))
                    (let* ((type 'CONSTANT)
                           (pred '=)
                           (test (make-test attrib pred vals))
                           (ce-after (insert-test ce attrib-table type test)))
                      (make-tests attrib-table ce-after
                                  (cdr tests-left) join-vars curr-attrib)))
                   (t (loop (cdr tests-left) (cons (car tests-left) vals))))))
          (loop (cdr tests) ())))
        ((is-constant first)
         (make-tests attrib-table
                     (insert-test ce attrib-table 'CONSTANT
                                  (make-test attrib '= first))
                     (cdr tests) join-vars curr-attrib))
        ((is-ops5-var first)
         (let ((type (if (member first join-vars) 'JOIN 'NON-JOIN)))
           (make-tests attrib-table
                       (insert-test ce attrib-table type
                                    (make-test attrib '= first))
                       (cdr tests) join-vars curr-attrib)))
        (t (sformat ops-out "make-tests: ERROR~%"))))))

;;;-----------------------------------------------------------------------------
;;; make-attrib
; strip initial ^ to give attrib name
;;;-----------------------------------------------------------------------------
(defun make-attrib (attrib)
  ;;(format "make-attrib: ~a~%" attrib)
  (make-symbol (let ((str (make <string> size: 0)))
                 (accumulate
                  (lambda (a v)
                    (if (eql v #\\x005e )
                        a
                      (concatenate a (convert v <string>))))
                  str
                  (symbol-name attrib)))))

;;;-----------------------------------------------------------------------------
;;; make-test
; Makes a single test
;;;-----------------------------------------------------------------------------
(defun make-test (attrib pred val)
  (list (make-attrib attrib) pred val))

;;;-----------------------------------------------------------------------------
;;; insert-test
; Add a test to the list of tests of the same type held by the
; condition element. Tests are ordered in increasing order of
; index so that comparisons for equality can be made. If two
; tests have the same attribute then they will be ordered by
; predicate, then by value.
; Returns the updated condition element
;;;-----------------------------------------------------------------------------
(defun insert-test (ce attrib-table test-type test)
  ;;(format "insert-test: ~a ~a ~a~%" ce test-type test)
  (cond
    ((eql test-type 'CONSTANT)
     (insert-test1 ce attrib-table test ce-c-tests set-ce-c-tests))
    ((eql test-type 'NON-JOIN)
     (insert-test1 ce attrib-table test ce-v-tests set-ce-v-tests))
    ((eql test-type 'JOIN)
     (let ((class-type (class-of ce)))
       (if (or (eql class-type <neg-join-ce>) (eql class-type <pos-join-ce>))
           (insert-test1 ce attrib-table test ce-j-tests set-ce-j-tests)
         (if (eql class-type <neg-njoin-ce>)
             (insert-test1 (make-neg-from-njoin-ce (ce-class-name ce)
                                                   (ce-c-tests ce)
                                                   (ce-v-tests ce))
                           attrib-table test ce-j-tests set-ce-j-tests)
           (insert-test1 (make-pos-from-njoin-ce (ce-class-name ce)
                                                 (ce-c-tests ce)
                                                 (ce-v-tests ce))
                         attrib-table test ce-j-tests set-ce-j-tests)))))
    (t (sformat ops-out "insert-test: ERROR~%"))))

(defun insert-test1 (ce attrib-table test reader writer)
  ;;(format "insert-test1: ~a ~a ~a~%" test reader writer)
  (let ((index (element attrib-table (test-attrib test))))
    (writer ce (labels
                ((loop (tests)
                       (let ((index2 (if tests
                                         (element attrib-table (caar tests))
                                       ())))
                         (cond
                           ((null? tests)
                            (list test))
                           ((>= index2 index)
                            (cons test tests))
                           (t (let ((res (cons (car tests)
                                               (loop (cdr tests)))))
                                res))))))
                (loop (reader ce))))
    ce))

(export read-ce make-attrib)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
