;;; Copyright 1995 Tracy Gardner & University of Bath
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
;;; Title: Production related classes for OPS5 implementation
;;;  Library: ops5
;;;  Authors: Tracy Gardner
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------

(defmodule prod
  (syntax (syntax-0
           macros-tag)
   import (level-0
           basic
           prod-gf
           cond-el-gf
           cond-el-2
           conflict
           action-gf
           ops-out)
   export (p-name
           make-production
           prod-actions
           set-prod-actions
           <production>
           prod-ce-vars
           set-prod-ce-vars))

;;;-----------------------------------------------------------------------------
;;; <production>
;  Production class, models user defined production.
;;;-----------------------------------------------------------------------------
(defclass <production> ()
  ((p-name                   ; used as a key into prod-ce-table
    keyword:  p-name:
    reader: prod-name)

   (ces
    default: ()
    reader: prod-ces
    writer: set-prod-ces)
   (pos-ces
    default: ()
    reader: prod-pos-ces
    writer: set-prod-pos-ces)
   (ordered-ces            ; in order pos-join neg-join pos-njoin
    default: ()
    reader: prod-ordered-ces
    writer: set-prod-ordered-ces)
   (ce-vars                ; member-alistiates ce-vars with ces
    default: ()
    reader:  prod-ce-vars
    writer:  set-prod-ce-vars)
   (actions                ; consequent of production
    default: ()
    keyword: actions:
    reader:  prod-actions
    writer:  set-prod-actions)

   (p-rating               ; rating based on number of tests in antecedent
    default: 0
    reader:  prod-rating
    writer:  set-prod-rating))
  constructor: (make-production p-name:))

(defmethod generic-print ((prod <production>) (s <stream>))
  (sformat ops-out "Production: ~a~% Cond-Els: " (p-name prod))
  (map
   (lambda (x)
     (sformat ops-out "~a " (ce-id x)))
   (prod-ces prod))
  (print "Pos: " nl)
  (map
   (lambda (x)
     (sformat ops-out "~a " (ce-id x)))
   (prod-pos-ces prod))
  (sformat ops-out "~%"))

(defmethod p-name ((prod <production>))
  (prod-name prod))

(defmethod add-cond-el ((prod <production>) (ce <pos-join-ce>))
  ;;   (sformat ops-out "Rating: ~a~%" (prod-rating prod))
  ;;   (sformat ops-out "Prod-ordered ces: ~a~%" (prod-ordered-ces prod))
  (set-prod-pos-ces prod (append (prod-pos-ces prod) (list ce)))
  ;;   (set-prod-ordered-ces prod (cons ce (prod-ordered-ces prod)))
  (set-prod-ordered-ces prod
                        (letfuns ((loop (res)
                                       (cond
                                         ((null? res) (list ce))
                                         ((or (eql (class-of (car res)) <neg-join-ce>)
                                              (eql (class-of (car res)) <pos-njoin-ce>))
                                          (cons ce res))
                                         (t (cons (car res)
                                                  (loop (cdr res)))))))
                                (loop (prod-ordered-ces prod))))
  ;;(format "(pj) ~a~%" (prod-ordered-ces prod))
  (set-prod-rating prod (+ (prod-rating prod) (ce-rating ce)))
  (set-prod-ces prod (cons ce (prod-ces prod))))

(defmethod add-cond-el ((prod <production>) (ce <condition-element>))
  ;;   (sformat ops-out "Rating: ~a~%" (prod-rating prod))
  (when (eql (class-of ce) <neg-join-ce>)
        ;;(format "(nj) ~a " ce)
        (set-prod-ordered-ces prod
                              (letfuns ((loop (res)
                                             (cond
                                               ((null? res) (list ce))
                                               ((eql (class-of (car res)) <pos-njoin-ce>)
                                                (cons ce res))
                                               (t (cons (car res)
                                                        (loop (cdr res)))))))
                                      (loop (prod-ordered-ces prod)))))
  (when (eql (class-of ce) <pos-njoin-ce>)
        ;;(format "(pnj) ")
        (set-prod-pos-ces prod (append (prod-pos-ces prod) (list ce)))
        (set-prod-ordered-ces prod
                              (append (prod-ordered-ces prod) (list ce))))
  (set-prod-rating prod (+ (prod-rating prod) (ce-rating ce)))
  ;;(format "Ordered: ~a~%" (prod-ordered-ces prod))
  (set-prod-ces prod (append (prod-ces prod) (list ce))))
;; all-satisfied
;; Returns t if all condition elements of prod are satisfied.
;; () otherwise.

(defun all-satisfied (prod)
  ;;(print "all-satisfied" nl)
  (accumulate
   (lambda (a x)
     (if (is-satisfied x)
         a
       ()))
   t
   (prod-ces prod)))

;;;-----------------------------------------------------------------------------
;;; create-prod-insts
;  If all condition elements within the production are satisfied then
;  the join process is initiated by calling begin join to prepare the
;  arguments.
;  Note that join-tests must be passed in since if this method was called
;  as the result of the removal of a negative joinable ce then the join
;  tests will no longer be stored by the ce.
;  On the other hand bindings need not be passed in since negative ces
;  can create no variable bindings.
;;;-----------------------------------------------------------------------------
(defmethod create-prod-insts ((prod <production>) ts
                              (ce <condition-element>)
                              join-tests
                              cr-manager)
  ;;(sformat ops-out "Create prod insts ~a~%" ts)
  ;;(sformat ops-out "Prod: ~a~%" (p-name prod))
  (when (all-satisfied prod)
        ;;(print "" nl)
        ;;(print "attempting join" nl)
        ;;(print (p-name prod)) (format "(~a)" ts) (sflush stdout)
        (begin-join prod ts ce join-tests cr-manager)))

;;;-----------------------------------------------------------------------------
;;; begin-join
;  Called to initiate the join process. Prepares arguments and then
;  calls the join method.
;;;-----------------------------------------------------------------------------
(defgeneric begin-join ((prod <production>) ts
                        (ce <condition-element>)
                        join-tests
                        cr-manager)
  method: (((prod <production>) ts (ce <pos-join-ce>) join-tests cr-manager)
           ;;(print "begin-join (pj)" nl)
           ;;(sformat ops-out "join-list: ~a~%" join-tests)
           ;;(sformat ops-out "ts: ~a~%" ts)
           ;;(sformat ops-out "ce: ~a~%" ce)
           ;;(print  (prod-ordered-ces prod) nl)
           ;;(print  (list-remove ce (prod-ordered-ces prod)) nl)
           (let* ((all-ces (prod-ordered-ces prod))
                  (ces (list-remove ce all-ces))
                  (bindings (var-bindings ce ts)))
             ;;(sformat ops-out "Trying to instantiate (pj): ~a~%"
             ;;      (p-name prod))
             ;;    (size all-ces) (size ces))
             ;;(sformat ops-out "ces to join: ~a~%"  ces)
             ;;(sformat ops-out "Bindings: ~a ~a~%" bindings (ce-matches ce))
             (join ce prod
                   (if (null? ces) () (car ces))
                   (if (null? ces) () (cdr ces))
                   (list ts) (list (cons ce ts)) join-tests bindings
                   cr-manager)))
  method: (((prod <production>) ts (ce <neg-join-ce>) join-tests cr-manager)
           ;;(print "begin-join (nj)" nl)
           (let* ((all-ces (prod-ordered-ces prod))
                  (ces (list-remove ce all-ces))
                  (bindings (var-bindings ce ts)))
             ;;(sformat ops-out "join-list: ~a~%" join-tests)
             ;;(sformat ops-out "Trying to instantiate (nj): ~a~%"
             ;;      (p-name prod))
             ;;(print all-ces nl)
             ;;(sformat ops-out "Bindings: ~a~%" bindings)
             (join ce prod
                   (if (null? ces) () (car ces))
                   (if (null? ces) () (cdr ces))
                   (list ts) () join-tests bindings
                   cr-manager)))

  method: (((prod <production>) ts (ce <pos-njoin-ce>) join-tests cr-manager)
           ;;(print "begin-join (pnj)" nl)
           (let* ((all-ces (prod-ordered-ces prod))
                  (ces (list-remove ce all-ces))
                  (bindings (var-bindings ce ts)))
             ;;(sformat ops-out "Trying to instantiate (pnj): ~a~%"
             ;;      (p-name prod))
             ;;(print all-ces nl)
             ;;(sformat ops-out "Bindings: ~a ~a~%" bindings ts)
             (join ce prod
                   (if (null? ces) () (car ces))
                   (if (null? ces) () (cdr ces))
                   (list ts) (list (cons ce ts)) () bindings
                   cr-manager)))

  method: (((prod <production>) ts (ce <neg-njoin-ce>) join-tests cr-manager)
           ;;(print "begin-join (nnj)" nl)
           (let* ((all-ces (prod-ordered-ces prod))
                  (ces (list-remove ce all-ces))
                  (bindings (var-bindings ce ts)))
             ;;(sformat ops-out "Trying to instantiate (nnj): ~a~%"
             ;;      (p-name prod))
             ;;        (sformat ops-out "Bindings: ~a~%" bindings)
             (join ce prod
                   (if (null? ces) () (car ces))
                   (if (null? ces) () (cdr ces))
                   () () () bindings
                   cr-manager))))

;;;-----------------------------------------------------------------------------
;;; join
;;;-----------------------------------------------------------------------------
(defgeneric join ((ce0 <condition-element>) prod next-ce ce-list
                  timestamps ce-ts join-tests bindings cr-manager)
  method: (((ce0 <condition-element>) prod next-ce ce-list
            timestamps ce-ts join-tests bindings cr-manager)
           ;;(print "join" nl)
           ;;(sformat ops-out "tests: ~a~%" join-tests)
           ;;(sformat ops-out "bindings: ~a~%" bindings)
           ;;(sformat ops-out "Prod: ~a Ces to go: ~a~%"
           ;;  (prod-name prod) (size ce-list))
           (cond
             ((null? next-ce) ;create prod inst
              ;;(print ce-list nl)
              ;;(sformat ops-out  "Inserting Prod Inst: ~a~%" (p-name prod))
              ;;(format "Bindings: ~a~%" bindings)
              ;;(print join-tests nl)
              ;;(print timestamps nl)
              ;;(format "Ce-ts: ~a~%" ce-ts)
              ;;(format "Ce-vars: ~a~%" (prod-ce-vars prod))
              ;; Add bindings for ce-vars
              (let ((new-bindings
                     (accumulate
                      (lambda (a x)
                        (cons (cons (car x) (cdr (member-alist (cdr x) ce-ts))) a))
                      bindings
                      (prod-ce-vars prod))))
                ;;(print new-bindings nl)
                ;;(print (prod-rating prod) nl)
                ;;(print (get-ts 1 prod ce-ts) nl)
                (cs-insert cr-manager
                           (make-prod-instantiation prod new-bindings
                                                    timestamps ce-ts
                                                    (prod-rating prod)
                                                    (get-ts 1 prod ce-ts)))))
             (t
              ;;(print "join-next" nl)
              (join-next ce0 prod next-ce ce-list timestamps ce-ts
                         join-tests bindings cr-manager)))))

;;;-----------------------------------------------------------------------------
;;; join-next
;;;-----------------------------------------------------------------------------
(defgeneric join-next ((ce0 <condition-element>) prod
                       (next-ce <condition-element>) ce-list timestamps
                       ce-ts join-tests bindings cr-manager)
  method: (((ce0 <condition-element>) prod (next-ce <pos-join-ce>)
            ce-list timestamps ce-ts join-tests bindings cr-manager)
           (join-join ce0 prod next-ce ce-list timestamps
                      ce-ts join-tests bindings cr-manager))
  method: (((ce0 <condition-element>) prod (next-ce <neg-join-ce>)
            ce-list timestamps ce-ts join-tests bindings cr-manager)
           (join-join ce0 prod next-ce ce-list timestamps
                      ce-ts join-tests bindings cr-manager))
  method: (((ce0 <condition-element>) prod (next-ce <pos-njoin-ce>)
            ce-list timestamps ce-ts join-tests bindings cr-manager)
           (dummy-join ce0 prod next-ce ce-list timestamps
                       ce-ts join-tests bindings cr-manager))
  method: (((ce0 <condition-element>) prod (next-ce <neg-njoin-ce>)
            ce-list timestamps ce-ts join-tests bindings cr-manager)
           (dummy-join ce0 prod next-ce ce-list timestamps
                       ce-ts join-tests bindings cr-manager)))

;;;-----------------------------------------------------------------------------
;;; dummy-join
;;;-----------------------------------------------------------------------------
(defun dummy-join (ce0 prod curr-ce ce-list timestamps
                       ce-ts join-tests bindings cr-manager)
  (format "-") (sflush stdout)
  ;;(print "dummy-join" nl)
  (let* ((next-ce (if (null? ce-list) () (car ce-list)))
         (rest-of-ces (if (null? next-ce) () (cdr ce-list)))
         (matching-wmes (ce-matches curr-ce)))
    ;; (print matching-wmes nl)
    (if (null? matching-wmes)
        (join ce0 prod next-ce rest-of-ces timestamps ce-ts join-tests
              bindings cr-manager)
      (do
       (lambda (match)
         ;;(format "dummy-join join 2: ~a ~a~%" (car match) timestamps)
         (join ce0 prod next-ce rest-of-ces
               (cons (car match) timestamps)
               (cons (cons curr-ce (car match)) ce-ts)
               join-tests
               (set-union bindings (cadr match))
               cr-manager))
       matching-wmes))))

(defun join-join (ce0 prod curr-ce ce-list timestamps
                      ce-ts join-tests bindings cr-manager)
  ;;(format ".") (sflush stdout)
  ;;(sformat ops-out "join-join: ~a~%" join-tests)
  ;;(format "Remaining ces: ~a~%" (size ce-list))
  ;;(print bindings nl)
  (let* ((next-ce (if (null? ce-list) () (car ce-list)))
         (rest-of-ces (if (null? next-ce) () (cdr ce-list)))
         (all-ces (cons curr-ce ce-list))
         (res
          (if (eql (class-of curr-ce) <neg-join-ce>)
              (letfuns ((loop (ces)
                             (when (null? ces)
                                   (print "Error: Unable to continue join" nl))
                             (let ((jtest (find-shared-jv join-tests
                                                          (car ces))))
                               (if jtest (cons jtest (car ces))
                                 (loop (cdr ces))))))
                      (loop all-ces))
            (letfuns ((loop (ces)
                           (cond
                             ((null? ces) ())
                             ((eql (class-of (car ces)) <neg-join-ce>) ())
                             (t
                              (let ((jtest (find-shared-jv join-tests
                                                           (car ces))))
                                (if jtest (cons jtest (car ces))
                                  (loop (cdr ces))))))))
                    (loop all-ces))))
         (test      (if res (car res) ()))
         (jv-ce     (if res (cdr res) ())))
    (if (null? res)
        (if (and (eql (class-of curr-ce) <neg-join-ce>)
                 (null? (ce-matches curr-ce)))
            (join ce0 prod next-ce rest-of-ces timestamps
                  ce-ts join-tests bindings cr-manager)
          (do
           (lambda (x)
             (join ce0 prod next-ce rest-of-ces
                   (cons (car x) timestamps)
                   (cons (cons curr-ce (car x)) ce-ts)
                   (set-union join-tests (join-list curr-ce (car x)))
                   (set-union bindings (var-bindings curr-ce (car x)))
                   cr-manager))
           (ce-matches curr-ce)))
      (let* ((rest (list-remove jv-ce all-ces))
             (rest-of-ces (if (null? rest) () (cdr rest)))
             (next-ce (if (null? rest) () (car rest)))
             (vals (member-alist (cadadr test) (ce-jv-vals jv-ce) eql))
             (jv-values (if (null? vals) () (cdr vals)))
             (matching-tstamps (query-pos jv-values test)))
        (solve-join jv-ce matching-tstamps
                    ce0 prod next-ce rest-of-ces timestamps ce-ts
                    join-tests bindings cr-manager)))))

(defgeneric solve-join ((jv-ce <condition-element>) matching-tstamps
                        ce0 prod next-ce rest-of-ces timestamps ce-ts
                        join-tests bindings cr-manager)
  method: (((jv-ce <pos-join-ce>) matching-tstamps
            ce0 prod next-ce rest-of-ces timestamps
            ce-ts join-tests bindings cr-manager)
           (solve-join-pos jv-ce matching-tstamps
                           ce0 prod next-ce rest-of-ces timestamps ce-ts
                           join-tests bindings cr-manager))
  method: (((jv-ce <neg-join-ce>) matching-tstamps
            ce0 prod next-ce rest-of-ces timestamps ce-ts
            join-tests bindings cr-manager)
           (solve-join-neg jv-ce matching-tstamps
                           ce0 prod next-ce rest-of-ces timestamps ce-ts
                           join-tests bindings cr-manager))
  method: (((jv-ce <condition-element>) matching-tstamps
            ce0 prod next-ce rest-of-ces timestamps ce-ts
            join-tests bindings cr-manager)
           (print "solve-join: ERROR" nl)))

(defun solve-join-pos (jv-ce matching-tstamps
                             ce0 prod next-ce rest-of-ces timestamps ce-ts
                             join-tests bindings cr-manager)
  ;;(print "P ") (sflush stdout)
  ;;(print "solve-join-pos" nl)
  ;;(sformat ops-out "bindings: ~a~%" bindings)
  ;;(sformat ops-out "join-tests: ~a~%" join-tests)
  ;;(format       "tstamps: ~a~%" matching-tstamps)
  ;;(print rest-of-ces nl)
  ;;(print (size matching-tstamps)) (sflush stdout)
  (do
   (lambda (x)
     ;;(format "current tstamp: ~a~%" x)
     (let ((consis-b (compute-consistent join-tests (join-list jv-ce x))))
       ;;(print consis-b nl)
       (when consis-b
             ;;(print "solve-join-pos join 1" nl)
             (join ce0 prod next-ce rest-of-ces (cons x timestamps)
                   (cons (cons jv-ce x) ce-ts)
                   consis-b
                   (set-union bindings (var-bindings jv-ce x))
                   cr-manager))))
   matching-tstamps))

(defun solve-join-neg (jv-ce matching-tstamps
                             ce0 prod next-ce rest-of-ces timestamps ce-ts
                             join-tests bindings cr-manager)
  ;;(print "solve-join-neg" nl)
  ;;(print "N ")   (print matching-tstamps nl)
  ;;(sflush stdout)
  (let ((res (letfuns
              ((find-consis (tstamps jtests ce)
                            (cond
                              ((null? tstamps) ())
                              ((compute-consistent jtests
                                                   (join-list ce (car tstamps)))
                               t)
                              (t
                               (find-consis (cdr tstamps) jtests ce)))))
              (find-consis matching-tstamps join-tests jv-ce))))
    ;;(if res (format "No join: ~a~%" res)
    ;;(print "No match -- join continuing" nl))
    (unless ;; if there are no consistent variable
     ;; bindings for jv-ce (-ve) then continue the join
     res
     ;;(format "Match found: ~a~%" res)
     (join ce0 prod next-ce rest-of-ces timestamps
           ce-ts join-tests bindings cr-manager))))

(defun compute-consistent (j-tests0 j-tests1)
  ;;(sformat ops-out "j-tests0: ~a~%j-tests1 ~a~%" j-tests0 j-tests1)
  ; At present this will only work if there is only one test
  ; per join variable in a condition element
  (let ((res (letfuns
              ((consis (join0 join1 new-join)
                       ;;(format "join0: ~a join1: ~a new-join: ~a~%"
                       ;;      join0 join1 new-join)
                       (cond
                         ((null? join0)
                          (append new-join join1))
                         (t (let* ((test0 (car join0))
                                   (jv (cadadr test0))
                                   (test1 (find-test jv join1)))
                              ;;(print test1 nl)
                              (cond
                                ((null? test1) ; no occurence of jv
                                 (consis (cdr join0)
                                         join1
                                         (cons test0 new-join)))
                                (t
                                 (let ((val0 (car test0))
                                       (pred0 (caadr test0))
                                       (val1 (car test1))
                                       (pred1 (caadr test1)))
                                   ;;(format "val0: ~a pred0: ~a val1: ~a pred1: ~a~%"
                                   ;;      val0 pred0 val1 pred1)
                                   (cond
                                     ((eql pred0 '=)
                                      (when (test-succeeds val1 pred1 val0)
                                            (consis
                                             (cdr join0)
                                             (list-remove test1 join1 binary=)
                                             (cons test0 new-join))))
                                     ((eql pred1 '=)
                                      (when (test-succeeds val0 pred0 val1)
                                            (consis
                                             (cdr join0)
                                             (list-remove test1 join1 binary=)
                                             (cons test1 new-join))))
                                     (t
                                      (consis
                                       (cdr join0)
                                       (list-remove test1 join1 binary=)
                                       (cons test1 (cons test0 new-join))))))))))))
               (find-test (jv tests)
                          (cond
                            ((null? tests) ())
                            ((binary= jv (cadadr (car tests)))
                             (car tests))
                            (t (find-test jv (cdr tests))))))
              (consis j-tests0 j-tests1 ()))))
    ;;(print res nl)
    res))

(defun set-union (l1 l2)
  (accumulate
   (lambda (a x)
     (if (member x a binary=)
         a
       (cons x a)))
   l1
   l2))

(defun find-shared-jv (join0 curr-ce)
  ;; (print "find-shared-jv" nl)
  ;;(print (ce-j-tests curr-ce) nl)
  ;;(print (class-of curr-ce) nl)
  ;;(print curr-ce nl)
  (if (eql (class-of curr-ce) <neg-join-ce>)
      ;; All jvs must be in list by now
      (let ((var (caddr (car (ce-j-tests curr-ce)))))
        ;; (format "Var: ~a~%" var) (sflush stdout)
        (letfuns ((loop (jlist)
                       ;;(when jlist (print (car jlist) nl)
                       ;;    (print (cadadr (car jlist)) nl))
                       (cond
                         ((null? jlist) (print "ERROR" nl))
                         ((eql (cadadr (car jlist)) var)
                          ;;(format "j-test: ~a~%" (car jlist))
                          (car jlist))
                         (t (loop (cdr jlist))))))
                (loop join0)))
    (let ((restricts (ce-j-tests curr-ce)))
      ;;(format "jv: ~a~%" (ce-jv-vals curr-ce))
      ;;(format "m: ~a~%"  (ce-matches curr-ce))
      ;;(format "join0: ~a~%" join0)
      ;;(format "restricts: ~a~%" restricts)
      (letfuns ((loop1 (jlist)
                      (cond
                        ((null? jlist) ())
                        (t
                         (let ((jtest
                                (letfuns ((loop2 (test jlist2)
                                                (cond
                                                  ((null? jlist2) ())
                                                  (t
                                                   ;;(format "p1: ~a p2: ~a v1: ~a v2: ~a~%"
                                                   ;;      (caadr test) (cadr (car jlist2))
                                                   ;;      (cadadr test) (caddr (car jlist2)))
                                                   (if (and
                                                        (or (eql (caadr test) '=)
                                                            (eql (cadr (car jlist2)) '=))
                                                        (eql (cadadr test)
                                                             (caddr (car jlist2))))
                                                       test
                                                     (loop2 test (cdr jlist2)))))))
                                        (loop2 (car jlist) restricts))))
                           ;;(format "jtest: ~a~%" jtest)
                           (if jtest jtest
                             (loop1 (cdr jlist))))))))
              (loop1 join0)))))

(defun query-pos (jv-values test)
  (let ((val (car test))
        (var (cadadr test))
        (pred (caadr test)))
    ;;(print "query" nl)
    ;;(sformat ops-out "val: ~a pred: ~a var: ~a jv-values: ~a~%" val pred
    ;;    var jv-values)
    (let ((res (accumulate
                (lambda (a x)
                  (if (or (eql pred '=)
                          (test-succeeds val pred (car x)))
                      (cons (cdr x) a)
                    a))
                ()
                jv-values)))
      ;;(print res nl)
      res)))

(defmethod get-ts (ce-num (prod <production>) ce-ts)
  (let* ((ce (element (prod-pos-ces prod)
                      (- ce-num 1)))
         ;;(convert (- ce-num 1) <integer>)))
         (val (member-alist ce ce-ts)))
    (if (null? val)
        (sformat ops-out "Error: No timestamp for ~a~%" (ce-id ce))
      (cdr (member-alist ce ce-ts)))))

;;;-----------------------------------------------------------------------------
;;; remove-prod-insts
;;  Remove production instantiations that have become invalid through a change
;;  to working memory. Only insertions of negative condition element cause
;;  this method to be called, removals of positive condition elements are
;;  handled directly by the conflict resolution manager.
;;;-----------------------------------------------------------------------------
(defmethod remove-prod-insts ((prod <production>) ts (ce <neg-join-ce>)
                              cr-manager)
  (when (all-satisfied prod)
        (remove-by-bindings cr-manager (join-list ce ts) prod)))

(defmethod remove-prod-insts ((prod <production>) ts (ce <neg-njoin-ce>)
                              cr-manager)
  (when (binary= (ce-num-matched ce) 1)
        (remove-by-prod cr-manager prod)))

(defmethod fire ((prod-inst <prod-instantiation>)
                 wm-manager ce-manager cr-manager)
  (let ((prod (pi-prod prod-inst)))
    (sformat ops-out "Firing production: ~a~%" (p-name prod))
    (letfuns ((loop (actions)
                   (cond
                     ((null? actions)
                      (fire-prod-inst cr-manager wm-manager ce-manager))
                     ((eql (class-of (car actions)) <halt-action>)
                      (sformat ops-out "Execution terminated by halt action~%"))
                     (t
                      (execute (car actions) prod-inst wm-manager
                               ce-manager cr-manager)
                      (loop (cdr actions))))))
            (loop (prod-actions prod)))))

;;;-----------------------------------------------------------------------------
)  ;; End of module prod
;;;-----------------------------------------------------------------------------
