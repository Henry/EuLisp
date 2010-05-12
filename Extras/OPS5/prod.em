;; File   : Eulisp input file; prod.em
;; Date   : 31 Jan 1995
;; Author : Tracy Gardner
;; Description: Production related classes for OPS5 implementation.
(defmodule prod
    (syntax (macros macros-tag)
     import (level1 basic prod-gf cond-el-gf cond-el-2
                    conflict action-gf ops-out))

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
     (ce-vars                ; associates ce-vars with ces
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

  (defmethod generic-prin ((prod <production>) (s <stream>))
    (format ops-out "Production: ~a~% Cond-Els: " (p-name prod))
    (map
      (lambda (x)
        (format ops-out "~a " (ce-id x)))
      (prod-ces prod))
    (print "Pos: ")
    (map
      (lambda (x)
        (format ops-out "~a " (ce-id x)))
      (prod-pos-ces prod))
    (format ops-out "~%"))

  (defmethod p-name ((prod <production>))
    (prod-name prod))

  (defmethod add-cond-el ((prod <production>) (ce <pos-join-ce>))
    ;;   (format ops-out "Rating: ~a~%" (prod-rating prod))
    ;;   (format ops-out "Prod-ordered ces: ~a~%" (prod-ordered-ces prod))
    (set-prod-pos-ces prod (append (prod-pos-ces prod) (list ce)))
    ;;   (set-prod-ordered-ces prod (cons ce (prod-ordered-ces prod)))
    (set-prod-ordered-ces prod
                          (labels ((loop (res)
                                         (cond
                                           ((null res) (list ce))
                                           ((or (eql (class-of (car res)) <neg-join-ce>)
                                                (eql (class-of (car res)) <pos-njoin-ce>))
                                            (cons ce res))
                                           (t (cons (car res)
                                                    (loop (cdr res)))))))
                                  (loop (prod-ordered-ces prod))))
    ;;(format t "(pj) ~a~%" (prod-ordered-ces prod))
    (set-prod-rating prod (+ (prod-rating prod) (ce-rating ce)))
    (set-prod-ces prod (cons ce (prod-ces prod))))

  (defmethod add-cond-el ((prod <production>) (ce <condition-element>))
    ;;   (format ops-out "Rating: ~a~%" (prod-rating prod))
    (when (eql (class-of ce) <neg-join-ce>)
          ;;(format t "(nj) ~a " ce)
          (set-prod-ordered-ces prod
                                (labels ((loop (res)
                                               (cond
                                                 ((null res) (list ce))
                                                 ((eql (class-of (car res)) <pos-njoin-ce>)
                                                  (cons ce res))
                                                 (t (cons (car res)
                                                          (loop (cdr res)))))))
                                        (loop (prod-ordered-ces prod)))))
    (when (eql (class-of ce) <pos-njoin-ce>)
          ;;(format t "(pnj) ")
          (set-prod-pos-ces prod (append (prod-pos-ces prod) (list ce)))
          (set-prod-ordered-ces prod
                                (append (prod-ordered-ces prod) (list ce))))
    (set-prod-rating prod (+ (prod-rating prod) (ce-rating ce)))
    ;;(format t "Ordered: ~a~%" (prod-ordered-ces prod))
    (set-prod-ces prod (append (prod-ces prod) (list ce))))
  ;; all-satisfied
  ;; Returns t if all condition elements of prod are satisfied.
  ;; () otherwise.

  (defun all-satisfied (prod)
    ;;(print "all-satisfied")
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
    ;;(format ops-out "Create prod insts ~a~%" ts)
    ;;(format ops-out "Prod: ~a~%" (p-name prod))
    (when (all-satisfied prod)
          ;;(print "")
          ;;(print "attempting join")
          ;;(prin (p-name prod)) (format t "(~a)" ts) (flush stdout)
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
             ;;(print "begin-join (pj)")
             ;;(format ops-out "join-list: ~a~%" join-tests)
             ;;(format ops-out "ts: ~a~%" ts)
             ;;(format ops-out "ce: ~a~%" ce)
             ;;(print  (prod-ordered-ces prod))
             ;;(print  (list-remove ce (prod-ordered-ces prod)))
             (let* ((all-ces (prod-ordered-ces prod))
                    (ces (list-remove ce all-ces))
                    (bindings (var-bindings ce ts)))
               ;;(format ops-out "Trying to instantiate (pj): ~a~%"
               ;;      (p-name prod))
               ;;    (size all-ces) (size ces))
               ;;(format ops-out "ces to join: ~a~%"  ces)
               ;;(format ops-out "Bindings: ~a ~a~%" bindings (ce-matches ce))
               (join ce prod
                     (if (null ces) () (car ces))
                     (if (null ces) () (cdr ces))
                     (list ts) (list (cons ce ts)) join-tests bindings
                     cr-manager)))
    method: (((prod <production>) ts (ce <neg-join-ce>) join-tests cr-manager)
             ;;(print "begin-join (nj)")
             (let* ((all-ces (prod-ordered-ces prod))
                    (ces (list-remove ce all-ces))
                    (bindings (var-bindings ce ts)))
               ;;(format ops-out "join-list: ~a~%" join-tests)
               ;;(format ops-out "Trying to instantiate (nj): ~a~%"
               ;;      (p-name prod))
               ;;(print all-ces)
               ;;(format ops-out "Bindings: ~a~%" bindings)
               (join ce prod
                     (if (null ces) () (car ces))
                     (if (null ces) () (cdr ces))
                     (list ts) () join-tests bindings
                     cr-manager)))

    method: (((prod <production>) ts (ce <pos-njoin-ce>) join-tests cr-manager)
             ;;(print "begin-join (pnj)")
             (let* ((all-ces (prod-ordered-ces prod))
                    (ces (list-remove ce all-ces))
                    (bindings (var-bindings ce ts)))
               ;;(format ops-out "Trying to instantiate (pnj): ~a~%"
               ;;      (p-name prod))
               ;;(print all-ces)
               ;;(format ops-out "Bindings: ~a ~a~%" bindings ts)
               (join ce prod
                     (if (null ces) () (car ces))
                     (if (null ces) () (cdr ces))
                     (list ts) (list (cons ce ts)) () bindings
                     cr-manager)))

    method: (((prod <production>) ts (ce <neg-njoin-ce>) join-tests cr-manager)
             ;;(print "begin-join (nnj)")
             (let* ((all-ces (prod-ordered-ces prod))
                    (ces (list-remove ce all-ces))
                    (bindings (var-bindings ce ts)))
               ;;(format ops-out "Trying to instantiate (nnj): ~a~%"
               ;;      (p-name prod))
               ;;        (format ops-out "Bindings: ~a~%" bindings)
               (join ce prod
                     (if (null ces) () (car ces))
                     (if (null ces) () (cdr ces))
                     () () () bindings
                     cr-manager))))

;;;-----------------------------------------------------------------------------
;;; join
;;;-----------------------------------------------------------------------------
  (defgeneric join ((ce0 <condition-element>) prod next-ce ce-list
                    timestamps ce-ts join-tests bindings cr-manager)
    method: (((ce0 <condition-element>) prod next-ce ce-list
              timestamps ce-ts join-tests bindings cr-manager)
             ;;(print "join")
             ;;(format ops-out "tests: ~a~%" join-tests)
             ;;(format ops-out "bindings: ~a~%" bindings)
             ;;(format ops-out "Prod: ~a Ces to go: ~a~%"
             ;;  (prod-name prod) (size ce-list))
             (cond
               ((null next-ce) ;create prod inst
                ;;(print ce-list)
                ;;(format ops-out  "Inserting Prod Inst: ~a~%" (p-name prod))
                ;;(format t "Bindings: ~a~%" bindings)
                ;;(print join-tests)
                ;;(print timestamps)
                ;;(format t "Ce-ts: ~a~%" ce-ts)
                ;;(format t "Ce-vars: ~a~%" (prod-ce-vars prod))
                ;; Add bindings for ce-vars
                (let ((new-bindings
                        (accumulate
                          (lambda (a x)
                            (cons (cons (car x) (cdr (assoc (cdr x) ce-ts))) a))
                          bindings
                          (prod-ce-vars prod))))
                  ;;(print new-bindings)
                  ;;(print (prod-rating prod))
                  ;;(print (get-ts 1 prod ce-ts))
                  (cs-insert cr-manager
                             (make-prod-instantiation prod new-bindings
                                                      timestamps ce-ts
                                                      (prod-rating prod)
                                                      (get-ts 1 prod ce-ts)))))
               (t
                 ;;(print "join-next")
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
    (format t "-") (flush stdout)
    ;;(print "dummy-join")
    (let* ((next-ce (if (null ce-list) () (car ce-list)))
           (rest-of-ces (if (null next-ce) () (cdr ce-list)))
           (matching-wmes (ce-matches curr-ce)))
      ;; (print matching-wmes)
      (if (null matching-wmes)
          (join ce0 prod next-ce rest-of-ces timestamps ce-ts join-tests
                bindings cr-manager)
        (do
          (lambda (match)
            ;;(format t "dummy-join join 2: ~a ~a~%" (car match) timestamps)
            (join ce0 prod next-ce rest-of-ces
                  (cons (car match) timestamps)
                  (cons (cons curr-ce (car match)) ce-ts)
                  join-tests
                  (set-union bindings (cadr match))
                  cr-manager))
          matching-wmes))))

  (defun join-join (ce0 prod curr-ce ce-list timestamps
                        ce-ts join-tests bindings cr-manager)
    ;;(format t ".") (flush stdout)
    ;;(format ops-out "join-join: ~a~%" join-tests)
    ;;(format t "Remaining ces: ~a~%" (size ce-list))
    ;;(print bindings)
    (let* ((next-ce (if (null ce-list) () (car ce-list)))
           (rest-of-ces (if (null next-ce) () (cdr ce-list)))
           (all-ces (cons curr-ce ce-list))
           (res
             (if (eql (class-of curr-ce) <neg-join-ce>)
                 (labels ((loop (ces)
                                (when (null ces)
                                      (print "Error: Unable to continue join"))
                                (let ((jtest (find-shared-jv join-tests
                                                             (car ces))))
                                  (if jtest (cons jtest (car ces))
                                    (loop (cdr ces))))))
                         (loop all-ces))
               (labels ((loop (ces)
                              (cond
                                ((null ces) ())
                                ((eql (class-of (car ces)) <neg-join-ce>) ())
                                (t
                                  (let ((jtest (find-shared-jv join-tests
                                                               (car ces))))
                                    (if jtest (cons jtest (car ces))
                                      (loop (cdr ces))))))))
                       (loop all-ces))))
           (test      (if res (car res) ()))
           (jv-ce     (if res (cdr res) ())))
      (if (null res)
          (if (and (eql (class-of curr-ce) <neg-join-ce>)
                   (null (ce-matches curr-ce)))
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
               (rest-of-ces (if (null rest) () (cdr rest)))
               (next-ce (if (null rest) () (car rest)))
               (vals (assoc (cadadr test) (ce-jv-vals jv-ce) eql))
               (jv-values (if (null vals) () (cdr vals)))
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
             (print "solve-join: ERROR")))

  (defun solve-join-pos (jv-ce matching-tstamps
                               ce0 prod next-ce rest-of-ces timestamps ce-ts
                               join-tests bindings cr-manager)
    ;;(prin "P ") (flush stdout)
    ;;(print "solve-join-pos")
    ;;(format ops-out "bindings: ~a~%" bindings)
    ;;(format ops-out "join-tests: ~a~%" join-tests)
    ;;(format t       "tstamps: ~a~%" matching-tstamps)
    ;;(print rest-of-ces)
    ;;(prin (size matching-tstamps)) (flush stdout)
    (do
      (lambda (x)
        ;;(format t "current tstamp: ~a~%" x)
        (let ((consis-b (compute-consistent join-tests (join-list jv-ce x))))
          ;;(print consis-b)
          (when consis-b
                ;;(print "solve-join-pos join 1")
                (join ce0 prod next-ce rest-of-ces (cons x timestamps)
                      (cons (cons jv-ce x) ce-ts)
                      consis-b
                      (set-union bindings (var-bindings jv-ce x))
                      cr-manager))))
      matching-tstamps))

  (defun solve-join-neg (jv-ce matching-tstamps
                               ce0 prod next-ce rest-of-ces timestamps ce-ts
                               join-tests bindings cr-manager)
    ;;(print "solve-join-neg")
    ;;(prin "N ")   (print matching-tstamps)
    ;;(flush stdout)
    (let ((res (labels
                 ((find-consis (tstamps jtests ce)
                               (cond
                                 ((null tstamps) ())
                                 ((compute-consistent jtests
                                                      (join-list ce (car tstamps)))
                                  t)
                                 (t
                                   (find-consis (cdr tstamps) jtests ce)))))
                 (find-consis matching-tstamps join-tests jv-ce))))
      ;;(if res (format t "No join: ~a~%" res)
      ;;(print "No match -- join continuing"))
      (unless ;; if there are no consistent variable
        ;; bindings for jv-ce (-ve) then continue the join
        res
        ;;(format t "Match found: ~a~%" res)
        (join ce0 prod next-ce rest-of-ces timestamps
              ce-ts join-tests bindings cr-manager))))

  (defun compute-consistent (j-tests0 j-tests1)
    ;;(format ops-out "j-tests0: ~a~%j-tests1 ~a~%" j-tests0 j-tests1)
    ; At present this will only work if there is only one test
    ; per join variable in a condition element
    (let ((res (labels
                 ((consis (join0 join1 new-join)
                          ;;(format t "join0: ~a join1: ~a new-join: ~a~%"
                          ;;      join0 join1 new-join)
                          (cond
                            ((null join0)
                             (append new-join join1))
                            (t (let* ((test0 (car join0))
                                      (jv (cadadr test0))
                                      (test1 (find-test jv join1)))
                                 ;;(print test1)
                                 (cond
                                   ((null test1) ; no occurence of jv
                                    (consis (cdr join0)
                                            join1
                                            (cons test0 new-join)))
                                   (t
                                     (let ((val0 (car test0))
                                           (pred0 (caadr test0))
                                           (val1 (car test1))
                                           (pred1 (caadr test1)))
                                       ;;(format t "val0: ~a pred0: ~a val1: ~a pred1: ~a~%"
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
                               ((null tests) ())
                               ((binary= jv (cadadr (car tests)))
                                (car tests))
                               (t (find-test jv (cdr tests))))))
                 (consis j-tests0 j-tests1 ()))))
      ;;(print res)
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
    ;; (print "find-shared-jv")
    ;;(print (ce-j-tests curr-ce))
    ;;(print (class-of curr-ce))
    ;;(print curr-ce)
    (if (eql (class-of curr-ce) <neg-join-ce>)
        ;; All jvs must be in list by now
        (let ((var (caddr (car (ce-j-tests curr-ce)))))
          ;; (format t "Var: ~a~%" var) (flush stdout)
          (labels ((loop (jlist)
                         ;;(when jlist (print (car jlist))
                         ;;    (print (cadadr (car jlist))))
                         (cond
                           ((null jlist) (print "ERROR"))
                           ((eql (cadadr (car jlist)) var)
                            ;;(format t "j-test: ~a~%" (car jlist))
                            (car jlist))
                           (t (loop (cdr jlist))))))
                  (loop join0)))
      (let ((restricts (ce-j-tests curr-ce)))
        ;;(format t "jv: ~a~%" (ce-jv-vals curr-ce))
        ;;(format t "m: ~a~%"  (ce-matches curr-ce))
        ;;(format t "join0: ~a~%" join0)
        ;;(format t "restricts: ~a~%" restricts)
        (labels ((loop1 (jlist)
                        (cond
                          ((null jlist) ())
                          (t
                            (let ((jtest
                                    (labels ((loop2 (test jlist2)
                                                    (cond
                                                      ((null jlist2) ())
                                                      (t
                                                        ;;(format t "p1: ~a p2: ~a v1: ~a v2: ~a~%"
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
                              ;;(format t "jtest: ~a~%" jtest)
                              (if jtest jtest
                                (loop1 (cdr jlist))))))))
                (loop1 join0)))))

  (defun query-pos (jv-values test)
    (let ((val (car test))
          (var (cadadr test))
          (pred (caadr test)))
      ;;(print "query")
      ;;(format ops-out "val: ~a pred: ~a var: ~a jv-values: ~a~%" val pred
      ;;    var jv-values)
      (let ((res (accumulate
                   (lambda (a x)
                     (if (or (eql pred '=)
                             (test-succeeds val pred (car x)))
                         (cons (cdr x) a)
                       a))
                   ()
                   jv-values)))
        ;;(print res)
        res)))

  (defmethod get-ts (ce-num (prod <production>) ce-ts)
    (let* ((ce (element (prod-pos-ces prod)
                        (- ce-num 1)))
           ;;(convert (- ce-num 1) <integer>)))
           (val (assoc ce ce-ts)))
      (if (null val)
          (format ops-out "Error: No timestamp for ~a~%" (ce-id ce))
        (cdr (assoc ce ce-ts)))))

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
      (format ops-out "Firing production: ~a~%" (p-name prod))
      (labels ((loop (actions)
                     (cond
                       ((null actions)
                        (fire-prod-inst cr-manager wm-manager ce-manager))
                       ((eql (class-of (car actions)) <halt-action>)
                        (format ops-out "Execution terminated by halt action~%"))
                       (t
                         (execute (car actions) prod-inst wm-manager
                                  ce-manager cr-manager)
                         (loop (cdr actions))))))
              (loop (prod-actions prod)))))

  (export p-name make-production prod-actions set-prod-actions <production>
   prod-ce-vars set-prod-ce-vars)

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
