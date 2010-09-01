;;; Copyright 1994-2010 Fraunhofer ISST
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'Eu2C'
;;;-----------------------------------------------------------------------------
;;
;;  Eu2C is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;;  Title:
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Horst Friedrich
;;;-----------------------------------------------------------------------------

#module inline
(import ((except (format print) eulisp1)
         SIMPLE-PROGRAMMING
         LZS
         MZS
         context
         analyse-h
         type-inference
         type-propagation
         (only (get-result-type) ti-signature)
         (only (true-subtype-expr?) ti-meet-join)
         (only (get-last-substitution set-right-expr) ti-eqs)
         (only (ti-print-string-no-cr) ti-write)
         vector
         (only (error format print assoc length append mapc) common-lisp)
         function-call-context)
 syntax (eulisp1)
 export (inline-a))


;; definitions and init-forms
(defvar ienv ())
(defvar iresult ())

(defun inline-a (con fun args resultvar)
  ;; inline-a will be calld with a <test> or an <arg> - context
  ;; 1. <arg>-context: During inlining the <arg>-context can be change to an
  ;; <join-label>-context. The result is a new tempvar.
  ;; 2. <test>-context: The results are
  ;;          a. the seted slots then-type-descr-s and else-type-descr-s
  ;;          b. the seted slot divided
  (let ((var-vec (?var-vec (?var-descr fun))))
    (dynamic-let ((ienv (mk-inline-env 1 (length var-vec) var-vec
                                       args))
                  (iresult resultvar))
                 (if (or (eq con t) (null? con))
                     (let ((old-tds (dynamic typepathes))
                           (res (inline-block1
                                 con
                                 (?start-block (?function-label fun))))
                           (new-tds (dynamic typepathes)))
                       (if (eq res
                               (vector-ref
                                (?var-vec
                                 (?var-descr
                                  (?stat (car new-tds))))
                                0))
                           (specialize-inline-descrs new-tds old-tds)
                         ())
                       res)
                   ;;                 (if (eq con (dynamic *arg-context*))
                   ;;                   (dynamic-let
                   ;;                    ((iresult (make <tempvar> :tnr (a-number))))
                   ;;                    (dynamic-setq ienv
                   ;;                                  (cons
                   ;;                                   (cons (vector-ref var-vec 0)
                   ;;                                         (dynamic iresult))
                   ;;                                   (dynamic ienv)))
                   ;;                    (let ((endcontext
                   ;;                           (inline-block
                   ;;                            con
                   ;;                            (?start-block (?function-label fun)))))
                   ;;                      (if (eq endcontext (dynamic *arg-context*)) ()
                   ;;                          (handle-join-label-context endcontext)
                   ;;                          )
                   ;;                      (dynamic iresult))
                   ;;                    )..)
                   (inline-block con
                                 (?start-block (?function-label fun))))))
  )

(defun specialize-inline-descrs (new-descrs old-descrs)
  (mapc (lambda (new-descr)
          (specialize-inline-descr new-descr old-descrs))
        new-descrs))

(defun specialize-inline-descr (new-descr old-descrs)
  (let ((old-descr (specialize-inline-descr? new-descr old-descrs)))
    (if old-descr
        (let ((old-result-type (get-result-type old-descr))
              (new-result-type (get-result-type new-descr)))
          (if (true-subtype-expr? old-result-type
                                   new-result-type)
              (let* ((subs (?type-vars new-descr))
                     (type-var (vector-ref (?type-vec new-descr) 0))
                     (equ (get-last-substitution subs type-var)))
                (set-right-expr equ old-result-type)
                ;;              (format t "~%Specialized inline result type ~A -> ~A"
                ;;                      (ti-print-string-no-cr new-result-type)
                ;;                      (ti-print-string-no-cr old-result-type))
                ))))))

(defun specialize-inline-descr? (new-descr old-descrs)
  (let ((prev-descr (?t-descr-before new-descr)))
    (if (and prev-descr (atom? prev-descr))
        (if (member prev-descr old-descrs)
            prev-descr
          (specialize-inline-descr? prev-descr old-descrs))
      ())))

(defun handle-join-label-context (con)
  (let ((in-block (?in-block con)))
    (if (null? in-block) ; function without return ???
        (error "inline destroy a function")
      (if (null? (cdr in-block))
          ;; only one block
          (let* ((bl (car in-block))
                 (interface (?interface bl)))
            (setf (?body bl) (clear-interface interface
                                              (?body bl)))
            (setf (?result bl) ())
            (setf (?interface bl) ())
            (setf (?out-label bl) ())
            ;; (dynamic-setq typepathes (?t-path bl))
            (dynamic-setq block bl))
        ;; copy of code from file if-form.em
        (let (
              ;;(nenv (join-variables in-block
              ;;                      (?env-level con)))
              ;;(ntypepathes (append-tpathes in-block))
              (nblock (make <block> :in-label con))
              (result (dynamic *void-context*)))
          (setf (?out-block con) nblock)
          ;;          (if (eq (?context con) *void-context*)
          ;;            (kill-results (?in-block con))
          ;;            (setq result  (join-results (?in-block con))))
          ;; neuer block env etc.
          ;;          (setf (dynamic env) nenv)
          ;;          (setf (dynamic typepathes) ntypepathes)
          (setf (dynamic block) nblock)
          result))))
  )

(defun clear-interface (interf body)
  (if (null? interf) body
    (clear-interface (cdr interf)
                     (append-stat body (car interf))))
  )

;; wenn eine Konstante an eine Variable gebunden  wird, und die Variable hat mehr
;; als einen Zuweisungslink, dann muss erst ein move eingefuegt werden !

(defun mk-inline-env (from to old new)
  (if (>= from to) ()
    (cons (cons (vector-ref old from)
                (vector-ref new from))
          (mk-inline-env (+ from 1) to old new)))
  )

(defun inline-block (con block)
  (inline-body (?body block) ())
  (inline-interface (?interface block))
  (inline-result con (?result block))
  (if (test? (?result block))
      ()
    (if (goto? (?result block))
        (setf (?out-label (dynamic block))
              (cdr (assoc (?out-label block) (dynamic ienv))))
      (inline-out-label (?out-label block) con)))
  con
  )


(defun inline-block1 (last-call block)
  (inline-body (?body block) last-call)
  (inline-result (dynamic *arg-context*) (?result block)))

(defgeneric inline-out-label (label con))

(defmethod inline-out-label ((label <function-label>)
                             (con <join-label>))
  (setf (?out-label (dynamic block)) con)
  )

(defmethod inline-out-label ((label <function-label>) con)
  ;; con = <arg> or <test>
  ())

(defmethod inline-out-label ((label <zykl-label>) con)
  (let* ((curblock (dynamic block))
         (newblock (make <block> ))
         (new-label (make <zykl-label>
                          :in-block (list curblock)
                          :out-block newblock)))
    (dynamic-setq ienv (cons (cons label new-label)
                             (dynamic ienv)))
    (setf (?out-label curblock) new-label)
    (setf (?in-label newblock) new-label)
    (dynamic-setq block newblock)
    (inline-block con (?out-block label)))
  )

;; !!! inline-out-label ((label <join-label>) not yet implemented

(defun inline-body (body last)
  (if body
      (progn (setf (?body (dynamic block))
                   (append-stat (?body (dynamic block))
                                (copy-stat (car body) last)))
             (inline-body (cdr body) last))
    ()))

(defun inline-interface (body)
  (if body
      (progn (setf (?interface (dynamic block))
                   (append-stat (?interface (dynamic block))
                                (copy-stat (car body) () )))
             (inline-interface (cdr body)))
    ()))

(defgeneric inline-result (con result))

(defmethod inline-result ((con <arg>) (result <test>))
  (print "arg-test not yet implemented"))

(defmethod inline-result ((con <join-label>) (result <test>))
  (print "join-label-test not yet implemented"))

(defmethod inline-result ((con <test>) (result <test>))
  (let* ((then-block (?then-block result))
         (then-body (?body then-block))
         (then-interface (?interface then-block))
         (then-result (?result then-block))
         (then-empty (and (null? then-body) (null? then-interface)))
         (then-empty-return (and then-empty (return? then-result)))
         ;;   else
         (else-block (?else-block result))
         (else-body (?body else-block))
         (else-interface (?interface else-block))
         (else-result (?result else-block))
         (else-empty (and (null? else-body) (null? else-interface)))
         (else-empty-return (and else-empty (return? else-result)))
         (divided (?divided con))
         )
    (if (and then-empty-return else-empty-return (null? divided))
        (let* ((var-vec (make-vector 3))
               (var-descr (make <var-descr>
                                :var-vec var-vec
                                :constant-counter 0)))
          (setf (?function con) (?function result))
          (setf (?read-glocs con) (?read-glocs result))
          (setf (?var-descr con) var-descr)
          (setf (?arg-num con) 2)
          (setf (?type-descr con) (general-var-actual-descr 2))
          (copy-vars 2 (?var-vec (?var-descr result)) var-descr con)
          ;; doppelter link  (link-var-vec var-vec con 2)
          (let ((value (?value then-result)))
            (if (or (null? value)
                    (and (named-const? value)
                         (null? (?value value))))
                (progn
                  (setq value (?then-block con))
                  (setf (?then-block con) (?else-block con))
                  (setf (?else-block con) value))
              ()))
          con)
      (let* ((newtest (copy-stat result ()))
             ;; result: test with or without then- and else-blocks
             (new-then-block (?then-block newtest))
             (new-else-block (?else-block newtest))
             (curblock (dynamic block)))
        (if (and new-then-block new-else-block)
            (progn (setf (?divided con) t)
                   (if then-empty-return
                       (let* ((value (?value then-result))

                              (newblock (if (or (null? value)
                                                (and (named-const? value)
                                                     (null? (?value value))))
                                            ;; the result is () / false
                                            (progn
                                              (setf (?else-type-descr-s con)
                                                    (append (?else-type-descr-s newtest)
                                                            (?else-type-descr-s con)))
                                              (?else-block con))
                                          ;; the result is t / true
                                          (progn
                                            (setf (?then-type-descr-s con)
                                                  (append (?then-type-descr-s newtest)
                                                          (?then-type-descr-s con)))
                                            (?then-block con))
                                          )))
                         (add-inlabel newtest newblock)
                         ;;              (setf (?in-label newblock)
                         ;;                    (cons newtest (?in-label newblock)))
                         (setf (?then-block newtest)
                               newblock))
                     ())
                   (if else-empty-return
                       (let* ((value (?value else-result))
                              (newblock (if (or (null? value)
                                                (and (named-const? value)
                                                     (null? (?value value))))
                                            ;; the result is () / false
                                            (progn
                                              (setf (?else-type-descr-s con)
                                                    (append (?else-type-descr-s newtest)
                                                            (?else-type-descr-s con)))
                                              (?else-block con))
                                          ;; the result is t / true
                                          (progn
                                            (setf (?then-type-descr-s con)
                                                  (append (?then-type-descr-s newtest)
                                                          (?then-type-descr-s con)))
                                            (?then-block con))
                                          )))
                         (add-inlabel newtest newblock)
                         ;;              (setf (?in-label newblock)
                         ;;                    (cons newtest (?in-label newblock)))
                         (setf (?else-block newtest)
                               newblock))
                     ())

                   (setf (?result curblock)
                         newtest)
                   (setf (?block newtest) curblock)
                   (if else-empty-return ()
                     (progn
                       (dynamic-setq block (?else-block newtest))
                       (inline-block con else-block)))
                   (if then-empty-return ()
                     (progn
                       (dynamic-setq block (?then-block newtest))
                       (inline-block con then-block)
                       )))
          ;;              ;; neither the else-path nor the then-path returned
          ;;              (let ((newcon (make <test> :then-block (?then-block con)
          ;;                                  :else-block (?else-block con)
          ;;                                  :then-type-descr-s ()
          ;;                                  :else-type-descr-s ()))
          ;;                    (curblock (dynamic block)))
          ;;                (setf (?result curblock) newtest)
          ;;                (setf (?block newtest) curblock)
          ;;                (dynamic-setq block (?then-block newtest))
          ;;                (inline-block newcon then-block)
          ;;                (dynamic-setq block (?else-block newtest))
          ;;                (inline-block con else-block)
          ;;                )
          ;; one block is empty, newtest is not necessary
          (if new-then-block
              (inline-block con then-block)
            (if new-else-block
                (inline-block con else-block)
              (error "inline-type-error")))))))
  )

(defun add-inlabel (test block)
  (let ((in-label (?in-label block)))
    (setf (?in-label block)
          (if (cons? in-label)
              (cons test in-label)
            (if (null? in-label) (list test)
              (list test in-label)))))
  )

(defmethod inline-result (con (result <goto>))
  (let* ((curblock (dynamic block))
         (newlbl (cdr (assoc (?label result) (dynamic ienv))))
         (argnum (?arg-num result))
         (var-vec (make-vector (+ argnum 1)))
         (var-descr (make <var-descr>
                          :var-vec var-vec
                          :constant-counter 0))
         (newgoto (make <goto> :label newlbl
                        :arg-num argnum
                        :var-descr var-descr
                        :block curblock)))
    (copy-vars argnum (?var-vec (?var-descr result)) var-descr newgoto)
    ;; (link-var-vec var-vec newgoto argnum)
    (setf (?result curblock) newgoto)
    (setf (?in-block newlbl)
          (cons curblock (?in-block newlbl))))
  )

(defmethod inline-result ((con <test>) (result <return>))
  (error  ; error-call to dedect programm errors
   "inline (context:test) of an not realy test-context function")
  )

(defmethod inline-result ((con <arg>) (result <return>))
  (let* ((value (?value result))
         (ass (assoc value (dynamic ienv))))
    (if ass (cdr ass) value)))

(defmethod inline-result ((con <join-label>) (result <return>))
  (print "join-label-result not yet implemented"))

(defmethod inline-result (con (result <void>))
  ;; handle out-label
  (setf (?result (dynamic block)) result)
  )

(defmethod inline-result ((con <test>) (result <tempvar>))
  (print "inline test-tempvar not yet implemented")
  )

(defmethod inline-result ((con <test>) (result <local-static>))
  (error "inline test-local-static not yet implemented")
  )

;;(defmethod inline-result ((con <test>) result)
;;  (print "inline test-local-statoc not yet implemented")
;;)


(defgeneric copy-stat (stat last))

(defmethod copy-stat ((stat <get-slot-value>) last)
  (let* ((var-vec (make-vector 2))
         (var-descr (make <var-descr>
                          :var-vec var-vec
                          :constant-counter 0))
         (newgetslot (make <get-slot-value>
                           :type-descr (general-var-actual-descr 1)
                           :arg-num 1 :var-descr var-descr
                           :type-descr-s () ; missing read-glocs, write-glocs
                           :slot (?slot stat)
                           )))
    (copy-vars 1 (?var-vec (?var-descr stat)) var-descr newgetslot)
    (link-var-vec var-vec newgetslot 1)
    (setf (?block newgetslot) (dynamic block))
    (setf (dynamic get-slot-value) (cons newgetslot (dynamic get-slot-value)))
    (let ((type-descr-s (inference-get-slot-value
                         (get-slot-tds (dynamic typepathes)
                                       (vector-ref var-vec 1) newgetslot)
                         (?slot newgetslot))))
      (setf (?type-descr-s newgetslot) type-descr-s)
      (dynamic-setq typepathes type-descr-s))
    newgetslot))

(defmethod copy-stat ((stat <set-slot-value>) last)
  (let* ((var-vec (make-vector 3))
         (var-descr (make <var-descr>
                          :var-vec var-vec
                          :constant-counter 0))
         (newsetslot (make <set-slot-value>
                           :type-descr (general-var-actual-descr 2)
                           :arg-num 2 :var-descr var-descr
                           :type-descr-s () ; missing read-glocs, write-glocs
                           :slot (?slot stat)
                           )))
    (copy-vars 2 (?var-vec (?var-descr stat)) var-descr newsetslot)
    (link-var-vec var-vec newsetslot 2)
    (setf (?block newsetslot) (dynamic block))
    (setf (dynamic set-slot-value) (cons newsetslot (dynamic set-slot-value)))
    (let ((type-descr-s (inference-set-slot-value
                         (set-slot-tds (dynamic typepathes)
                                       (vector-ref var-vec 1)
                                       (vector-ref var-vec 2)
                                       newsetslot)
                         (?slot newsetslot))))
      (setf (?type-descr-s newsetslot) type-descr-s)
      (dynamic-setq typepathes type-descr-s)
      )
    newsetslot))

(defmethod copy-stat ((stat <move>) last)
  (let* ((var-vec (make-vector 2))
         (var-descr (make <var-descr>
                          :var-vec var-vec
                          :constant-counter 0))
         (newmove (make <move>
                        :type-descr (general-var-actual-descr 1)
                        :arg-num 1
                        :var-descr var-descr
                        :type-descr-s ()
                        :read-glocs (?read-glocs stat)
                        :write-glocs (?write-glocs stat))))
    (copy-vars 1 (?var-vec (?var-descr stat)) var-descr newmove)
    (link-var-vec var-vec newmove 1)
    (setf (?block newmove) (dynamic block))
    (setf (dynamic moves)
          (cons newmove (dynamic moves)))
    newmove)
  )

(defmethod copy-stat ((stat <funcall>) last)
  (let* ((arg-num (?arg-num stat))
         (value (?value stat))
         (var-vec (make-vector (+ arg-num 1)))
         (var-descr (make <var-descr>
                          :var-vec var-vec
                          :constant-counter 0))
         (newcall (make <funcall>
                        ;; :value value
                        :type-descr (general-var-actual-descr arg-num)
                        :arg-num (?arg-num stat)
                        :var-descr var-descr
                        :read-glocs (?read-glocs stat)
                        :write-glocs (?write-glocs stat))))
    (copy-vars (?arg-num stat) (?var-vec (?var-descr stat)) var-descr newcall)
    (let ((typedescrs
           (make-actual-type-descr (dynamic typepathes)
                                   newcall
                                   (?var-descr newcall)
                                   arg-num
                                   ())))
      ;; make a type - inference
      (setq typedescrs
            (inference (?function stat) typedescrs))
      (if (null? typedescrs) (error "Typeerror")
        (progn
          ;; add the type-descriptors
          (setf (?type-descr-s newcall) typedescrs)
          (setf (dynamic typepathes) typedescrs))
        )
      (link-var-vec var-vec newcall arg-num)
      ;; handle funcall-function
      (let ((ass  (assoc value (dynamic ienv))))
        (setf (?value newcall)
              (if ass
                  (link-new-var (cdr ass) newcall)
                (copy-and-link-var value newcall))))
      (setf (?block newcall) (dynamic block))
      (setf (dynamic calls)
            (cons newcall (dynamic calls)))
      newcall))
  )


(defgeneric link-new-var (nvar stat))

(defmethod link-new-var ((var <local-static>) stat)
  (setf (?link var)
        (cons (cons stat 'die-panda-war-gerade-abgeschaltet)
              (?link var)))
  var
  )

(defmethod link-new-var ((var <tempvar>) stat)
  (setf (?link var)
        (cons (cons stat 'die-panda-war-gerade-abgeschaltet)
              (?link var)))
  var
  )

(defmethod link-new-var (var stat)
  var)

(defgeneric copy-and-link-var (var stat))

(defmethod copy-and-link-var ((var <local-static>) stat)
  (let ((newvar (make <tempvar> :tnr (a-number)
                      :link (list (cons stat
                                        'die-panda-war-gerade-abgeschaltet))
                      ))
        )
    (dynamic-setq ienv (cons (cons var newvar) (dynamic ienv)))
    newvar)
  )

(defmethod copy-and-link-var ((var <tempvar>) stat)
  (let ((newvar (make <tempvar> :tnr (a-number)
                      :link (list (cons stat
                                        'die-panda-war-gerade-abgeschaltet))
                      ))
        )
    (dynamic-setq ienv (cons (cons var newvar) (dynamic ienv)))
    newvar)
  )

(defmethod copy-and-link-var (con stat) con)



(defmethod copy-stat ((stat <test>) last)
  ;; result: test with or without then- and else-blocks
  (let* ((then-block (make <block>))
         (else-block (make <block>))
         (test (make <test>
                     ;; :then-block then-block
                     ;; :else-block else-block
                     :arg-num 2)))
    (setf (?in-label then-block) test)
    (setf (?in-label else-block) test)
    (let* ((var-vec (make-vector 3))
           (var-descr (make <var-descr>
                            :var-vec var-vec
                            :constant-counter 0))
           (fun (?function stat)))
      (setf (?function test) fun)
      (setf (?read-glocs test) (?read-glocs stat))
      (setf (?var-descr test) var-descr)
      (setf (?type-descr test) (general-var-actual-descr 2))
      (copy-vars 2 (?var-vec (?var-descr stat)) var-descr test)
      (let ((typedescrs
             (make-actual-type-descr (dynamic typepathes)
                                     test
                                     var-descr
                                     2
                                     ()))
            (then-typedescrs ())
            (else-typedescrs ()))
        ;; make a type - inference
        (setq typedescrs
              (inference fun typedescrs))
        (setq then-typedescrs (select-then-type-descr fun typedescrs))
        (setq else-typedescrs (select-else-type-descr fun typedescrs))
        (setf (?then-type-descr-s test) then-typedescrs)
        (setf (?else-type-descr-s test) else-typedescrs)
        (setf (?type-descr-s test) typedescrs)
        ;; (propagate-type-descrs typedescrs test)
        (setf (?then-block test) (if then-typedescrs then-block ()))
        (setf (?else-block test) (if else-typedescrs else-block ()))
        (if (and then-typedescrs else-typedescrs)
            (progn
              ;; link variables
              (link-var-vec var-vec test 2)
              ;; add annotation to the called function
              (setf (?applications fun)
                    (cons test (?applications fun)))
              ;; add test to the function
              (setf (dynamic tests) (cons test (dynamic tests))))
          ())
        test)))
  )

(defmethod copy-stat ((stat <return>) last)
  (print "copy-stat return not yet implemented"))

(defmethod copy-stat ((stat <goto>) last)
  (print "copy-stat goto not yet implemented"))

(defmethod copy-stat ((stat <switch>) last)
  (print "copy-stat switch not yet implemented"))

(defmethod copy-stat ((stat <call>) last)
  (copy-stat1 stat 0 last))

(defmethod copy-stat ((stat <last-call>) last)
  (copy-stat1 stat 1 last))

(defmethod copy-stat ((stat <asm>) last)
  (copy-stat1 stat 2 last))

(defmethod copy-stat ((stat <last-asm>) last)
  (copy-stat1 stat 3 last ))

(defmethod copy-stat1 (stat type last)
  (let* ((arg-num (?arg-num stat))
         (fun (?function stat))
         (var-vec (make-vector (+ arg-num 1)))
         (var-descr (make <var-descr>
                          :var-vec var-vec
                          :constant-counter 0))
         (newcall (make (if (eq type 0) <call>
                          (if (eq type 1)
                              (if last <last-call> <call>)
                            (if (eq type 2) <asm>
                              (if last <last-asm> <asm>))))
                        ;; removed last-call and last asm
                        :function fun
                        :type-descr (general-var-actual-descr arg-num)
                        :arg-num (?arg-num stat)
                        :var-descr var-descr
                        :read-glocs (?read-glocs stat)
                        :write-glocs (?write-glocs stat))))
    (if (or (eq type 3) (eq type 1))
        (progn (dynamic-setq
                ienv
                (cons (cons (vector-ref (?var-vec (?var-descr stat)) 0)
                            (dynamic iresult)) (dynamic ienv))))
      ())
    (copy-vars (?arg-num stat) (?var-vec (?var-descr stat))
               var-descr newcall)
    (let ((typedescrs
           (make-actual-type-descr (dynamic typepathes)
                                   newcall
                                   (?var-descr newcall)
                                   arg-num
                                   ())))
      ;; add annotations to called function
      (setf (?applications fun)
            (cons newcall (?applications fun)))
      ;; make a type - inference
      (setq typedescrs
            (inference fun typedescrs))
      (if (and (generic-fun? fun) *actual-method-subset*
               (null? (cdr *actual-method-subset*))) ; only one method
          (progn
            (format t "m")
            (setq fun (?fun (car *actual-method-subset*)))
            (setf (?function newcall) fun)
            )
        ())
      (if (null? typedescrs) (error "Error: type clash")
        (progn
          ;; add the type-descriptors
          (setf (?type-descr-s newcall) typedescrs)
          (setf (dynamic typepathes) typedescrs))
        )
      (link-var-vec var-vec newcall arg-num)
      (setf (?block newcall) (dynamic block))
      (setf (dynamic calls)
            (cons newcall (dynamic calls)))
      newcall))
  )

(defun copy-vars (upto from to stat)
  (copy-vars1 0 upto from to stat))

(defun copy-vars1 (from to from-vec to-descr stat)
  (if (> from to) ()
    (let* ((oldvar (vector-ref from-vec from))
           (ass (assoc oldvar (dynamic ienv))))
      (if ass
          (handle-new-var (cdr ass) from to-descr stat)
        (copy-old-var oldvar from to-descr stat))
      (copy-vars1 (+ from 1) to from-vec to-descr stat)))
  )

(defgeneric handle-new-var (var-or-con to to-descr stat))

(defmethod handle-new-var ((var <local-static>) to to-descr stat)
  ;;  (setf (?link var) (cons (cons stat to) (?link var)))
  (setf (vector-ref (?var-vec to-descr) to) var)
  )

(defmethod handle-new-var ((var <tempvar>) to to-descr stat)
  ;;  (setf (?link var) (cons (cons stat to) (?link var)))
  (setf (vector-ref (?var-vec to-descr) to) var)
  )

(defmethod handle-new-var (con to to-descr stat)
  (setf (vector-ref (?var-vec to-descr) to) con)
  (if (simple-constant? con)
      (setf (?constant-counter to-descr)
            (+ (?constant-counter to-descr) 1))
    ())
  )

(defgeneric copy-old-var (var to to-descr stat))

(defmethod copy-old-var ((var <local-static>) to to-descr stat)
  (let ((newvar (make <local-static> :identifier (?identifier var)
                      :link ()
                      ;;     (list (cons stat to))
                      ))
        )
    (dynamic-setq ienv (cons (cons var newvar) (dynamic ienv)))
    (setf (vector-ref (?var-vec to-descr) to) newvar))
  )

(defmethod copy-old-var ((var <tempvar>) to to-descr stat)
  (let ((newvar (make <tempvar> :tnr (a-number)
                      :link ()
                      ;;(list (cons stat to))
                      )))
    (dynamic-setq ienv (cons (cons var newvar) (dynamic ienv)))
    (setf (vector-ref (?var-vec to-descr) to) newvar))
  )

(defmethod copy-old-var (con to to-descr stat)
  (setf (vector-ref (?var-vec to-descr) to) con)
  (if (simple-constant? con)
      (setf (?constant-counter to-descr)
            (+ (?constant-counter to-descr) 1))
    ())
  )

#module-end
