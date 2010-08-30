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

#module if-form
(import
 ((except (format) eulisp1)
  SIMPLE-PROGRAMMING
  LZS
  MZS
  context
  analyse-h ; make-vector and vector-ref
  type-propagation
  progn-context
  ;;  void-context
  vector
  lzs-to-mzs-fun
  debugging
  name-of-fun
  ;;  function-call-context
  (only (append caar assoc cdar caar format error) common-lisp)
  type-inference)
 ;; typeinfernce

 syntax
 (eulisp1)

 export
 (if-form-a) ; label if-form -> tempvar
 )



(defun if-form-a (con form)
  (let* ((t-block (make <block>))
         (e-block (make <block>))
         (test (make <test> :then-block t-block
                     :else-block e-block
                     :then-type-descr-s ()
                     :else-type-descr-s ()))
         (then-block ())
         (else-block ()))

    (l2m-a test (?pred form))
    ;; test inline-divided !!!
    (let ((then-typedescrs ())
          (else-typedescrs ())
          (type-descr-s ()))
      (if (?divided test)
          (progn (setq then-typedescrs (?then-type-descr-s test))
                 (setq else-typedescrs (?else-type-descr-s test))
                 (setq type-descr-s
                       (append then-typedescrs else-typedescrs)))
        (if (?function test)
            (let* ((t-pathes (dynamic typepathes))
                   (act-td (make-actual-type-descr
                            t-pathes test (?var-descr test)
                            2 ()))
                   )
              ;; debug
              ;;          (common-lisp::format t " td  V O R  dem Inferenzschritt")
              ;;          (ti-write::ti-print act-td)
              ;; end debug
              ;; make a type - inference
              (setq type-descr-s (inference (?function test)
                                            act-td))
              ;; debug
              ;;          (common-lisp::format t " td  N A C H  dem Inferenzschritt")
              ;;          (ti-write::ti-print type-descr-s)
              ;; end debug
              (setq then-typedescrs (select-then-type-descr
                                     (?function test)
                                     type-descr-s))
              ;; debug
              ;;          (common-lisp::format t " then-typedescrs")
              ;;          (ti-write::ti-print then-typedescrs)
              ;; end debug
              (setq else-typedescrs (select-else-type-descr
                                     (?function test)
                                     type-descr-s))
              ;; debung
              ;;          (common-lisp::format t " else-typedescrs")
              ;;          (ti-write::ti-print else-typedescrs)
              ;;end debung
              ;; (propagate-type-descrs typedescrs test)
              (setf (?in-label t-block) test)
              (setf (?in-label e-block) test)
              )
          (let ((t-path (dynamic typepathes)))
            (setq then-typedescrs  t-path)
            (setq else-typedescrs t-path))))
      (setq then-block (if then-typedescrs (?then-block test) ()))
      (setq else-block (if else-typedescrs (?else-block test) ()))
      ;;      ; !!! debug
      ;;      (common-lisp::format t "~% if-form: then ~s else ~s" then-block else-block)
      ;;      ; !!! debug
      (if (and then-block else-block)
          (let* ((env (dynamic env))
                 (closure (dynamic closure))
                 (pathes (dynamic pathes))
                 (then-pathes (cons-block then-block pathes))
                 (else-pathes (cons-block else-block pathes))
                 )
            ;; add test to the function
            (setf (dynamic tests) (cons test (dynamic tests)))
            ;; add the type-descriptors
            (setf (?type-descr test) (general-var-actual-descr 2))
            (setf (?type-descr-s test) type-descr-s)
            (setf (?then-type-descr-s test) then-typedescrs)
            (setf (?else-type-descr-s test) else-typedescrs)
            (if (null? (?divided test))
                (progn
                  ;; link variables
                  (link-var-vec (?var-vec (?var-descr test)) test 2)
                  ;; add annotation to the called function
                  (setf (?applications (?function test))
                        (cons test (?applications (?function test))))
                  ;; test is the result of the last block
                  (setf (?result (dynamic block)) test)
                  (setf (?block test) (dynamic block)))
              ())
            (let ((new-con (if (or (function-label-p con)
                                   (join-label-p con)) con
                             (make <join-label>
                                   :context con
                                   :env-level env
                                   :in-block ())))
                  then-result else-result)
              (dynamic-let ((block then-block)
                            (pathes then-pathes)
                            (typepathes then-typedescrs))
                           (setq then-result
                                 (l2m-a new-con
                                        (if (eq then-block t-block )
                                            (?then form)
                                          (?else form))))
                           ;; !!! result
                           ;;                       (setf (?result (dynamic block))
                           ;;                             then-result)
                           ;; *hf* 13.09
                           (if (?t-path (dynamic block))
                               ()
                             (progn
                               (setf (?t-path (dynamic block))
                                     (dynamic typepathes))
                               (if (label-p new-con)
                                   (let ((bl (dynamic block)))
                                     (setf (?env bl)
                                           (dynamic env))
                                     (setf (?in-block new-con)
                                           (cons bl
                                                 (?in-block new-con)))
                                     (setf (?out-label bl)
                                           new-con)
                                     ;;                               (if (and (join-label-p new-con)
                                     ;;                                        (null? (eq (?context new-con)
                                     ;;                                                  (dynamic *void-context*))
                                     (setf (?result bl) then-result))
                                 () )))
                           ;; *hf* 13.09 () )
                           )
              (dynamic-setq block else-block)
              (dynamic-let (; (block else-block)
                            (pathes else-pathes)
                            (typepathes else-typedescrs)
                            (env env)
                            (closure closure))
                           (setq else-result
                                 (if (eq else-block e-block)
                                     (l2m-a new-con (?else form))
                                   (l2m-a new-con (?then form))))
                           ;; !!! result
                           ;;                     (setf (?result (dynamic block))
                           ;;                             else-result)
                           ;; *hf* 13.09
                           (if (?t-path (dynamic block))
                               ()
                             (let ((bl (dynamic block)))
                               (setf (?t-path bl)
                                     (dynamic typepathes))
                               (if (label-p new-con)
                                   (progn
                                     (setf (?env bl)
                                           (dynamic env))
                                     (setf (?in-block new-con)
                                           (cons bl
                                                 (?in-block new-con)))
                                     (setf (?out-label bl)
                                           new-con)
                                     (setf (?result bl) else-result))
                                 ())))
                           ;; *hf* 13.09 ())
                           )
              (if (eq new-con con) ()
                ;; copy of this code in inline.em
                (let ((nenv (join-variables (?in-block new-con)
                                            (?env-level new-con)))
                      (ntypepathes (append-tpathes (?in-block new-con)))
                      (nblock (make <block> :in-label new-con))
                      (result (dynamic *void-context*)))
                  (setf (?out-block new-con) nblock)
                  (if (eq con (dynamic *void-context*))
                      (kill-results (?in-block new-con))
                    (setq result  (join-results (?in-block new-con))))
                  ;; neuer block env etc.
                  (setf (dynamic env) nenv)
                  (setf (dynamic typepathes) ntypepathes)
                  (setf (dynamic block) nblock)
                  result))
              ))
        (if then-block
            (if (eq then-block t-block )
                (progn
                  (information "ELSE-path removed")
                  (l2m-a con (?then form)))
              (progn
                (information "THEN-path removed")
                (l2m-a con (?else form))))
          (if else-block
              (if (eq else-block e-block)
                  (progn
                    (information "THEN-path removed")
                    (l2m-a con (?else form)))
                (progn
                  (information "THEN-path removed")
                  (l2m-a con (?then form))))
            (error "empty-test"))))))
  )

(defun information (txt)
  (let ((fun (analysed-fun)))
    (format t "~% ---------------- information ----------------------")
    (format t "~% in ~a function ~a: ~a"
            (funtype-of fun) (name-of fun) txt)
    (format t "~% ---------------------------------------------------~%"))
  )

(defun join-variables (blocks env-level)
  (if (null? blocks) env-level
    (let ((block (car blocks)))
      (join-variables1 block (?env block) env-level)
      (join-variables (cdr blocks) env-level)))
  )

(defun join-variables1 (block block-env env-level)
  (if (eq block-env env-level) ()
    (let* ((var (car (car block-env)))
           (fromvar (cdr (car block-env)))
           (oldvar (assoc var env-level))
           (td (empty-actual-descr 1)))
      (if oldvar
          (let* ((tovar (cdr oldvar))
                 (varvec (make-vector 2))
                 (move (make <move>
                             :arg-num 2
                             :var-descr (make <var-descr>
                                              :var-vec varvec
                                              :constant-counter 0)
                             :block block
                             ::type-descr td))
                 (type-descr-s (make-move-tds (?t-path block)
                                              fromvar move)))
            (setf (?type-descr-s move) type-descr-s)
            (setf (dynamic moves) (cons move (dynamic moves)))
            (setf (?interface block)
                  (cons move
                        (?interface block)))
            ;; fill the var-vec
            (setf (vector-ref varvec 0) tovar)
            (setf (vector-ref varvec 1) fromvar)
            ;; link the variables
            (setf (?link tovar)
                  (cons (cons move 0) (?link tovar)))
            (setf (?link fromvar)
                  (cons (cons move 1) (?link fromvar)))
            )
        ())
      (join-variables1 block (cdr block-env) env-level)))
  )




;;(defun join-variables (blocks env-level)
;; the result is the new env
;;  (let ((join-vars (join-vars1 blocks env-level env-level)))
;;    (if (eq join-vars env-level) env-level
;;        (add-interface blocks join-vars env-level))
;;    ))
;;
;;(defun join-vars1 (blocks env-level jvars)
;;  (if (null? blocks) jvars
;;      (let ((benv (?env (car blocks))))
;;        (if (eq benv env-level)
;;          (join-vars1 (cdr blocks) env-level jvars)
;;          (join-vars1 (cdr blocks) env-level
;;                      (join-vars2 benv env-level jvars))))))
;;
;;(defun join-vars2 (benv envl jvars)
;;  (if (eq benv envl) jvars
;;      (let ((var (caar benv)))
;;        (if (member-var var jvars envl)
;;          (join-vars2 (cdr benv) envl jvars)
;;          (join-vars2 (cdr benv) envl
;;                      (cons (cons var
;;                                  (make <tempvar>
;;                                        :tnr (a-number)))
;;                                  jvars))))))
;;
;;(defun member-var (var jvars envl)
;;  (cond ((eq jvars envl) nil)
;;        ((eq var (caar jvars)) t)
;;        (t (member-var var (cdr jvars) envl))))
;;
;;(defun add-interface (bls j-vars envl)
;;  (if (null? bls) j-vars
;;      (let ((bl (car bls)))
;;        (add-interface1 bl (?env bl) j-vars envl)
;;        (add-interface (cdr bls) j-vars envl))))
;;
;;(defun add-interface1 (bl env-bl jvars envl)
;;  (if (eq jvars envl) ()
;;      (let* ((nvar (cdar jvars))
;;             (ovar (cdr (assoc (caar jvars) env-bl)))
;;             (varvec (make-vector 2))
;;             (move (make <move>
;;                         :arg-num 2
;;                         :var-descr (make <var-descr>
;;                                          :var-vec varvec
;;                                          :constant-counter 0)
;;                         :block bl)))
;;        (setf (dynamic moves) (cons move (dynamic moves)))
;;        (setf (?interface bl)
;;              (cons move
;;                    (?interface bl)))
;; fill the var-vec
;;        (setf (vector-ref varvec 0) nvar)
;;        (setf (vector-ref varvec 1) ovar)
;; link the variables
;;        (setf (?link nvar)
;;              (cons (cons move 0) (?link nvar)))
;;        (setf (?link ovar)
;;              (cons (cons move 1) (?link ovar)))
;;        (add-interface1 bl env-bl (cdr jvars) envl))))

(defun append-tpathes (blocks)
  (if (cdr blocks) (append (?t-path (car blocks))
                           (append-tpathes (cdr blocks)))
    (?t-path (car blocks))))

(defun kill-results (bls)
  (cond ((null? bls) ())
        (t (setf (?result (car bls)) (dynamic *void-context*))
           (kill-results (cdr bls)))))

(defun join-results (blocks)
  (let ((jvar (make <tempvar> :tnr (a-number))))
    (join-results1 jvar blocks)
    jvar))

(defun join-results1 (jvar blocks)
  (if blocks
      (let* ((bl (car blocks))
             (td (empty-actual-descr 1))
             (varvec (make-vector 2))
             (vardescr (make <var-descr>
                             :var-vec varvec
                             :constant-counter 0))
             (move (make <move> :arg-num 2
                         :var-descr vardescr
                         :block bl
                         :type-descr td))
             (q (?result bl))
             (type-descr-s (make-move-tds (?t-path bl) q move)) )
        (setf (?type-descr-s move) type-descr-s)
        (setf (dynamic moves) (cons move (dynamic moves)))
        (setf (vector-ref varvec 0) jvar)
        (setf (vector-ref varvec 1) q)
        (setf (?result bl) jvar)
        (setf (?interface bl)
              (append-stat (?interface bl) move))
        (setf (?result bl) (dynamic *void-context*))
        ;; link the variables
        (setf (?link jvar)
              (cons (cons move 0) (?link jvar)))
        (if (or (local-static-p q)
                (tempvar-p q))
            (setf (?link q)
                  (cons (cons move 1) (?link q)))
          ())
        (join-results1 jvar (cdr blocks)))
    ()))

#module-end
