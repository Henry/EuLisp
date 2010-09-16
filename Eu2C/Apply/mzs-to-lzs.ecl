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
;;; Title:
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;    Bekannter Fehler: (setq a (foo ..)) -> (move tempvar tempvar)
;;    (in stat2lzs) (move tempvar (foo ..)) noch korrekt
;;    dann subst von tempvar !!!! Fehler
;;;  Requires:
;;;  Problems:
;;;  Authors: Horst Friedrich
;;;-----------------------------------------------------------------------------

#module mzs-to-lzs
(import (LZS
         MZS
         ;;  LZS-mop ; ~class-of
         lzs-modules
         null
         (only (cons?)
               list)
         accessors
         vector
         simple-programming ; only for the hack !!!
         analyse-h
         tail-module ; %unsigned-word-integer ...
         ti-signature ; convert-to-sys-type-vec
         expand-literal
         lzs-mop
         apply-funs
         (only (error
                car
                cdr
                cadr
                cdar
                cons
                append
                reverse
                member
                format  ; only for the hack !!!
                )
               common-lisp)
         ;;  code-print
         ;;  code-debug
         debugging
         name-of-fun
         ;;  codegen-data ; get-structure-offset (possible: get-instance-size), size-of
         )
 syntax (level-1)
 export (mzs2lzs-4-modules
         *assembler-code-generated*))

(defglobal *assembler-code-generated* ())

(deflocal inputfoo ())

(defun mzs2lzs-4-modules (m-list)
  (setq inputfoo (find-lexical ^read  ^read))
  (mzs2lzs-4-modules1 m-list))

(defun mzs2lzs-4-modules1 (m-list)
  ;;  (mzs2lzs-fun (?toplevel-forms (car m-list)))
  ;;  (mzs2lzs-functions (?fun-list (car m-list))) ; exported-funs
  ;;  (mzs2lzs-functions (get-functions-used-in-literals)))

  (if (null? m-list) (mzs2lzs-functions (get-functions-used-in-literals))
    (progn ;(pause-mzs2lzs)
      (mzs2lzs-4-module (car m-list))
      (mzs2lzs-4-modules1 (cdr m-list)))))

(defun mzs2lzs-4-module (modul)
  (let ((tlf (?toplevel-forms modul)))
    (if tlf (mzs2lzs-fun tlf) ())
    (mzs2lzs-functions (?fun-list modul)))
  )

(defun mzs2lzs-functions (fun-list)
  (if (null? fun-list) ()
    (progn (mzs2lzs-fun (car fun-list))
           (mzs2lzs-functions (cdr fun-list))))
  )
;; --------- dynamic-variables -------------
(defglobal *var-list* ())
(defglobal *init-list* ())
(defglobal *type-list* ())
(defglobal *tagbody-form* ())

(defun mzs2lzs-fun (fun)
  (start-analyse-fun fun) ; for debugging
  (if (or (eq (?pass fun) 5)
          (eq (?pass fun) 3))
      ;;    (and  (or (global-fun? fun) (local-fun? fun))
      ;;           (?params fun))
      (let* ((var-vec (?var-vec (?var-descr fun)))
             (type-vec (?type-descr fun))
             (arg-num (?arg-num fun))
             )
        (setf (?pass fun) 100)
        (if (< arg-num 0) (setq arg-num (- 0 arg-num)) ())
        (setf (?params fun)
              (var-vec2params var-vec arg-num))
        (dynamic-let
         ((*currend-function* fun)
          (*currend-statement* ())
          (*var-list* ())
          (*init-list* ())
          (*type-list* ())
          (*tagbody-form* ()))
         (setf (?body fun)
               (form-list2form
                (check-arg-types var-vec type-vec 1 arg-num fun
                                 (block2lzs (?start-block
                                             (?function-label fun)) () ))))))
    ())
  (end-analyse-fun fun) ; for debugging
  )

(defun check-arg-types (var-vec type-vec idx num fun lst)
  (if inputfoo (check-arg-types1 var-vec type-vec idx num fun lst)
    lst))

(deflocal max-depth 4)

(defun call-path-to-input (fun idx depth funs vars)
  (if (> depth max-depth) ()
    (call-path-to-input1 (?applications fun) idx depth funs vars)))

(defun call-path-to-input1 (appl idx depth funs vars)
  (if appl
      (if (stat-path-to-input (car appl) idx (+ depth 1) funs vars)
          t
        (call-path-to-input1 (cdr appl) idx depth funs vars))
    ()))

(defgeneric stat-path-to-input (stat idx depth funs vars))

(defmethod stat-path-to-input ((stat <function-statement>) idx depth funs vars)
  (var?ath-to-input (vector-ref (?var-vec (?var-descr stat)) idx)
                     depth funs vars))

(defmethod stat-path-to-input (stat idx depth funs vars) ())

(defgeneric var?ath-to-input (var depth funs vars))

(defmethod var?ath-to-input ((var <tempvar>) depth funs vars)
  (if (member var vars) ()
    (links-path-to-input (?link var) depth funs (cons var vars))))

(defmethod var?ath-to-input ((var <local-static>) depth funs vars)
  (if (member var vars) ()
    (links-path-to-input (?link var) depth funs (cons var vars))))

(defmethod var?ath-to-input ((app <app>) depth funs vars)
  (result-path-to-input-fun (?function app) depth funs vars))

(defmethod var?ath-to-input ((ref <var-ref>) depth funs vars)
  (var?ath-to-input (?var ref) depth funs vars))


(defmethod var?ath-to-input (var depth funs vars) ())

(defun links-path-to-input (links depth funs vars)
  (if links
      (let ((l (car links)))
        (if (eq (cdr l) 0)
            (if (result-path-to-input (car l) depth funs vars)
                t
              (links-path-to-input (cdr links) depth funs vars))
          (if (defined-fun? (car l))
              (call-path-to-input (car l) (cdr l) depth funs vars)
            (links-path-to-input (cdr links) depth funs vars))))
    ()))

(defgeneric result-path-to-input (stat depth funs vars))

(defmethod result-path-to-input ((stat <move>) depth funs vars)
  (var?ath-to-input (vector-ref (?var-vec (?var-descr stat)) 1)
                     depth funs vars))

(defmethod result-path-to-input ((stat <call>) depth funs vars)
  (result-path-to-input-fun (?function stat) depth funs vars))

(defmethod result-path-to-input ((stat <last-call>) depth funs vars)
  (result-path-to-input-fun (?function stat) depth funs vars))

(defmethod result-path-to-input (stat depth funs vars) ())

(defun result-path-to-input-fun (fun depth funs vars)
  (if (eq fun inputfoo) t
    (if (member fun funs) ()
      (if (> depth max-depth) ()
        (if (defined-generic-fun? fun)
            (var?ath-to-input
             (vector-ref (?var-vec
                          (?var-descr (?discriminating-fun fun))) 0)
             (+ depth 1) (cons fun funs) vars)
          (if (defined-fun? fun)
              (var?ath-to-input
               (vector-ref (?var-vec (?var-descr fun)) 0)
               (+ depth 1) (cons fun funs) vars)
            ())))))
  )

(defun check-arg-types1 (var-vec type-vec idx num fun lst)
  (if (> idx num) lst
    (let* ((funtype (vector-ref type-vec idx))
           (vartype
            (if (and (eq funtype %object)
                     (call-path-to-input fun idx 0 (list fun) ()))
                (collect-var-types (?link (vector-ref var-vec idx)))
              ())))
      (if vartype
          (if (null? (cdr vartype))
              (if (or (eq funtype (car vartype))
                      (eq (car vartype) %void))
                  (setq vartype ())
                () ; moeglicherweise muss die Klassenmenge eingeschr. werden
                )))
      (if vartype
          (let ((vec (make-vector 3)))
            (setf (vector-ref vec 0) funtype)
            (setf (vector-ref vec 1) funtype)
            (setf (vector-ref vec 2) %object)
            (format t "t")
            ;; (typecheck-break funtype vartype)
            (cons
             (make <app> :function typecheck
                   ;;(make <global-fun> :identifier 'typecheck)
                   :arg-list (list (make <var-ref>
                                         :var (vector-ref var-vec idx))
                                   (expand-literal vartype))
                   :type-descr vec)
             (check-arg-types1 var-vec type-vec (+ idx 1) num fun lst)))
        (check-arg-types1 var-vec type-vec (+ idx 1) num fun lst)))))

(defun var-vec2params (var-vec arg-num)
  (make <params>
        :allow-other-keys ()
        :key-list ()
        :rest ()
        :opt-list ()
        :var-list (var-vec2var-list var-vec 1 arg-num)
        :source ()))

(defun var-vec2var-list (vec from to)
  (if (<= from to)
      (let* ((var (vector-ref vec from))
             (link (?link var))
             (local-static ;(if (local-static? var) ()
              (mk-local-static1 link var)))
        ;;)
        (if local-static (setq var local-static) ())
        (setq local-static (make <var-ref> :var var))
        (replace-var link local-static)
        (cons var
              (var-vec2var-list vec (+ from 1) to)))
    ()))

(defun form-list2form (form-list)
  (if (dynamic *var-list*)
      (make <let*-form>
            :var-list (reverse (dynamic *var-list*))
            :init-list (reverse (dynamic *init-list*))
            :type-list (reverse (dynamic *type-list*))
            :body (if (dynamic *tagbody-form*)
                      (let ((tagbody-form (dynamic *tagbody-form*)))
                        (setf (?first-form tagbody-form)
                              (collect-first-forms form-list))
                        (setf (?tagged-form-list tagbody-form)
                              (delete-first-forms form-list))
                        tagbody-form)
                    (if form-list
                        (if (cdr form-list)
                            (make <progn-form>
                                  :form-list form-list)
                          (car form-list))
                      () )))
    (if (dynamic *tagbody-form*)
        (let ((tagbody-form (dynamic *tagbody-form*)))
          (setf (?first-form tagbody-form)
                (collect-first-forms form-list))
          (setf (?tagged-form-list tagbody-form)
                (delete-first-forms form-list))
          tagbody-form)
      (if form-list
          (if (cdr form-list)
              (make <progn-form> :form-list form-list)
            (car form-list))
        () )))
  )

(defun form-list2simple-form (form-list)
  (if form-list
      (if (cdr form-list)
          (make <progn-form> :form-list form-list)
        (car form-list))
    () )
  )

(defun collect-first-forms (form-list)
  (let ((first-forms (collect-first-forms1 form-list)))
    (if first-forms
        (if (cdr first-forms) (make <progn-form> :form-list first-forms)
          (car first-forms))
      ()))
  )

(defun collect-first-forms1 (form-list)
  (if form-list
      (if (tagged-form? (car form-list)) ()
        (cons (car form-list)
              (collect-first-forms1 (cdr form-list))))
    () )
  )

(defun delete-first-forms (form-list)
  (if form-list
      (if (tagged-form? (car form-list)) form-list
        (delete-first-forms (cdr form-list)))
    () )
  )

;; labels = ((<mzs-label> . <tagged-form>) ...)

(defun block2lzs (block labels)
  (let* ((label (find-label (?in-label block) labels))
         (stat-list (stats2lzs (?body block)))
         (interface-list (stats2lzs (?interface block)))
         (result-list (result2lzs (?result block) (?out-label block) labels))
         ;; (mzs-label (?out-label block))
         (form-list (append-lists stat-list interface-list result-list))
         )
    (if label ; label is  a tagged-form
        (progn (setf (?form label)
                     (form-list2simple-form form-list))
               (list label))
      ;;      (let ((first-form (collect-first-forms form-list))
      ;;            (rest-forms (delete-first-forms form-list)))
      ;;        (setf (?form label) first-form)
      ;;        (cons label rest-forms))
      form-list)))

(defun find-label (label labels)
  (if (null? labels) ()
    (if (eq (car (car labels)) label)
        (cdr (car labels))
      (find-label label (cdr labels))))
  )

(defun append-lists (a b c)
  (if c (if b (append a (append b c))
          (append a c))
    (if b (append a b) a)))

(defun stats2lzs (stat-list)
  (if (null? stat-list) ()
    (let ((stat (stat2lzs (car stat-list))))
      (if stat (cons stat (stats2lzs (cdr stat-list)))
        (stats2lzs (cdr stat-list)))))
  )

(defgeneric stat2lzs (stat))

(defmethod stat2lzs ((stat <set-slot-value>))
  (let ((var-vec (?var-vec (?var-descr stat))))
    (setf (?value stat) (vector-ref var-vec 2))
    (slot-value2lzs stat var-vec))
  )

(defmethod stat2lzs ((stat <get-slot-value>))
  (slot-value2lzs stat (?var-vec (?var-descr stat)))
  )

(defun slot-value2lzs (stat var-vec)
  (let* ((type-vec (?type-descr stat))
         (block (?block stat))
         (var (vector-ref var-vec 0))
         (link (?link var)))
    ;; build a correkt statement
    (setf (?instance stat) (vector-ref var-vec 1))
    (if (null? (cdr link)) stat ; result never used
      (if (and (tempvar? var)
               (null? (cdr (cdr link))))
          ;; result only once used
          (let* ((used-stat (if (eq (cdr (car link)) 0)
                                (car (cdr link))
                              (car link)))
                 (where-used (cdr used-stat)))
            (setq used-stat (car used-stat))
            (if (and (eq block (?block used-stat))
                     (null? (return? used-stat)))
                (progn
                  (setf (vector-ref (?var-vec (?var-descr used-stat))
                                    where-used) stat)
                  ())
              (if (setq var (add-let-variable var (vector-ref type-vec 0)
                                              stat link))
                  (let ((ntype-vec (make-vector 2)))
                    (setf (vector-ref ntype-vec 0)
                          (vector-ref type-vec 0))
                    (setf (vector-ref ntype-vec 1)
                          (vector-ref type-vec 0))
                    (make <setq-form>
                          :location var :form stat
                          :type-descr ntype-vec))
                ())))
        ;; result more than once used
        (if (setq var (add-let-variable var (vector-ref type-vec 0)
                                        stat link))
            (let ((ntype-vec (make-vector 2)))
              (setf (vector-ref ntype-vec 0)
                    (vector-ref type-vec 0))
              (setf (vector-ref ntype-vec 1)
                    (vector-ref type-vec 0))
              (make <setq-form>
                    :location var :form stat
                    :type-descr ntype-vec))
          ())))))



(defmethod stat2lzs ((stat <call>)) (call2lzs stat))
(defmethod stat2lzs ((stat <asm>)) (call2lzs stat))


(defun typecheck-break (t1 t2)
  ;;  (if (eq t1 t2) ()
  ;;    (format t "~% Typpruefung fuer ~s -> ~s ~%" (?identifier t1)
  ;;           (?identifier (car t2)))))
  ())

(defun collect-var-types (links)
  (collect-var-types1 links 0 0 () ()))

(defun collect-var-types1 (links obj sonst instl tl)
  (if (null? links)
      (if (>= obj sonst) ()
        (if tl (append instl tl) ()))
    (let ((link (car links)))
      (if (or (eq (cdr link) 0)
              (return? (car link))
              (goto? (car link)))
          (collect-var-types1 (cdr links) obj sonst instl tl)
        (if (function-call? (car link))
            (let ((fun (?function (car link))))
              (if (eq fun %class-of) ()
                (if (eq fun %instance-of?)
                    (let ((itype (vector-ref (?var-vec (?var-descr (car link))) 2)))
                      (if (member itype instl)
                          (collect-var-types1 (cdr links) obj sonst instl tl)
                        (collect-var-types1 (cdr links) obj sonst
                                            (cons itype instl)
                                            (del-type itype tl))))

                  (let ((type (vector-ref (?type-descr (car link))
                                          (cdr link))))
                    (if (eq type %object)
                        (collect-var-types1 (cdr links) (+ obj 1) sonst instl tl)
                      (if (or (member type tl) (is-supertype type instl))
                          (collect-var-types1 (cdr links) obj (+ sonst 1)
                                              instl tl)
                        (collect-var-types1 (cdr links) obj (+ sonst 1)
                                            instl (cons type tl))))))))
          (let ((type (vector-ref (?type-descr (car link))
                                  (cdr link))))
            (if (eq type %object)
                (collect-var-types1 (cdr links) (+ obj 1) sonst instl tl)
              (if (or (member type tl) (is-supertype type instl))
                  (collect-var-types1 (cdr links) obj (+ sonst 1)
                                      instl tl)
                (collect-var-types1 (cdr links) obj (+ sonst 1)
                                    instl (cons type tl)))))))))
  )

(defun is-supertype (ty tyl)
  (if tyl
      (if (eq ty (car tyl))
          t
        (if (member ty (~class-precedence-list (car tyl)))
            t
          (is-supertype ty (cdr tyl))))
    ()))

(defun del-type (ty l)
  (if l (if (eq (car l) ty) (del-type ty (cdr l))
          (cons (car l) (del-type ty (cdr l))))
    ()))

(defun call2lzs (stat)
  (let* ((var-vec (?var-vec (?var-descr stat)))
         (type-vec (?type-descr stat))
         (fun (?function stat))
         (app (make <app>
                    :function fun
                    :arg-list (var-vec2arg-list var-vec)
                    :type-descr type-vec))
         (block (?block stat))
         (var (vector-ref var-vec 0))
         (link (?link var)))
    ;; add typecheck
    (if (or (null? inputfoo)
            (null? (or (?type-descr fun)
                      (?range-and-domain fun)))) ()
      (let* ((funtype (vector-ref (if (?type-descr fun)
                                      (?type-descr fun)
                                    (?range-and-domain fun)) 0))
             (vartype
              (if (and (eq funtype %object)
                       (result-path-to-input stat 0 () ()))
                  (collect-var-types link) ())))
        (if vartype
            (if (null? (cdr vartype))
                (if (or (eq funtype (car vartype))
                        (eq (car vartype) %void))
                    (setq vartype ())
                  () ; moeglicherweise muss die Klassenmenge eingeschr. werden
                  )))
        (if vartype
            (let ((vec (make-vector 3)))
              (setf (vector-ref vec 0) funtype)
              (setf (vector-ref vec 1) funtype)
              (setf (vector-ref vec 2) %object)
              (format t "t")
              (typecheck-break funtype vartype)
              (setq app
                    (make <app> :function typecheck
                          ;; (make <global-fun> :identifier 'typecheck)
                          :arg-list (list app
                                          (expand-literal vartype))
                          :type-descr vec))))))
    (if (null? (cdr link)) app ; result never used
      (if (and (tempvar? var) (null? (cdr (cdr link))))
          ;; result only once used
          (let* ((used-stat (if (eq (cdr (car link)) 0)
                                (car (cdr link))
                              (car link)))
                 (where-used (cdr used-stat)))
            (setq used-stat (car used-stat))
            (if (and (eq block (?block used-stat))
                     (null? (return? used-stat)))
                (progn
                  (setf (vector-ref (?var-vec (?var-descr used-stat))
                                    where-used) app)
                  ())
              (if (setq var (add-let-variable var (vector-ref type-vec 0)
                                              app link))
                  (let ((ntype-vec (make-vector 2)))
                    (setf (vector-ref ntype-vec 0)
                          (vector-ref type-vec 0))
                    (setf (vector-ref ntype-vec 1)
                          (vector-ref type-vec 0))
                    (make <setq-form>
                          :location var :form app
                          :type-descr ntype-vec))
                ())))
        ;; result more than once used
        (if (setq var (add-let-variable var (vector-ref type-vec 0)
                                        app link))
            (let ((ntype-vec (make-vector 2)))
              (setf (vector-ref ntype-vec 0)
                    (vector-ref type-vec 0))
              (setf (vector-ref ntype-vec 1)
                    (vector-ref type-vec 0))
              (make <setq-form>
                    :location var :form app
                    :type-descr ntype-vec))
          () )))))

(defun add-let-variable (var type app link)
  ;; returned a var-ref, when the application is not used
  ;; in the let-init-forms !!!
  (let* ((local-static (mk-local-static var link))
         (var-ref (make <var-ref> :var local-static)))
    (dynamic-setq  *var-list*
                   (cons local-static (dynamic *var-list*)))
    (dynamic-setq *init-list*
                  (cons ^unknown (dynamic *init-list*)))
    (dynamic-setq *type-list*
                  (cons type (dynamic *type-list*)))
    (replace-var link var-ref)
    var-ref))

(defun mk-local-static (var link)
  (if (local-static? var) var
    (if (and (var-ref? var) (local-static? (?var var)))
        (?var var)
      (let ((local-static (mk-local-static2 link)))
        (make <local-static>
              :identifier (if local-static
                              (?identifier local-static) ()) ; !!! new name
              )))))

(defun mk-local-static2 (link)
  (if (null? link) ()
    (let ((stat (car (car link))))
      (if (and (move? stat) (eq (cdr (car link)) 1))
          (let ((local-static (vector-ref (?var-vec (?var-descr stat)) 0)))
            (if (local-static? local-static)
                local-static
              (if (and (var-ref? local-static)
                       (local-static? (?var local-static)))
                  (?var local-static)
                (mk-local-static2 (cdr link)))))
        (mk-local-static2 (cdr link)))))
  )

(defun mk-local-static1 (link var)
  (if (null? (cdr link))
      (let* ((fun (analysed-fun))
             (name (name-of fun))
             (printwarn (if (cons? name)
                            (if (or (eq (car name) ^method)
                                    (eq (car name) ^setter)
                                    (eq (car name) ^converter)) t ())
                          (if name t ()))))
        ;;       (if printwarn
        ;;           (progn
        ;;             (format t "~% Warning: in ~a function ~a:"
        ;;                     (funtype-of fun) name)
        ;;             (format t "~%          formal parameter ~a never used~%"
        ;;                     (?identifier var)))
        ;;  ())
        ())
    (if (cdr (cdr link)) ()
      (mk-local-static11 link))))

(defun mk-local-static11 (link)
  (if (null? link) ()
    (let ((stat (car (car link))))
      (if (and (move? stat) (eq (cdr (car link)) 1))
          (let ((local-static (vector-ref (?var-vec (?var-descr stat)) 0)))
            (if (local-static? local-static)
                (if (is-parameter (?link local-static)) () local-static)
              (mk-local-static11 (cdr link))
              ))
        (mk-local-static11 (cdr link))
        )))
  )

(defun is-parameter (link)
  (if (null? link) ()
    (if (fun? (car (car link))) t
      (is-parameter (cdr link)))))

(defun replace-var (link var)
  (if (null? link) ()
    (let ((stat (car (car link)))
          (where (cdr (car link))))
      (if (return? stat)
          (setf (?value stat) var)
        (if (or (goto? stat) (fun? stat)) ()
          (setf (vector-ref (?var-vec (?var-descr stat)) where) var)))
      (replace-var (cdr link) var)))
  )

(defmethod stat2lzs ((stat <last-call>)) (last-call2lzs stat))
(defmethod stat2lzs ((stat <last-asm>)) (last-call2lzs stat))

(defun last-call2lzs (stat)
  (let* ((var-vec (?var-vec (?var-descr stat)))
         (type-vec (?type-descr stat))
         (app (make <app>
                    :function (?function stat)
                    :arg-list (var-vec2arg-list var-vec)
                    :type-descr type-vec)))
    app)
  )

(defmethod stat2lzs ((stat <move>))
  ;; to global-static
  (let* ((var-vec (?var-vec (?var-descr stat)))
         (type-vec (?type-descr stat))
         (to-var-ref (vector-ref var-vec 0))
         (from (vector-ref var-vec 1))
         ;; (to-type (vector-ref type-vec 0))
         ;; (from-type (vector-ref type-vec 1))
         (to-var (if (var-ref? to-var-ref) (?var to-var-ref) to-var-ref))
         )
    (if (var-ref? to-var-ref)
        (if (and (var-ref? from) (eq (?var from) to-var)) ()
          (make <setq-form>
                :form (if (var? from)
                          (make <var-ref> :var from)
                        from)
                :location to-var-ref :type-descr type-vec))
      (if (local-static? to-var)
          (let ((link (?link to-var)))
            (if (and (var-ref? from)
                     (eq (?var from) to-var))
                (progn (replace-var link (make <var-ref> :var to-var))
                       ())
              (let ((var-ref (add-let-variable to-var
                                               (vector-ref type-vec 0)
                                               ^unknowm
                                               link)))
                (make <setq-form> :form (if (var? from)
                                            (make <var-ref> :var from)
                                          from)
                      :location var-ref :type-descr type-vec)
                )))
        (if (tempvar? to-var)
            (let* ((link (?link to-var))
                   (var-ref (add-let-variable to-var
                                              (vector-ref type-vec 0)
                                              ^unknowm
                                              link)))
              (make <setq-form> :form (if (var? from)
                                          (make <var-ref> :var from)
                                        from)
                    :location var-ref :type-descr type-vec))
          (make <setq-form> :form (if (var? from)
                                      (make <var-ref> :var from)
                                    from)
                :location (make <var-ref> :var to-var)
                :type-descr type-vec))))))

(defun var-vec2arg-list (vec)
  (var-vec2arg-list1  1 (length vec) vec))

(defun var-vec2arg-list1 (from to vec)
  (if (< from to)
      (cons (vector-ref vec from)
            (var-vec2arg-list1 (+ from 1) to vec))
    ()))

;; result2lzs (?result block) (?out-label block) labels
(defgeneric result2lzs (result out-label labels))

(defmethod result2lzs ((result <return>) out-label
                       labels)
  (let ((value (?value result)))
    (if (tempvar? value) () ; result of last-call
      (list value)))
  )

;; labels = ((<mzs-label> . <tagged-form>) ...)

(defmethod result2lzs ((result <void>) (out-label <zykl-label>) labels)
  (let ((tagbody (dynamic *tagbody-form*))
        (tagged (make <tagged-form>)))
    (if tagbody ()
      (dynamic-setq *tagbody-form*
                    (setq tagbody (make <tagbody-form>))))
    (setf (?tagbody tagged) tagbody)
    (block2lzs  (?out-block out-label)
                (cons (cons out-label tagged) labels)))
  )
;; doppelt gemoppelt
;;
;;    (let* ((form-list (block2lzs  (?out-block out-label)
;;                                 (cons (cons out-label tagged) labels)))
;;           (first-form (collect-first-forms form-list))
;;           (rest-forms (delete-first-forms form-list)))
;;      (setf (?form tagged)
;;            first-form)
;;      (cons tagged rest-forms)))
;;)

(defmethod result2lzs ((result <void>) out-label labels)
  () )

(defmethod result2lzs ((result <null>) out-label
                       labels)
  () )

(defmethod result2lzs ((result <goto>) out-label
                       labels)
  (let ((tagged (find-label (?label result) labels)))
    (list tagged))
  )

(defglobal *joind-labels* ())

(defmethod result2lzs ((test <test>) out-label
                       labels)
  (let* ((type-vec (?type-descr test))
         (var-vec (?var-vec (?var-descr test)))
         (if-form (make <if-form>
                        :pred (make <app>
                                    :function (?function test)
                                    :arg-list (var-vec2arg-list var-vec)
                                    :type-descr type-vec)
                        :then (form-list2simple-form
                               (block2lzs (?then-block test) labels))
                        :else (form-list2simple-form
                               (block2lzs (?else-block test) labels))
                        ))
         (join-label (find-join-label (?then-block test)
                                      (?else-block test))))
    (if join-label
        (cons if-form (block2lzs (?out-block join-label) labels))
      (list if-form)))
  )

(defun find-join-label (then-block else-block)
  (let* ((then (find-join-label1 then-block))
         (else (if then (find-join-label1 else-block) ())))
    (if else
        (if (and (eq (car then) (car else))
                 (eq (length (?in-block (car then)))
                     (+ (cdr then) (cdr else))))
            (car then)
          ())))
  )

(defun find-join-label1 (block)
  (let ((result (?result block))
        (out-label (?out-label block)))
    (if (join-label? out-label) (cons out-label 1)
      (if (test? result)
          (let* ((then (find-join-label1 (?then-block result)))
                 (else (if then (find-join-label1 (?else-block result)) ())))
            (if else
                (progn
                  (if (eq (car then) (car else)) ()
                    (error "~% differen then and else-blocks "))
                  (if (eq (length (?in-block (car then)))
                          (+ (cdr then) (cdr else)))
                      (find-join-label1 (?out-block (car then)))
                    (cons (car then) (+ (cdr then) (cdr else)))))
              ()))
        ())))
  )


#module-end
