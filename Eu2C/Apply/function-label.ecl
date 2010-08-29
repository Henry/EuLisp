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

#module function-label

(import
 (eulisp1
  SIMPLE-PROGRAMMING
  LZS
  MZS
  context
  analyse-h ; make-vector and vector-ref
  type-propagation
  progn-context
  vector
  lzs-to-mzs-fun
  if-form
  letstar-form
  setq-form
  function-call
  function-call-context
  slot-value
  (only (error append) common-lisp)
  gutter
  type-inference)
 ;; typeinfernce

 syntax
 (eulisp1)

 export
 (?arg-num-fun)
 )


;;---------------------------------

(defmethod finish-a ((con <function-label>) var-or-const)
  (cond ((or (tempvar-p var-or-const)
             (local-static-p var-or-const))
         (setf (?link var-or-const)
               (cons (cons (return-variable var-or-const con) 1) ;1-01.06 *hf*
                     (?link var-or-const)))) ; ! neu 02.06 *hf*
        ((var-p var-or-const) (return-variable var-or-const con))
        (t  (return-constant var-or-const con))))


(defun ?arg-num-fun (fun)
  (let ((arg-num (?arg-num fun)))
    (if (< arg-num 0) (- 0 arg-num) arg-num)))

;;;-----------------------------------------------------------------------------
;;; constants
;;;-----------------------------------------------------------------------------
(defun return-constant (con function-label)
  (let* ((curblock (dynamic block))
         (con-type (constant-type con))
         (return (make <return> :block curblock
                       :value con
                       :type con-type)))
    (setf (?result curblock) return)
    ;;    (setf (?env curblock) (dynamic env))
    (setf (?end-blocks function-label)
          (cons curblock (?end-blocks function-label)))
    (setf (?out-label curblock)
          function-label)
    ;; link the result-variable
    (link-result-variable
     function-label
     curblock return)
    ;; fill all type-descr-s
    (let ((fun (?function function-label)))
      (if (?range-and-domain fun)
          (let ((ftd (make-formal-type-descr
                      (dynamic typepathes)
                      return
                      con
                      con-type
                      (?var-descr fun)
                      (?arg-num-fun fun)
                      () ; old tds
                      () ; no recursive
                      )))
            (check-result-subtypes
             ftd
             (?type-descr fun))
            (setf (?type-descr-s fun)
                  (append ftd
                          (?type-descr-s fun))))
        (setf (?type-descr-s fun)
              (make-formal-type-descr (dynamic typepathes)
                                      return
                                      con
                                      con-type
                                      (?var-descr fun)
                                      (?arg-num-fun fun)
                                      (?type-descr-s fun)
                                      () ; no recursive
                                      )))
      )))

(defun link-result-variable (fun-label block statement)
  (let ((var (vector-ref (?var-vec (?var-descr (?function fun-label))) 0)))
    (setf (?link var)
          (cons (cons statement 0) (?link var)))))

;;;-----------------------------------------------------------------------------
;;; constants
;;;-----------------------------------------------------------------------------

(defmethod l2m-a ((con <function-label>) (form <named-const>))
  ;;
  ;; form = <defined-named-constant>, <imported-named-constant>
  ;;
  (return-constant form con)
  form)

(defmethod l2m-a ((con <function-label>) (form <sym>))
  ;;
  ;; form = <defined-symbol>, <imported-symbol>
  ;;
  (return-constant form con)
  form)

;;  symbol ist nur ein slot-name, und duerfte in einer normalen funktion nicht auftreten
;;(defmethod l2m-a ((con <function-label>) (form <symbol>))
;;
;; form = <defined-symbol>, <imported-symbol>
;;
;;   (return-constant form con)
;;   form)

(defmethod l2m-a ((con <function-label>) (form <structured-literal>))
  ;;
  ;; value = <vector>, <pair>, <string>, LITERAL-INSTANCE
  ;;
  (return-constant form con)
  form)

(defmethod l2m-a ((con <function-label>) (form <fpi>))
  ;;
  ;; <spint> - single precisition integer
  ;;
  (return-constant form con)
  form)

(defmethod l2m-a ((con <function-label>) (form <double-float>))
  (return-constant form con)
  form)

(defmethod l2m-a ((con <function-label>) (form <character>))
  (return-constant form con)
  form)

(defmethod l2m-a ((con <function-label>) (form <class-def>))
  ;;
  ;;  form = <defined-class>, <imported-class>
  ;;
  (return-constant form con)
  form)

(defmethod l2m-a ((con <function-label>) (form <literal-instance>))
  (return-constant form con)
  form)

(defmethod l2m-a ((con <function-label>) (form <global-fun>))
  (if (eq (?function-type form) $normal)
      (setf (?function-type form) $data)
    ())
  (return-constant form con)
  form)

(defmethod l2m-a ((con <function-label>) (form <local-fun>))
  (lzs2mzs-fun form)
  (if (eq (?function-type form) $closure)
      (let ((var (make-a-closure-function form (dynamic block))))
        (return-variable var con)
        var)
    (progn (return-constant form con) form))
  )
(defmethod l2m-a ((con <function-label>) (form <imported-fun>))
  (if (eq (?function-type form) $normal)
      (setf (?function-type form) $data)
    ())
  (return-constant form con)
  form)

(defmethod l2m-a ((con <function-label>) (form <special-sys-fun>))
  (if (eq (?function-type form) $normal)
      (setf (?function-type form) $data)
    ())
  (return-constant form con)
  form)

(defmethod l2m-a ((con <function-label>) (form <global-generic-fun>))
  (if (eq (?function-type form) $normal)
      (setf (?function-type form) $data)
    ())
  (return-constant form con)
  form)

(defmethod l2m-a ((con <function-label>) (form <local-generic-fun>))
  (if (eq (?function-type form) $normal)
      (setf (?function-type form) $data)
    ())
  (return-constant form con)
  form)

(defmethod l2m-a ((con <function-label>) (form <imported-generic-fun>))
  (if (eq (?function-type form) $normal)
      (setf (?function-type form) $data)
    ())
  (return-constant form con) form)

(defmethod l2m-a ((con <function-label>) (form <cont>))
  (print "<function-label> <cont> not correct implemented")
  (return-constant form con) form)

(defmethod l2m-a ((con <function-label>) (form <null>))
  ;;  (print "<function-label> <null> not implemented")
  (return-constant form con) form)

;;;-----------------------------------------------------------------------------
;;; Variables
;;;-----------------------------------------------------------------------------

(defmethod l2m-a ((con <function-label>) (form <var-ref>))
  ;;
  ;; var = <local-static>, <global-static>, <imported-static>, <dynamic>
  ;;
  (let ((variable (?var form)))
    (cond ((or (local-static-p variable) (tempvar-p variable)) ; neu 26.05 *hf*
           (setq variable (rename variable))
           (setf (?link variable)
                 (cons (cons (return-variable variable con) 1) ;1-01.06 *hf*
                       (?link variable))) ; ! neu 25.05 *hf*
           variable)
          (t ; dynamic global-static imported-static
           (let ((statement (return-variable variable con))
                 ;; (fun (?function con))
                 )
             (if (?read-gloc form)
                 (setf (?read-glocs statement) (list (?read-gloc form)))
               ())
             (setf (?read-stats variable)
                   (cons (list statement 0 (general-type))
                         (?read-stats variable)))
             variable)))))

(defun return-variable (var fun-label)
  (let* ((curblock (dynamic block))
         (fun (?function fun-label))
         (pathes (dynamic pathes))
         (return (make <return> :block curblock
                       :value var
                       ;;  :function fun
                       :pathes pathes)))
    (setf (?result curblock) return)
    (setf (?end-blocks fun-label)
          (cons curblock (?end-blocks fun-label)))
    (setf (?out-label curblock)
          fun-label)
    ;; link the result-variable
    (link-result-variable fun-label curblock return)
    ;; fill the new type-descr-s
    (if (?range-and-domain fun)
        (let ((ftd (make-formal-type-descr
                    (dynamic typepathes)
                    return
                    var
                    ()
                    (?var-descr fun)
                    (?arg-num-fun fun)
                    () ; old tds
                    () ; no recursive
                    )))
          (check-result-subtypes
           ftd
           (?type-descr fun))
          (setf (?type-descr-s fun)
                (append ftd
                        (?type-descr-s fun))))
      (setf (?type-descr-s fun)
            (make-formal-type-descr
             (dynamic typepathes)
             return
             var
             ()
             (?var-descr fun)
             (?arg-num-fun fun)
             (?type-descr-s fun)
             () ; no recursive
             )))
    return
    ))

;;;-----------------------------------------------------------------------------
;;; function-call
;;;-----------------------------------------------------------------------------
(defmethod l2m-a ((con <function-label>) (form <app>))
  ;;
  (let ((fun (?function form))
        (in-fun (?function con))
        (arg-list (?arg-list form)))
    (if (cont-p fun) (l2m-a con (car (?arg-list form)))
      ;; ??? dieser Aufruf ist unklar !!!!!!!!!
      (if (or (eq fun in-fun)
              (and (generic-fun-p fun)
                   (eq (?discriminating-fun fun) in-fun)
                   (setq fun (?discriminating-fun fun))))
          ;; rest-recursion
          (let* ((arg-num (length arg-list))
                 (goto (make <goto>))
                 (var-vec (make-vector (+ arg-num 1))))
            (setf (vector-ref var-vec  0) ())
            (setf (?arg-num goto) arg-num)
            (setf (?var-descr goto) (make <var-descr>
                                          :var-vec var-vec
                                          :constant-counter 0))
            ;; fill the var-descr
            (l2m-call goto arg-list)
            ;; constant propagation ???
            ;; add annotation to function
            (setf (dynamic rec-calls) (cons goto (dynamic rec-calls)))
            ;; link variable
            (let ((curblock (dynamic block)))
              (link-var-vec var-vec goto arg-num)
              ;; add the statement to the Block
              (setf (?block goto) curblock)
              (setf (?result curblock) goto)
              ;; add type-descr to goto
              (setf (?type-descr-s goto)
                    (inference fun   ; *hf* 18.02
                               (make-actual-type-descr
                                (dynamic typepathes) goto (?var-descr goto) arg-num ())))
              (setf (?type-descr goto) (general-var-actual-descr arg-num))
              (let ((ntd (inference fun
                                    (make-formal-type-descr
                                     (dynamic typepathes)
                                     goto
                                     () ; var is not used
                                     (general-type) ; result type
                                     (?var-descr fun)
                                     (?arg-num-fun fun)
                                     () ; (?type-descr-s fun)
                                     t ; recursive
                                     ))))
                (setf (?type-descr-s fun)
                      (append  ntd (?type-descr-s fun)))
                (setf (?t-path curblock) ntd)
                )))
        ;; normal function call
        (let* ((result (call-a-function fun arg-list t (?read-glocs form)))
               (curblock (dynamic block))
               (return (make <return>
                             ;; :last-call call
                             :value result :block curblock)))
          (setf (?result curblock) return)
          (setf (?end-blocks con)
                (cons curblock (?end-blocks con)))
          (setf (?out-label curblock)
                con)
          ;; link the result-variable
          (link-result-variable con (dynamic typepathes) return)

          ;; fill the new formal type-descr-s
          (if (?range-and-domain in-fun)
              (let ((ftd (make-formal-type-descr
                          (dynamic typepathes)
                          return
                          result
                          ()
                          (?var-descr in-fun)
                          (?arg-num-fun in-fun)
                          () ; (?type-descr-s in-fun)
                          () ; no recursive
                          )))
                (check-result-subtypes
                 ftd
                 (?type-descr in-fun))
                (setf (?type-descr-s in-fun)
                      (append
                       ftd
                       (?type-descr-s in-fun))))
            (setf (?type-descr-s in-fun)
                  (make-formal-type-descr
                   (dynamic typepathes)
                   return
                   result
                   ()
                   (?var-descr in-fun)
                   (?arg-num-fun in-fun)
                   (?type-descr-s in-fun)
                   () ; no recursive
                   )))
          (if (or (local-static-p result)
                  (tempvar-p result))
              (setf (?link result)
                    (cons (cons return 1) ;1-01.06 *hf*
                          (?link result))) ; ! neu 03.06 *hf*
            ())
          result))
      )))

;;;-----------------------------------------------------------------------------
;;; special-forms
;;;-----------------------------------------------------------------------------

(defmethod l2m-a ((con <function-label>) (form <set-slot-value>))
  (finish-a con (set-slot-value-a con form)))

(defmethod l2m-a ((con <function-label>) (form <get-slot-value>))
  (finish-a con (get-slot-value-a con form)))

(defmethod l2m-a ((con <function-label>) (form <setq-form>))
  ;;
  ;; location = <local-static>, <global-static>, <imported-static>, <dynamic>,
  ;; <defined-named-const>, <imported-named-const>
  ;;
  (finish-a con (setq-form-a con form)))

(defmethod l2m-a ((con <function-label>) (form <progn-form>))
  (l2m-progn con (?form-list form)))

(defmethod l2m-a ((con <function-label>) (form <if-form>))
  (if-form-a con form)
  )


(defmethod l2m-a ((con <function-label>) (form <switch-form>))
  (print "<function-label> <switch-form> not implemented")
  ())

(defmethod l2m-a ((con <function-label>) (form <let*-form>))
  (letstar-a con form)
  )

(defmethod l2m-a ((con <function-label>) (form <labels-form>))
  (l2m-a con (?body form))
  )

(defmethod l2m-a ((con <function-label>) (form <let/cc-form>))
  (print "<function-label> <let/cc-form> not implemented")
  ())

;;
;;(defmethod l2m-a ((con <function-label>) (form <labeled-form>))
;;   (print "<function-label> <labeled-form> not implemented")
;;   ())
;;
;;
;;(defmethod l2m-a ((con <function-label>) (form <tagbody-form>))
;;   (print "<function-label> <tagbody-form> not implemented")
;;   ())
;;
;;
;;(defmethod l2m-a ((con <function-label>) (form <mv-lambda>))
;;   (print "<function-label> <mv-lambda> not implemented")
;;   ())
;;

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
