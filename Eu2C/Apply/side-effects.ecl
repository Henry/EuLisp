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
;;;  Title: analyse of side-effects and synthese of types
;;;  Description:
;;    * the pass-slot will be changed from 0 to 1
;;    * if not (?sys-glocs fun) is set, fill slot fread-gloc and fwrite-gloc
;;    * set in structure - the slots
;;    var-ref - read-gloc
;;    fun - read-glocs write-glocs fread-gloc fwrite-gloc
;;    app - read-glocs
;;    setq-form - write-gloc read-gloc
;;    if-form - read-gloc
;;    let*-form - read-gloc-list write-gloc-list
;;    * set (?function-type fun)
;;    $normal - simple function
;;    $data   - used as data
;;    $closure - used closure-vars (and possible as data)
;;    * set (?closure local-static)
;;    * env = (var1 var2 var3 ...)
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Horst Friedrich
;;;-----------------------------------------------------------------------------

#module side-effects
(import ((except (format)
                 level-1)
         lzs
         mzs
         simple-programming
         side-effects-h
         analyse-h
         accessors
         type-propagation
         type-inference
         (only (range&domain-descr
                range&domain-as-signature-p)
               ti-descrs)
         debugging
         name-of-fun
         inline-method ; transform-call-next-method
         ;; transform-next-method?
         ;; arg-error in-generic-fun in-method
         apply-funs ; %cons, typecheck, %call-next-method, %next-method?
         expand-literal
         tail-module ; %cast
         (only (append
                format)
               common-lisp))
 syntax (level-1)
 expose (side-effects-h)
 export (analysis-side-effects ; args: module-list
         ;; bild read-glocs/write-glocs etc.
         synthesis-types-in-side-effects ; args: module-list
         balance-side-effects ; args: list-of-fgloc's list-of-glocs
         balance-side-effects1 ; args: new-glocs old-glocs
         init-side-effecs-fun ; for ann new function (in gutter)
         *funcall-fread-gloc*
         *funcall-fwrite-gloc*))


(deflocal *amend-glocs* ())

(defglobal read-glocs ())

(defglobal write-glocs ())

(deflocal *funcall-read-glocs* ())

(deflocal *funcall-write-glocs* ())

(deflocal *funcall-fread-gloc* ())

(deflocal *funcall-fwrite-gloc* ())

(defun analysis-side-effects (mlist)
  (format t "~%S")
  (setq *funcall-fread-gloc* (make <fgloc>
                                   :fun ^funcall :glocs ()))
  (setq *funcall-fwrite-gloc* (make <fgloc>
                                    :fun ^funcall :glocs ()))
  ;;  (init-side-effecs mlist)
  (dynamic-let ((cur-module (car mlist)))
               (init-side-effecs-fun (?toplevel-forms (car mlist)))
               (init-side-effecs-exported-funs mlist)
               (init-side-effecs-fun typecheck)
               (init-side-effects-literal-funs
                (get-functions-used-in-literals)))
  (analysis-side-effects1 mlist)
  )

(defun init-side-effecs-exported-funs (modules)
  (when modules
        (init-side-effecs-xfuns (?fun-list (car modules)))
        (init-side-effecs-exported-funs (cdr modules))))

(defun init-side-effecs-xfuns (funs)
  (when funs
        (let ((fun (car funs)))
          (when (and (global? fun) (?exported fun))
                (init-side-effecs-fun fun)))
        (init-side-effecs-xfuns (cdr funs))))

(defun init-side-effects-literal-funs (funs)
  (init-side-effecs-funs funs)
  (let ((funs-after (get-functions-used-in-literals)))
    (if (eq (length funs) (length funs-after))
        () ; then we are ready
      (init-side-effects-literal-funs funs-after))))

(defun analysis-side-effects1 (mlist)
  (if *amend-glocs*
      (progn (setq *amend-glocs* ())
             (format t "S")
             (analysis-side-effects2 mlist)
             (analysis-side-effects1 mlist))
    ()))

(defun analysis-side-effects2 (mlist)
  (if (null? mlist) ()
    (let* ((m (car mlist))
           (tl (?toplevel-forms m)))
      (a-s-e-funs (?fun-list m))
      (if tl (a-s-e-fun tl) ())
      (analysis-side-effects2 (cdr mlist)))))

(defun a-s-e-funs (flist)
  (if flist
      (progn (a-s-e-fun (car flist))
             (a-s-e-funs (cdr flist)))
    ()))

(defun a-s-e-fun (fun)
  (if (or (?sys-glocs fun)
          (null? (?fread-gloc fun)))
      ()
    (let* ((frg (?fread-gloc fun))
           (fwg (?fwrite-gloc fun))
           (nfrg
            (balance-side-effects (?read-glocs fun)
                                  (?glocs frg)))
           (nfwg
            (balance-side-effects (?write-glocs  fun)
                                  (?glocs fwg))))
      (if (eq nfrg (?glocs frg)) ()
        (progn (setf (?glocs frg) nfrg)
               (setq *amend-glocs* ^t)))
      (if (eq nfwg (?glocs fwg)) ()
        (progn (setf (?glocs fwg) nfwg)
               (setq *amend-glocs* ^t)))
      )))

(defglobal cur-module ())

;;(defun init-side-effecs (mlist)
;;  (if (null? mlist) ()
;;      (let* ((m (car mlist))
;;             (tl (?toplevel-forms m)))
;;        (dynamic-let ((cur-module m))
;;                     (init-side-effecs-funs (?fun-list m))
;;                     (if tl (init-side-effecs-fun tl) ())
;;                     (init-side-effecs (cdr mlist)))))
;;)

(defun init-side-effecs-funs (funl)
  (if (null? funl) ()
    (progn (init-side-effecs-fun (car funl))
           (init-side-effecs-funs (cdr funl)))))

(defun init-side-effecs-fun (fun)
  (dynamic-let ((in-generic-fun ())
                (in-method ()))
               (init-side-effecs-fun2 fun)))

;; To avoid: Warning: Undefined variable ES::USED-CL-VARS
#+ :cmu (defglobal used-cl-vars ())

(defun init-side-effecs-fun2 (fun)
  ;; (start-analyse-fun fun)
  (if (and (range&domain-as-signature-p fun)
           (= (?pass fun) 0))
      (set-signature fun (list (range&domain-descr fun))))
  (if (or (= (?pass fun) 1)
          ;; (?fread-gloc fun)
          ;; (?sys-glocs fun)
          (null? (or (defined-fun? fun)
                    (defined-generic-fun? fun)))
          (null? (?params fun))
          )
      (if (and (imported-fun? fun) (eq (?pass fun) 0))
          (setf (?pass fun) 1)
        ())
    (progn (setf (?pass fun) 1)
           (setf (?arg-num fun) (compute-argnum 0
                                                (?var-list (?params fun))
                                                (?rest (?params fun))))
           (if (local-fun? fun)
               (let ((cl ()))
                 (dynamic-let ((env (get-parameter fun))
                               (used-cl-vars ()))
                              (init-side-effecs-fun1 fun)
                              (setq cl (dynamic used-cl-vars)))
                 (if cl
                     (progn (setf (?function-type fun) $closure)
                            (dynamic-setq used-cl-vars 't))
                   (setf (?function-type fun) $data))
                 ;;               (let ((fl (?fun-list (dynamic cur-module))))
                 ;;                 (if (member fun fl) ()
                 ;;                     (progn (setf (?identifier fun)
                 ;;                                  (format () "~a_local"
                 ;;                                          (?identifier (analysed-fun))))
                 ;;                            (setf (?fun-list (dynamic cur-module))
                 ;;                                  (cons fun fl)))))
                 )
             (if (global-generic-fun? fun)
                 (let ((df (?discriminating-fun fun)))
                   (init-side-effecs-fun df)
                   (init-side-effects-methods fun
                                              (?method-list fun)
                                              (?identifier fun))
                   ;;                 (let ((fl (?fun-list (dynamic cur-module))))
                   ;;                 (if (member df fl) ()
                   ;;                     (progn (setf (?identifier df)
                   ;;                                  (format () "~a_descr"
                   ;;                                          (?identifier fun)))
                   ;;                            (setf (?fun-list (dynamic cur-module))
                   ;;                                  (cons df fl)))))
                   )
               (if (?sys-glocs fun)
                   (dynamic-let ((env (get-parameter fun))
                                 (used-cl-vars ()))
                                (side-effects (?body fun)))
                 (dynamic-let ((env (get-parameter fun))
                               (used-cl-vars ()))
                              (init-side-effecs-fun1 fun)))))))
  ;; (end-analyse-fun fun)
  )

(defun init-side-effects-methods (gf m-list id)
  (dynamic-let ((in-generic-fun gf))
               (init-side-effects-methods1 m-list id)))

(defun init-side-effects-methods1 (m-list id)
  (if m-list
      (let ((mf (?fun (car m-list))))
        (dynamic-let ((in-method (car m-list)))
                     (init-side-effecs-fun2 mf))
        (init-side-effects-methods1 (cdr m-list) id))
    ()))

(defun compute-argnum (nr var-list rest)
  (if (null? var-list)
      (if rest (- 0 (+ nr 1)) nr)
    (compute-argnum (+ nr 1) (cdr var-list) rest)))

(defun get-parameter (fun)
  (let ((para (?params fun)))
    (if (?rest para)
        (cons (?rest para) (?var-list para))
      (?var-list para))))

(defun init-side-effecs-fun1 (fun)
  (start-analyse-fun fun)
  (format t "s")
  (let ((fread-gloc (make <fgloc> :fun fun :glocs ()))
        (fwrite-gloc (make <fgloc> :fun fun :glocs ())))
    (setf (?fread-gloc fun) fread-gloc)
    (setf (?fwrite-gloc fun) fwrite-gloc)
    (dynamic-let
     ((read-glocs ())
      (write-glocs ()))
     (let* ((body (?body fun))
            (seresult (side-effects body)))
       (setf (?body fun) (collect-literals body))
       (if seresult
           (if (cons? seresult)
               (dynamic-setq read-glocs
                             (append seresult (dynamic read-glocs)))
             (dynamic-setq read-glocs
                           (cons seresult (dynamic read-glocs))))
         ())
       (if (dynamic read-glocs)
           (progn
             (setf (?read-glocs fun) (dynamic read-glocs))
             (setf (?glocs fread-gloc)
                   (balance-side-effects (dynamic read-glocs)
                                         () ; (?glocs fread-gloc)
                                         ))
             (setq *amend-glocs* t))
         ())
       (if (dynamic write-glocs)
           (progn
             (setf (?write-glocs fun) (dynamic write-glocs))
             (setf (?glocs fwrite-gloc)
                   (balance-side-effects (dynamic write-glocs)
                                         () ; (?glocs fwrite-gloc)
                                         ))
             (setq *amend-glocs* t)) ())
       )))
  (end-analyse-fun fun))


(defun balance-side-effects (lfgloc+gloc old-glocs)
  ;; lfgloc+gloc - list of fgloc's and gloc's
  ;; old-glocs - list of gloc's
  (if (null? lfgloc+gloc) old-glocs
    (let ((fgloc-or-gloc (car lfgloc+gloc)))
      (if (gloc? fgloc-or-gloc)
          (if (gloc-assoc fgloc-or-gloc old-glocs)
              (balance-side-effects (cdr lfgloc+gloc) old-glocs)
            (balance-side-effects (cdr lfgloc+gloc)
                                  (cons fgloc-or-gloc old-glocs)))
        (balance-side-effects   (cdr lfgloc+gloc)
                                (balance-side-effects1
                                 (?glocs fgloc-or-gloc) old-glocs))))))

(defun balance-side-effects1 (new-glocs old-glocs)
  (if (null? new-glocs) old-glocs
    (let ((ng (car new-glocs)))
      (if (gloc-assoc ng old-glocs)
          (balance-side-effects1 (cdr new-glocs) old-glocs)
        (balance-side-effects1 (cdr new-glocs) (cons ng old-glocs))))))

(defun gloc-assoc(gloc gloc-list)
  (gloc-assoc1 (?gplace gloc) gloc-list))

(defun gloc-assoc1 (gplace gloc-list)
  (if (null? gloc-list) ()
    (let ((gloc (car gloc-list)))
      (if (eq (?gplace gloc) gplace) gloc-list
        (gloc-assoc1 gplace (cdr gloc-list))))))

(defgeneric side-effects (form) )
;; the result of side-effets are read-glocs (from global-static or
;; imported-static variables), either (), a (unbound) gloc
;; or a list of gloc«s

;;--------------------------
;; constants
;;--------------------------

(defmethod collect-literals ((form <named-const>))
  (setf (?value form) (expand-literal (?value form)))
  form)

(defmethod side-effects ((form <named-const>))
  ())

(defmethod collect-literals ((form <sym>))
  (expand-literal form))

(defmethod side-effects ((form <sym>))
  ())

(defmethod collect-literals ((form <symbol>))
  form)

(defmethod side-effects ((form <symbol>))
  ())

(defmethod collect-literals ((form <structured-literal>))
  (expand-literal form))

(defmethod side-effects ((form <structured-literal>))
  ())

(defmethod collect-literals ((form <fpi>))
  (expand-literal form))

(defmethod side-effects ((form <fpi>))
  ())

(defmethod collect-literals ((form <double-float>))
  (expand-literal form))

(defmethod side-effects ((form <double-float>))
  ())

(defmethod collect-literals ((form <character>))
  (expand-literal form))

(defmethod side-effects ((form <character>))
  ())

(defmethod collect-literals ((form <class-def>))
  (expand-literal form)
  form)

(defmethod side-effects ((form <class-def>))
  ())

(defmethod collect-literals ((form <literal-instance>))
  (expand-literal form))

(defmethod side-effects ((form <literal-instance>))
  ())


(defmethod side-effects ((form <global-fun>))
  (init-side-effecs-fun form)
  (let ((fread-gloc (?fread-gloc form)))
    (if fread-gloc
        (progn (setq *funcall-read-glocs*
                     (cons fread-gloc *funcall-read-glocs*))
               (setf (?function-type form) $data)
               ())
      ())))

(defmethod collect-literals ((form <global-fun>))
  (expand-literal form))

(defmethod side-effects ((form <local-fun>))
  (init-side-effecs-fun form)
  (let ((fread-gloc (?fread-gloc form)))
    (if fread-gloc
        (progn (setq *funcall-read-glocs*
                     (cons fread-gloc *funcall-read-glocs*))
;;;(setf (?function-type form) $data)
               ())
      ())))

(defmethod collect-literals ((form <local-fun>))
  (if (eq (?function-type form) $closure) form
    (expand-literal form)))

(defmethod side-effects ((form <imported-fun>))
  (init-side-effecs-fun form)
  (let ((fread-gloc (?fread-gloc form)))
    (if fread-gloc
        (progn (setq *funcall-read-glocs*
                     (cons fread-gloc *funcall-read-glocs*))
;;;            (setf (?function-type form) $data)
               ())
      ())))

(defmethod collect-literals ((form <imported-fun>))
  (expand-literal form))

(defmethod side-effects ((form <special-sys-fun>))
  (init-side-effecs-fun form)
  (let ((fread-gloc (?fread-gloc form)))
    (if fread-gloc
        (progn (setq *funcall-read-glocs*
                     (cons fread-gloc *funcall-read-glocs*))
               (setf (?function-type form) $data)
               ())
      ())))

(defmethod collect-literals ((form <special-sys-fun>))
  (let ((fun (analysed-fun)))
    (format t "~% -------------------------------------------")
    (format t "~% *** E R R O R in ~a function ~a ***"
            (funtype-of fun) (name-of fun))
    (format t "~% used the TAIL-function ~A as data"
            (name-of form)  )
    (format t "~% -------------------------------------------")
    form))

(defmethod side-effects ((form <global-generic-fun>))
  ;;(print "side-effects of <global-generic-fun> not yet implemented")
  ())

(defmethod collect-literals ((form <global-generic-fun>))
  (expand-literal form))

(defmethod side-effects ((form <local-generic-fun>))
  ;;(print "side-effects of <local-generic-fun> not yet implemented")
  ())

(defmethod collect-literals ((form <local-generic-fun>))
  (if (eq (?function-type form) $closure) form
    (expand-literal form)))

(defmethod side-effects ((form <imported-generic-fun>))
  ;;(print "side-effects of <imported-generic-fun> not yet implemented")
  ())

(defmethod collect-literals ((form <imported-generic-fun>))
  (expand-literal form))

(defmethod side-effects ((form <method-def>))
  ())

(defmethod collect-literals ((form <method-def>))
  (expand-literal form))

(defmethod side-effects ((form <cont>))
  ;;(print "side-effects of <cont> not yet implemented")
  ()
  )

(defmethod side-effects ((form <null>))
  ())

(defmethod collect-literals ((form <null>))
  (expand-literal form)
  form)

;;--------------------------
;;   end of constans
;;--------------------------

;;--------------------------
;;   begin of variables
;;--------------------------

(defmethod side-effects ((form <var-ref>))
  (side-effects-var (?var form) form))

(defmethod collect-literals ((form <var-ref>))
  form)

(defgeneric side-effects-var (var var-ref))

(defmethod side-effects-var ((var <local-static>) var-ref)
  (if (member var (dynamic env)) ()
    (progn
      (setf (?closure var) 't)
      ;; ; debug *closure*
      ;; (format t "~% Closure ~s env: ~s Fun: ~s"
      ;;         var (dynamic env) (name-of (analysed-fun)))
      ;; (unterbrechung var (dynamic env))
      (dynamic-setq used-cl-vars 't)))
  ())

(defmethod side-effects-var ((var <global-static>) var-ref)
  (let ((gloc (make <gloc> :gplace var)))
    (setf (?read-gloc var-ref)  gloc)
    gloc))

(defmethod side-effects-var ((var <imported-static>) var-ref)
  (let ((gloc (make <gloc> :gplace var)))
    (setf (?read-gloc var-ref)  gloc)
    gloc))

(defmethod side-effects-var ((var <dynamic>) var-ref)
  (let ((gloc (make <gloc> :gplace var)))
    (setf (?read-gloc var-ref)  gloc)
    (dynamic-setq read-glocs (cons gloc (dynamic read-glocs)))
    ()))


;;--------------------------
;;   begin of function call
;;--------------------------


(defmethod side-effects ((form <app>))
  (let ((fun (?function form))
        (arg-list (?arg-list form)))
    (if (named-const? fun)
        (let ((value (?value fun)))
          (if (eq value ^unknown) ()
            (progn
              (setf (?function form) value)
              (setq fun value))))
      ())
    (if (or (global-fun? fun)
            (imported-fun? fun)
            (global-generic-fun? fun))
        (if (eq fun %apply)
            (progn
              (setq fun (map-apply (length arg-list)))
              ;; (init-side-effecs-fun fun)
              )
          ())
      (progn
        (setf (?arg-list form) (cons fun arg-list))
        (setq fun (map-funcall (length arg-list)))
        (init-side-effecs-fun fun)
        (setq arg-list (?arg-list form))))
    (setf (?function form) fun)
    (if (and (fun? fun) (?reduce fun))
        () ; make the translation in the collect-literal-pass
      (checkup-arguments fun arg-list form))
    (let ((args (side-effects-args (?arg-list form))))
      (if args
          (dynamic-setq read-glocs (append args (dynamic read-glocs)))
        ())
      (if (fun? fun)
          (progn
            (init-side-effecs-fun fun)
            (let ((frgloc (?fread-gloc fun))
                  (fwgloc (?fwrite-gloc fun)))
              (if frgloc
                  (dynamic-setq read-glocs (cons frgloc (dynamic read-glocs)))
                ())
              (if fwgloc
                  (dynamic-setq write-glocs (cons fwgloc (dynamic write-glocs)))
                ()))
            )
        (let ((sresult (side-effects fun)))
          (if sresult
              (if (cons? sresult)
                  (dynamic-setq read-glocs
                                (append sresult (dynamic read-glocs)))
                (dynamic-setq read-glocs
                              (cons sresult (dynamic read-glocs))))
            ())
          ))
      ())))

(defun side-effects-args (arg-list)
  (if (null? arg-list) ()
    (let ((res (side-effects (car arg-list))))
      (if res
          (if (cons? res)
              (append res (side-effects-args (cdr arg-list)))
            (cons res (side-effects-args (cdr arg-list))))
        (side-effects-args (cdr arg-list))))))

(defmethod collect-literals ((form <app>))
  (let* ((fun (?function form))
         (arg-list (?arg-list form)))
    (if (and (fun? fun) (?reduce fun))
        (reduce-ap? (?reduce fun) arg-list form)
      (if (eq fun %call-next-method)
          (dynamic-let ((next-method?arams
                         (?params (?fun (dynamic in-method)))))
                       (transform-call-next-method arg-list form))
        (if (eq fun %next-method?)
            (transform-next-method? arg-list form)
          (progn
            (setf (?arg-list form)
                  (if (eq fun %cast)
                      (cons (car arg-list)
                            (collect-literals-list (cdr arg-list)))
                    (collect-literals-list arg-list)))
            form))))))

(defun reduce-ap? (red-descr arg-list form)
  (let ((bin-fun (car red-descr))
        (one-arg (car (cdr red-descr)))
        (zero-arg (car (cdr (cdr red-descr))))
        (type (car (cdr (cdr (cdr red-descr)))))
        (n (length arg-list)))
    (if (= n 0)
        (reduce-zero zero-arg form)
      (if (= n 1)
          (reduce-one one-arg (car arg-list) form)
        (if (eq type ^acc)
            (reduce-acc bin-fun arg-list)
          (if (eq type ^logical)
              (reduce-logical bin-fun arg-list)
            ;;            (if (eq type ^select1)
            ;;              (reduce-select1 bin-fun arg-list)
            ;;              (if (eq type ^select2)
            ;;                (reduce-select2 bin-fun arg-list)
            (unknown-reduce 'type type form))))))
  ;;   ))
  )

(defun unknown-reduce (what arg form)
  (let ((fun (?function form))
        (arg-list (?arg-list form)))
    (format t "~% unknown reduce-~a ~a" what arg)
    (setf (?arg-list form)
          (if (eq fun %cast)
              (cons (car arg-list)
                    (collect-literals-list (cdr arg-list)))
            (collect-literals-list arg-list)))
    form))

(defun reduce-zero (zero-arg form)
  (if (eq zero-arg ^error)
      (let ((foo (?function form))
            (fun (analysed-fun)))
        (format t "~% -------------------------------------------")
        (format t "~% *** E R R O R in ~a function ~a ***"
                (funtype-of fun) (name-of fun))
        (format t "~% missing arguments in a call of ~A function ~A"
                (funtype-of foo) (name-of foo)  )
        (format t "~% -------------------------------------------")
        form)
    (collect-literals zero-arg)))

(defun reduce-one (one-arg self form)
  (if (eq one-arg ^self)
      (collect-literals self)
    (if (eq one-arg ^t)
        (collect-literals t)
      (if (cons? one-arg)
          (make <app>
                :function (car one-arg)
                :arg-list (collect-literals-list
                           (subst-self (cdr one-arg) self)))
        (unknown-reduce '1-arg-descr one-arg form)))))

(defun subst-self (li self)
  (if li
      (if (eq (car li) ^self)
          (cons self (subst-self (cdr li) self))
        (cons (car li) (subst-self (cdr li) self)))
    ()))

(defun reduce-acc (bin-fun arg-list)
  (reduce-acc1 (cdr (cdr arg-list))
               bin-fun
               (make <app>
                     :function bin-fun
                     :arg-list (collect-literals-list
                                (list (car arg-list)
                                      (car (cdr arg-list)))))))

(defun reduce-acc1 (arg-list bin-fun arg1)
  (if arg-list
      (reduce-acc1 (cdr arg-list)
                   bin-fun
                   (make <app> :function bin-fun
                         :arg-list
                         (list arg1 (collect-literals (car arg-list)))))
    arg1))

(defun reduce-logical (bin-fun arg-list)
  (if (cdr (cdr arg-list))
      (let* ((var-exp-lst (select-let-expr arg-list))
             (var-lst (select-vars var-exp-lst))
             (init-lst (if var-lst (select-inits var-exp-lst) ())))
        (if var-lst
            (make <let*-form>
                  :var-list var-lst
                  :init-list init-lst
                  :body (logical-body var-exp-lst bin-fun))
          (logical-body var-exp-lst bin-fun)))
    (make <app> :function bin-fun
          :arg-list (collect-literals-list
                     (list (car arg-list)
                           (car (cdr arg-list)))))))

(defun logical-body (lst bin-fun)
  (if (cdr lst)
      (make <if-form>
            :pred (make <app>
                        :function bin-fun
                        :var-list (list (car lst)
                                        (car (cdr lst))))
            :then (logical-body (cdr lst) bin-fun)
            :else ())
    t))

(defun select-vars (lst)
  (if lst
      (if (cons? (car lst))
          (cons (car (car lst))
                (select-vars (cdr lst)))
        (select-vars (cdr lst)))
    ()))

(defun select-inits (lst)
  (if lst
      (if (cons? (car lst))
          (cons (cdr (car lst))
                (select-inits (cdr lst)))
        (select-inits (cdr lst)))
    ()))

(defun select-let-expr (arg-list)
  (if arg-list
      (cons (select-let-expr1 (car arg-list))
            (select-let-expr (cdr arg-list)))
    ()))

(defgeneric select-let-expr1 (form))

(defmethod select-let-expr1 ((form <named-const>))
  form)
(defmethod select-let-expr1 ((form <sym>))
  form)
(defmethod select-let-expr1 ((form <symbol>))
  form)
(defmethod select-let-expr1 ((form <structured-literal>))
  form)
(defmethod select-let-expr1 ((form <fpi>))
  form)
(defmethod select-let-expr1 ((form <double-float>))
  form)
(defmethod select-let-expr1 ((form <character>))
  form)
(defmethod select-let-expr1 ((form <class-def>))
  form)
(defmethod select-let-expr1 ((form <literal-instance>))
  form)
(defmethod select-let-expr1 ((form <global-fun>))
  form)
(defmethod select-let-expr1 ((form <local-fun>))
  form)
(defmethod select-let-expr1 ((form <imported-fun>))
  form)
(defmethod select-let-expr1 ((form <special-sys-fun>))
  form)
(defmethod select-let-expr1 ((form <global-generic-fun>))
  form)
(defmethod select-let-expr1 ((form <local-generic-fun>))
  form)
(defmethod select-let-expr1 ((form <imported-generic-fun>))
  form)
(defmethod select-let-expr1 ((form <cont>))
  form)
(defmethod select-let-expr1 ((form <null>))
  form)
(defmethod select-let-expr1 ((form <var-ref>))
  form)
(defmethod select-let-expr1 (form)
  (cons (make <local-static>) form))


(defun collect-literals-list (args)
  (if (null? args) ()
    (let* ((first (car args))
           (rest (cdr args))
           (cfirst (collect-literals first))
           (crest (collect-literals-list rest)))
      (if (and (eq first cfirst)
               (eq rest crest)) args
        (cons cfirst crest)))))

(defun map-apply (argc)
  (if (eq argc 0) %apply ; missing arguments
    (if (eq argc 1) %funcall0
      (if (eq argc 2) %apply1
        (if (eq argc 3) %apply2
          (if (eq argc 4) %apply3
            (if (eq argc 5) %apply4
              (if (eq argc 6) %apply5
                (if (eq argc 7) %apply6
                  (if (eq argc 8) %apply7
                    (if (eq argc 9) %apply8
                      %apply)))))))))))

(defun map-funcall (argc)
  (if (eq argc 0) %funcall0
    (if (eq argc 1) %funcall1
      (if (eq argc 2) %funcall2
        (if (eq argc 3) %funcall3
          (if (eq argc 4) %funcall4
            (if (eq argc 5) %funcall5
              (if (eq argc 6) %funcall6
                (if (eq argc 7) %funcall7
                  (if (eq argc 8) %funcall8
                    (progn (format t "~% funcall with too many arguments (> 8)")
                           %apply)))))))))))

(defun checkup-arguments (fun arg-list application)
  ;; !!! Hack
  (if (fun? fun)
      (if (special-sys-fun? fun) () ; at time no argument-check !!!
        (let ((params (?params fun)))
          (if (null? params) ()
            (let ((length-var-list (length (?var-list params)))
                  (length-arg-list (length arg-list))
                  (rest (?rest params)))
              (if (eq length-var-list length-arg-list)
                  (if rest
                      (setf (?arg-list application)
                            (append arg-list '(())))
                    ())
                (if rest
                    (if (> length-arg-list length-var-list)
                        (cons-rest-arguments length-var-list
                                             arg-list application)
                      (progn
                        (arg-error fun (- length-var-list length-arg-list)
                                   "too few")
                        (setf (?arg-list application)
                              (append arg-list
                                      (add-arguments
                                       (+ (- length-var-list length-arg-list)
                                          1))))))
                  (if (> length-var-list length-arg-list)
                      (progn
                        (arg-error fun (- length-var-list length-arg-list)
                                   "too few")
                        (setf (?arg-list application)
                              (append arg-list
                                      (add-arguments
                                       (- length-var-list length-arg-list)))))
                    (progn
                      (arg-error fun (- length-arg-list length-var-list)
                                 "too many")
                      (setf (?arg-list application)
                            (use-first-arguments
                             length-var-list arg-list)))))))))
        ) ; (if (special-sys-fun? fun) ()) ; at time no argument-check !!!
    ()))

(defun add-arguments (n)
  (if (= n 0) ()
    (cons () (add-arguments (- n 1)))))

(defun use-first-arguments (n arg-list)
  (if (= n 0) ()
    (cons (car arg-list)
          (use-first-arguments (- n 1) (cdr arg-list)))))



(defun cons-rest-arguments (l arg-list application)
  (setf (?arg-list application)
        (cons-rest-arguments1 l arg-list)))

(defun cons-rest-arguments1 (l arg-list)
  (if (= l 0)
      (list (cons-argument arg-list))
    (cons (car arg-list)
          (cons-rest-arguments1 (- l 1) (cdr arg-list)))))

(defun cons-argument (arglist)
  (if arglist
      (make <app> :function %cons
            :arg-list (list (car arglist)
                            (cons-argument (cdr arglist))))
    ()))


;;--------------------------
;;   end of function call
;;--------------------------
(defmethod side-effects ((form <set-slot-value>))
  (let ((iresult (side-effects (?instance form)))
        (vresult (side-effects (?value form))))
    ;; (if iresult (setf (?read-gloc form) iresult) ())
    vresult))

(defmethod side-effects ((form <get-slot-value>))
  (let ((iresult (side-effects (?instance form))))
    ;; (if iresult (setf (?read-gloc form) iresult) ())
    iresult))

(defmethod side-effects ((form <setq-form>))
  (let ((sresult (side-effects (?form form)))
        (var (if (defined-named-const? (?location form))
                 ()
               (?var (?location form)))))
    (if sresult (setf (?read-gloc form) sresult) ())
    (if (local-static? var)
        (if (member var (dynamic env)) ()
          (progn (setf (?closure var) 't)
                 ;;              ; debug *closure*
                 ;;              (format t "~% Closure ~s env: ~s Fun: ~s"
                 ;;                      var (dynamic env) (name-of (analysed-fun)))
                 ;;               (unterbrechung var (dynamic env))
                 (dynamic-setq used-cl-vars 't)))
      ())
    (if (or (global-static? var)
            (local-static? var)
            (dynamic? var))
        (let ((gloc (make <gloc> :gplace var)))
          (setf (?write-gloc form) gloc)
          (dynamic-setq write-glocs
                        (cons gloc (dynamic write-glocs))))
      ())
    sresult))

(defmethod collect-literals ((form <get-slot-value>))
  (setf (?instance form) (collect-literals (?instance form)))
  form)

(defmethod collect-literals ((form <set-slot-value>))
  (setf (?value form) (collect-literals (?value form)))
  (setf (?instance form) (collect-literals (?instance form)))
  form)

(defmethod collect-literals ((form <setq-form>))
  ;; the (?location form) is not interesting
  (setf (?form form) (collect-literals (?form form)))
  form)

(defmethod side-effects ((form <progn-form>))
  (side-effects-progn (?form-list form)))

(defmethod collect-literals ((form <progn-form>))
  (setf (?form-list form)
        (collect-literals-list (?form-list form)))
  form)

(defun side-effects-progn (list)
  (if (null? list) ()
    (if (cdr list)
        (progn (side-effects (car list))
               (side-effects-progn (cdr list)))
      (side-effects (car list)))))

(defmethod side-effects ((form <if-form>))
  (let ((test-rgloc (side-effects (?pred form)))
        (then-rgloc (side-effects (?then form)))
        (else-rgloc (side-effects (?else form))))
    (if test-rgloc
        (setf (?read-gloc form) test-rgloc) ())
    (if then-rgloc
        (if else-rgloc
            (if (cons? then-rgloc)
                (if (cons? else-rgloc)
                    (append then-rgloc else-rgloc)
                  (cons else-rgloc then-rgloc))
              (if (cons? else-rgloc)
                  (cons then-rgloc else-rgloc)
                (list then-rgloc else-rgloc)))
          then-rgloc)
      else-rgloc)))

(defmethod collect-literals ((form <if-form>))
  (setf (?pred form) (collect-literals (?pred form)))
  (setf (?then form) (collect-literals (?then form)))
  (setf (?else form) (collect-literals (?else form)))
  form)

(defmethod side-effects ((form <switch-form>))
  (print "side-effects <switch-form> not yet implemented"))

(defmethod collect-literals ((form <switch-form>))
  form)

(defun side-effects-let*-vars (var-list)
  (if (null? var-list) ()
    (cons (side-effects-leT*-var (car var-list))
          (side-effects-let*-vars (cdr var-list)))))

(defun side-effects-let*-inits (forms)
  (if (null? forms) ()
    (cons (side-effects (car forms))
          (side-effects-let*-inits (cdr forms)))))

(defmethod side-effects ((form <let*-form>))
  (setf (?write-gloc-list form)
        (side-effects-let*-vars (?var-list form)))
  (let ((new-env (append (?var-list form) (dynamic env))))
    (dynamic-let ((env new-env))
                 (setf (?read-gloc-list form)
                       (side-effects-let*-inits (?init-list form)))
                 (side-effects (?body form)))))

(defmethod collect-literals ((form <let*-form>))
  (setf (?init-list form) (collect-literals-list (?init-list form)))
  (setf (?body form) (collect-literals (?body form)))
  form)

(defgeneric side-effects-let*-var (var) )

(defmethod side-effects-let*-var ((var <local-static>))
  ())

(defmethod side-effects-let*-var ((var <global-static>))
  (let ((gloc (make <gloc> :gplace var)))
    (dynamic-setq write-glocs (cons gloc (dynamic write-glocs)))
    gloc))

(defmethod side-effects-let*-var ((var <imported-static>))
  (let ((gloc (make <gloc> :gplace var)))
    (dynamic-setq write-glocs (cons gloc (dynamic write-glocs)))
    gloc))

(defmethod side-effects-let*-var ((var <dynamic>))
  (let ((gloc (make <gloc> :gplace var)))
    (dynamic-setq write-glocs (cons gloc (dynamic write-glocs)))
    gloc))

(defmethod side-effects ((form <labels-form>))
  (init-side-effects-labels (?fun-list form))
  (side-effects (?body form)))

(defun init-side-effects-labels (fun-list)
  (if (null? fun-list) ()
    (progn (init-side-effecs-fun (car fun-list))
           (init-side-effects-labels (cdr fun-list)))))

(defmethod collect-literals ((form <labels-form>))
  (setf (?fun-list form) (collect-literals-list (?fun-list form)))
  (setf (?body form) (collect-literals (?body form)))
  form)

(defmethod side-effects ((form <let/cc-form>))
  (side-effects (?body form)))

(defmethod collect-literals ((form <let/cc-form>))
  (setf (?body form) (collect-literals (?body form)))
  form)

;;
;;(defmethod side-effects ((form <labeled-form>))
;;)
;;
;;(defmethod side-effects ((form <tagbody-form>))
;;)
;;
;;(defmethod side-effects ((form <mv-lambda>))

(defun synthesis-types-in-side-effects (mlist)
  mlist)

#module-end
