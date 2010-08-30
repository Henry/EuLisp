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
;;;  Title: inline of LZS-Functions
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Horst Friedrich
;;;-----------------------------------------------------------------------------

#module inline-method
(import ((except (format)
                 eulisp1)
         LZS
         LZS-MOP
         simple-programming
         expand-literal
         debugging
         name-of-fun
         apply-funs
         accessors
         (only (mapcar
                assoc
                make-instance
                append
                format)
               common-lisp))
 syntax (eulisp1)
 export (inline-method
         in-generic-fun
         in-method
         next-method-params
         transform-call-next-method
         transform-next-method-p
         arg-error
         more-specific-p))


(defvar in-generic-fun ())
(defvar in-method ())
(defvar next-method-params ())

(defun transform-call-next-method (arg-list form)
  (let ((gf (dynamic in-generic-fun))
        (mf (dynamic in-method)))
    (if arg-list
        (arg-error %call-next-method (length arg-list) "too many")
      ())
    (if (and gf mf)
        (let ((nm (next-specific-method mf gf)))
          (if nm
              (let ((params (dynamic next-method-params)))
                (make <app>
                      :function nm
                      :arg-list (mk-var-ref (?var-list params)
                                            (?rest params))))
            (progn
              (no-next-method-error)
              form)
            ))
      (progn
        (outside-error %call-next-method)
        form))))

(defun mk-var-ref (vars rest)
  (if vars
      (cons (make <var-ref> :var (car vars))
            (mk-var-ref (cdr vars) rest))
    (if rest (list (make <var-ref> :var rest)) ())))
(defun transform-next-method-p (arg-list form)
  (let ((gf (dynamic in-generic-fun))
        (mf (dynamic in-method)))
    (if arg-list
        (arg-error %next-method-p (length arg-list) "too many")
      ())
    (if (and gf mf)
        (let ((nm (next-specific-method mf gf)))
          (if nm (expand-literal ;^t
                  66) ()))  ; hock !!!
      (progn
        (outside-error %next-method-p)
        form))))

(defun next-specific-method (meth gf)
  (let ((nm (next-specific-method1
             (~generic-function-methods gf)
             (~method-domain meth) meth () ())))
    (if nm (?fun nm) ())))

(defun next-specific-method1 (m-lst dom mth s-dom s-mth)
  (if m-lst
      (let* ((c-mth (car m-lst))
             (c-dom (~method-domain c-mth)))
        (if (eq c-mth mth)
            (next-specific-method1 (cdr m-lst) dom mth s-dom s-mth)
          (if (and (more-specific-p dom c-dom)
                   (or (null? s-dom)
                       (more-specific-p c-mth s-dom)))
              (next-specific-method1 (cdr m-lst) dom mth c-dom c-mth)
            (next-specific-method1 (cdr m-lst) dom mth s-dom s-mth))))
    s-mth))

(defun more-specific-p (dom1 dom2)
  (if dom1
      (if (eq (car dom1) (car dom2))
          (more-specific-p (cdr dom1) (cdr dom2))
        (if (member (car dom2)
                    (~class-precedence-list (car dom1)))
            t ()))
    ()))

(defun no-next-method-error ()
  (let ((fun (analysed-fun)))
    (format t "~% --------------------- error -----------------------")
    (format t "~% no NEXT-METHOD applicable !!! ")
    (format t "~% in ~a function ~a "
            (funtype-of fun) (name-of fun))
    (format t "~% ---------------------------------------------------~%")))

(defun outside-error (foo)
  (let ((fun (analysed-fun)))
    (format t "~% --------------------- error -----------------------")
    (format t "~% call to ~a outside (!!) of a method "
            (name-of foo))
    (format t "~% in ~a function ~a "
            (funtype-of fun) (name-of fun))
    (format t "~% ---------------------------------------------------~%")))

(defun arg-error (foo nr str)
  ;;  (arg-error-break)
  (let ((fun (analysed-fun)))
    (format t "~% --------------------- error -----------------------")
    (format t "~% ~A arguments ~A in a call of ~A function ~A"
            nr str (funtype-of foo) (name-of foo)  )
    (format t "~% in ~a function ~a "
            (funtype-of fun) (name-of fun))
    (format t "~% ---------------------------------------------------~%"))
  )
;;definitions and init-forms

(defvar inline-env ())

(defun inline-method (method-def var-list)
  (let* ((fun (?fun method-def))
         (params (?params fun)))
    (if (imported-p fun)
        (progn
          (if (?rest params)
              (progn
                (setf (?var-list params)
                      (append (?var-list params) (list (?rest params))))
                (setf (?rest params) ())) ())
          (make-instance <app>
                         :function fun
                         :arg-list (mapcar (lambda (var)
                                             (make-instance <var-ref> :var var))
                                           var-list)))
      (dynamic-let ((in-method method-def))
                   (dynamic-let ((inline-env
                                  (compute-env var-list
                                               (?var-list params)
                                               (?rest params))))
                                (copy-lzs-form (?body fun)))))))

(defun compute-env (new-var old-var rest)
  (if old-var
      (cons (cons (car old-var) (car new-var))
            (compute-env (cdr new-var) (cdr old-var) rest))
    (if rest
        (list (cons rest (car new-var)))
      ())))

(defgeneric copy-lzs-form (form))
;;--------------------------
;; constants
;;--------------------------

(defmethod copy-lzs-form ((form <named-const>))
  form)

(defmethod copy-lzs-form ((form <sym>))
  form)

(defmethod copy-lzs-form ((form <symbol>))
  form)

(defmethod copy-lzs-form ((form <structured-literal>))
  form)

(defmethod copy-lzs-form ((form <fpi>))
  form)

(defmethod copy-lzs-form ((form <double-float>))
  form)

(defmethod copy-lzs-form ((form <character>))
  form)

(defmethod copy-lzs-form ((form <class-def>))
  form)

(defmethod copy-lzs-form ((form <literal-instance>))
  form)

(defmethod copy-lzs-form ((form <global-fun>))
  form)

(defmethod copy-lzs-form ((form <local-fun>))
  form)

(defmethod copy-lzs-form ((form <imported-fun>))
  form)

(defmethod copy-lzs-form ((form <special-sys-fun>))
  form)

(defmethod copy-lzs-form ((form <global-generic-fun>))
  form)

(defmethod copy-lzs-form ((form <local-generic-fun>))
  form)

(defmethod copy-lzs-form ((form <imported-generic-fun>))
  form)

(defmethod copy-lzs-form ((form <cont>))
  form)

(defmethod copy-lzs-form ((form <null>))
  form)

;;--------------------------
;;   end of constans
;;--------------------------

;;--------------------------
;;   begin of variables
;;--------------------------

(defmethod copy-lzs-form ((form <var-ref>))
  (let ((newvar (assoc (?var form) (dynamic inline-env))))
    (make-instance <var-ref>
                   :var (if newvar (cdr newvar) (?var form))))
  )

;;--------------------------
;;   begin of function call
;;--------------------------

(defun copy-lzs-form-list (form-list)
  (if form-list
      (cons (copy-lzs-form (car form-list))
            (copy-lzs-form-list (cdr form-list)))
    ()))

(defmethod copy-lzs-form ((form <app>))
  (let* ((fun (?function form))
         (newvar (if (var-ref-p fun)
                     (assoc (?var fun) (dynamic inline-env))
                   ())))
    (if (eq fun %call-next-method)
        (transform-call-next-method (?arg-list form) form)
      (if (eq fun %next-method-p)
          (transform-next-method-p (?arg-list form) form)
        (make-instance <app>
                       :function (if newvar
                                     (make-instance <var-ref> :var (cdr newvar)) fun)
                       :arg-list (copy-lzs-form-list (?arg-list form))))))
  )

(defun add-env (old new env)
  (if old
      (cons (cons (car old) (car new))
            (add-env (cdr old) (cdr new) env))
    env))

;;--------------------------
;;   end of function call
;;--------------------------

(defmethod copy-lzs-form ((form <get-slot-value>))
  (make-instance <get-slot-value>
                 :instance (copy-lzs-form (?instance form))
                 :slot (?slot form)))

(defmethod copy-lzs-form ((form <set-slot-value>))
  (make-instance <set-slot-value>
                 :instance (copy-lzs-form (?instance form))
                 :slot (?slot form)
                 :value (copy-lzs-form (?value form))))

(defmethod copy-lzs-form ((form <setq-form>))
  (let* ((location (?location form))
         (isvar (if (defined-named-const-p location)
                    () t))
         (newvar (if isvar (assoc (?var location) (dynamic inline-env))
                   ())))
    (make-instance <setq-form>
                   :form (copy-lzs-form (?form form))
                   :location (if isvar
                                 (make-instance <var-ref>
                                                :var (if newvar (cdr newvar)
                                                       (?var location)))
                               location))))

(defmethod copy-lzs-form ((form <progn-form>))
  (make-instance <progn-form>
                 :form-list (copy-lzs-form-list (?form-list form)))
  )


(defmethod copy-lzs-form ((form <if-form>))
  (make-instance <if-form>
                 :pred (copy-lzs-form (?pred form))
                 :then (copy-lzs-form (?then form))
                 :else (copy-lzs-form (?else form))))

(defmethod copy-lzs-form ((form <switch-form>))
  (print "copy-lzs-form <switch-form> not yet implemented")
  )

(defun copy-var-list (old-vars)
  (if old-vars
      (cons (let ((var (car old-vars)))
              (if (local-static-p var)
                  (make-instance <local-static>
                                 :identifier (?identifier var)
                                 :module (?module var))
                var))
            (copy-var-list (cdr old-vars)))
    ()))

(defmethod copy-lzs-form ((form <let*-form>))
  (let* ((old-vars (?var-list form))
         (new-vars (copy-var-list old-vars)))
    (dynamic-setq inline-env
                  (add-env old-vars new-vars (dynamic inline-env)))
    (make-instance <let*-form>
                   :var-list new-vars
                   :init-list (copy-lzs-form-list (?init-list form))
                   :body (copy-lzs-form (?body form))
                   :type-list (?type-list form)))
  )

(defmethod copy-lzs-form ((form <labels-form>))
  (make-instance <labels-form>
                 :fun-list (copy-lzs-form-list (?fun-list form))
                 :body (copy-lzs-form (?body form))))

(defmethod copy-lzs-form ((form <let/cc-form>))
  (make-instance <let/cc-form>
                 :cont (?cont form)
                 :body (?body form)))

;;
;;(defmethod copy-lzs-form ((form <labeled-form>))
;;)
;;
;;(defmethod copy-lzs-form ((form <tagbody-form>))
;;)
;;
;;(defmethod copy-lzs-form ((form <mv-lambda>))


#module-end
