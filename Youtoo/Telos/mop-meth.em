;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;; Description: handling methods
;;;-----------------------------------------------------------------------------
(defmodule mop-meth
  (syntax (_boot0 _mop-gf0 _mop-meth0)
   import (boot mop-prim mop-key mop-class mop-inspect mop-gf)
   export (compute-method-lookup-function stable-add-method make-method
                                          compute-discriminating-function add-method))

;;;-----------------------------------------------------------------------------
;;; Method allocation
;;;-----------------------------------------------------------------------------
(defun make-method (method-class domain fun inits)
  (if (and (eq method-class <simple-method>)
           ;(vector? domain)
           (function? fun)
           (null? inits))
      (let ((meth (primitive-allocate <simple-method> method-size)))
        ((setter method-domain) meth domain)
        ((setter method-function) meth fun)
        meth)
    (apply make method-class domain: domain function: fun inits)))

;;;-----------------------------------------------------------------------------
;;; Adding methods
;;;-----------------------------------------------------------------------------
(defgeneric add-method ((gf <generic-function>) (meth <method>)))

(defmethod add-method ((gf <generic-function>) (meth <method>))
  (check-method-domain meth gf)
  (if (subclass? (class-of meth)
                 (generic-function-method-class gf)) ()
    (error
     "mismatch between class of generic function ~a and method class ~a"
     (function-name gf)
     (class-of meth)))
  (let ((old (find-method gf (method-domain meth))))
    (if old (remove-method gf old) ()))
  ((setter generic-function-methods) gf
   (cons meth (generic-function-methods gf)))
  ((setter method-generic-function) meth gf)
  (finalize-generic gf)
  gf)

(defun primitive-add-method (gf meth)
  (check-method-domain meth gf)
  (let ((old (primitive-find-method gf (method-domain meth))))
    (if old
        (primitive-remove-method gf old)
      ()))
  ((setter generic-function-methods) gf
   (cons meth (generic-function-methods gf)))
  ((setter method-generic-function) meth gf)
  (gf-reset-cache gf)
  gf)

(defun stable-add-method (gf meth)
  (if (and (eq (class-of gf) <simple-generic-function>)
           (eq (class-of meth) <simple-method>))
      (primitive-add-method gf meth)
    (add-method gf meth)))

;;;-----------------------------------------------------------------------------
;;; Range and domain
;;;-----------------------------------------------------------------------------
(defun check-method-domain (meth gf)
  (let* ((meth-dom (method-domain meth))
         (meth-arity (vector-size meth-dom))
         (gf-dom (generic-function-domain gf))
         (gf-arity (vector-size gf-dom)))
    (labels
     ((loop (i)
            (if (< i meth-arity)
                (let ((meth-dom-class (or (vector-ref meth-dom i) <object>))
                      (gf-dom-class (vector-ref gf-dom i)))
                  (if gf-dom-class
                      (if (cpl-subclass? meth-dom-class gf-dom-class)
                          (loop (+ i 1))
                        ())
                    ;; Remember non-discriminating arg in method domain
                    (if (eq meth-dom-class <object>)
                        ((setter vector-ref) meth-dom i ())
                      ())))
              t)))
     (if (and (= meth-arity gf-arity)
              (loop 0))
         t
       (error
        "method extends domain of generic function ~a\n    method domain: ~a\n    generic function domain: ~a"
        (function-name gf) meth-dom gf-dom)))))

;;;-----------------------------------------------------------------------------
;;; Find method
;;;-----------------------------------------------------------------------------
(defgeneric find-method ((gf <generic-function>) sig))

(defmethod find-method ((gf <generic-function>) sig)
  (primitive-find-method gf sig))

(defun primitive-find-method (gf sig)
  (labels
   ((loop (meths)
          (if (null? meths) ()
            (let ((meth (car meths)))
              (if (sig= (method-domain meth) sig)
                  meth
                (loop (cdr meths)))))))
   (loop (generic-function-methods gf))))

;;;-----------------------------------------------------------------------------
;;; Remove method
;;;-----------------------------------------------------------------------------
(defun primitive-remove-method (gf meth)
  (let ((meths (generic-function-methods gf)))
    (if (member1-list meth meths)
        (progn
          ((setter generic-function-methods) gf (list-remove meth meths))
          ((setter method-generic-function) meth ())
          (gf-reset-cache gf))
      ()))
  gf)

(defgeneric remove-method ((gf <generic-function>) (meth <method>)))

(defmethod remove-method ((gf <generic-function>) (meth <method>))
  (let ((meths (generic-function-methods gf)))
    (if (member1-list meth meths)
        (progn
          ((setter generic-function-methods) gf (list-remove meth meths))
          ((setter method-generic-function) meth ())
          (finalize-generic gf))
      ()))
  gf)

;;;-----------------------------------------------------------------------------
;;; Finalize generic function
;;;-----------------------------------------------------------------------------
(defgeneric finalize-generic ((gf <generic-function>)))

(defmethod finalize-generic ((gf <generic-function>))
  (let* ((domain (generic-function-domain gf))
         (methods (generic-function-methods gf))
         (lookup (compute-method-lookup-function gf domain methods))
         (disc (compute-discriminating-function gf domain lookup methods)))
    ((setter generic-function-method-lookup-function) gf lookup)
    ((setter generic-function-discriminating-function) gf disc))
  (gf-reset-cache gf)
  gf)

;;;-----------------------------------------------------------------------------
;;; Initialize generic function
;;;-----------------------------------------------------------------------------
(defmethod initialize ((gf <generic-function>) keywords)
  (call-next-method)
  (do1-list (lambda (meth) (add-method gf meth))
            (find-key methods: keywords ()))
  (finalize-generic gf)
  gf)

;;;-----------------------------------------------------------------------------
;;; Initialize method
;;;-----------------------------------------------------------------------------
(defmethod initialize ((meth <method>) keywords)
  (call-next-method)
  (let ((gf (find-key generic-function: keywords ())))
    (if gf (add-method gf meth) ()))
  meth)

;;;-----------------------------------------------------------------------------
;;; Method lookup function
;;;-----------------------------------------------------------------------------
(defgeneric compute-method-lookup-function
  ((gf <generic-function>) sig methods))

(defmethod compute-method-lookup-function
  ((gf <generic-function>) sig methods)
  (let ((domain (generic-function-domain gf)))
    (named-lambda method-lookup-function values
                  (the-method-lookup-function gf values domain))))

;;;-----------------------------------------------------------------------------
;;; Discriminating function
;;;-----------------------------------------------------------------------------
(defgeneric compute-discriminating-function
  ((gf <generic-function>) domain lookup meths))

(defmethod compute-discriminating-function
  ((gf <generic-function>) domain lookup meths)
  (named-lambda discriminating-function values
                (let* ((method-cache (generic-function-method-cache gf))
                       (value-dom (vector-ref method-cache 0))
                       (method-cache-index (vector-ref method-cache 1))
                       (appl-meths (apply lookup values))
                       (meth-funs (map1-list method-function appl-meths)))
                  (if (null? meth-funs)
                      (error-no-applicable-methods gf values)
                    (let ((entry (cons value-dom meth-funs)))
                      ((setter vector-ref) method-cache method-cache-index entry)
                      (set-global-register next-methods (cdr meth-funs))
                      ;; Apply the function of the most specific applicable method
                      (apply (car meth-funs) values))))))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
