;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;; Description: defmethod macro
;;;-----------------------------------------------------------------------------
(defmodule mop-meth0
  (syntax (boot0)
   import (level1))

;;;-----------------------------------------------------------------------------
;;; Syntax: (defmethod gfname {key val}* (arglist) {form}*), where
;;  gfname is {symbol | (setter symbol)}, and arglist is
;;  {{symbol | (symbol class)}+ [ . symbol ]}
;;;-----------------------------------------------------------------------------
  (defmacro defmethod (gfname . form)
    (let* ((*absent* '(absent))
           (keywords (defmethod-keywords form))
           (sig (defmethod-sig form))
           (body (defmethod-body form))
           (inits (filter-keywords keywords '(class:)))
           (method-class (find-key class: keywords *absent*))
           (args (defmethod-args sig))
           (domain (defmethod-domain sig)))
      `(stable-add-method
        ,gfname
        (make-method ,(if (eq method-class *absent*)
                          `(generic-function-method-class ,gfname)
                        method-class)
                     (make-vector ,(size domain) ,@domain)
                     (named-method-function-lambda ,gfname ,args ,@body)
                     (append
                      (list ,@inits)
                      (generic-function-method-keywords ,gfname))))))

;;;-----------------------------------------------------------------------------
;;; Defmethod auxilary functions
;;;-----------------------------------------------------------------------------
  (defmacro defmethod-keywords (form)
    (if (atom (car form))
        (cons (car form)
              (cons (car (cdr form))
                    (defmethod-keywords (cdr (cdr form)))))
      ()))

  (defmacro defmethod-sig (form)
    (if (atom (car form))
        (defmethod-sig (cdr (cdr form)))
      (car form)))

  (defmacro defmethod-body (form)
    (if (atom (car form))
        (defmethod-body (cdr (cdr form)))
      (cdr form)))

  (defmacro defmethod-args (sig)
    ;; allows { symbol | (symbol+ [ . symbol ]) }
    (cond ((null sig) ())
          ((atom sig) sig)
          ((atom (car sig)) (cons (car sig)
                                  (defmethod-args (cdr sig))))
          (t (cons (caar sig)
                   (defmethod-args (cdr sig))))))

  (defmacro defmethod-domain (sig)
    (cond ((atom sig) ())
          ((atom (car sig))
           (cons () (defmethod-domain (cdr sig))))
          (t (cons (cadr (car sig)) (defmethod-domain (cdr sig))))))

;;;-----------------------------------------------------------------------------
;;; Create an anonymous method.
;;  Syntax: (method-lambda {key val}* (arglist) {form}*), where arglist is
;;  {{symbol | (symbol class)}+ [ . symbol]}
;;;-----------------------------------------------------------------------------
  (defmacro method-lambda form
    (let* ((keywords (defmethod-keywords form))
           (sig (defmethod-sig form))
           (body (defmethod-body form))
           (inits (filter-keywords keywords '(class:)))
           (method-class (find-key class: keywords '<simple-method>))
           (args (defmethod-args sig))
           (domain (defmethod-domain sig)))
      `(make-method ,method-class
                    (make-vector ,(size domain) ,@domain)
                    (method-function-lambda ,args ,@body)
                    (list ,@inits))))

;;;-----------------------------------------------------------------------------
;;; Create a lambda that can be used as the function part of a method.
;;;  Syntax: (method-function-lambda (arglist) {form}*), where arglist is
;;;  { (symbol+ [ . symbol ]) }
;;;-----------------------------------------------------------------------------
  (defmacro method-function-lambda (args . body)
    `(lambda ,args (progn ,@body)))

  (defmacro named-method-function-lambda (name args . body)
    `(named-lambda (method ,name) ,args (progn ,@body)))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
