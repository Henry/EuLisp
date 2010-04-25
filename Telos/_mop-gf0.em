;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;; Description: defgeneric macro
;;;-----------------------------------------------------------------------------
(defmodule _mop-gf0
  (syntax (boot0)
   import (level1))

;;;-----------------------------------------------------------------------------
;;; Syntax: (defgeneric gfname (arglist) {keyword}*), where
;; gfname is {symbol | (setter symbol)},
;; arglist is {{symbol | (symbol class)}+ [ . symbol ]}, and
;; keyword is {key val}. Allowable keywords include
;; class:                   the class of the generic function
;; method-class:            the class of the associated methods
;; method-keywords:         a list of {key val} keywords to be passed to
;;                          calls of defmethod on this gfname
;; method:                  a method to be attached to the generic function
;; The method: keyword can be repeated.
;;;-----------------------------------------------------------------------------
  (defmacro defgeneric (gfname args . keywords)
    (let* ((gf-class (find-key class: keywords '<simple-generic-function>))
           (method-class (find-key method-class: keywords '<simple-method>))
           (method-inits (find-key method-keywords: keywords ()))
           (reqd (required-args args))
           (domain (map1-list
                    (lambda (a) (if (atom a) '<object> (car (cdr a))))
                    reqd))
           (name gfname)
           (target
            (cond ((symbolp gfname)
                   'deflocal)
                  ((and (consp gfname) (eq (car gfname) 'setter))
                   (setq name (car (cdr gfname)))
                   '(setter setter))
                  ((and (consp gfname) (eq (car gfname) 'converter))
                   (setq name (car (cdr gfname)))
                   '(setter converter))
                  (t (error "bad name for generic function ~a" gfname)))))
      `(progn
         (,target ,name
                  (make-generic-function
                   ',gfname
                   (make-vector ,(size domain) ,@domain)
                   ,gf-class ,method-class
                   (list ,@method-inits)
                   (list
                    ,@(filter-keywords
                                  keywords
                                  '(class: method-class: method: name:
                                           method-keywords:)))))
         ,@(do-defgeneric-methods gfname keywords)
         ,gfname)))

;;;-----------------------------------------------------------------------------
;;; Syntax: (generic-lambda (args) {keyword}*).
;; See defgeneric for details.
;;;-----------------------------------------------------------------------------
  (defmacro generic-lambda (args . keywords)
    (let* ((gf-class (find-key class: keywords '<simple-generic-function>))
           (method-class (find-key method-class: keywords '<simple-method>))
           (method-inits (find-key method-keywords: keywords ()))
           (name (find-key name: keywords 'anonymous))
           (reqd (required-args args))
           (domain (map (lambda (a)
                          (if (atom a) '<object> (car (cdr a))))
                        reqd))
           (gfname (gensym)))  ; "GENERIC-LAMBDA"
      `(let ((,gfname
              (make-generic-function
               ',name
               (make-vector ,(size domain) ,@domain)
               ,gf-class
               ,method-class
               (list ,@method-inits)
               (list
                ,@(filter-keywords
                   keywords
                   '(class: method-class: method: name: method-keywords:))))))
         ,@(do-defgeneric-methods gfname keywords)
         ,gfname)))

;;;-----------------------------------------------------------------------------
;;; Methods
;;;-----------------------------------------------------------------------------
  (defun do-defgeneric-methods (name keywords)
    (cond ((null keywords) ())
          ((eq (car keywords) method:)
           (cons `(defmethod ,name ,@(cadr keywords))
                 (do-defgeneric-methods name (cddr keywords))))
          (t (do-defgeneric-methods name (cddr keywords)))))

;;;-----------------------------------------------------------------------------
;;; Required arguments
;;;-----------------------------------------------------------------------------
  (defun required-args (args)
    (if (atom args) ()
      (cons (car args)
            (required-args (cdr args)))))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
