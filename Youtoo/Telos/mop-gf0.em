;;; Copyright 1997 A. Kind & University of Bath
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;
;;  Youtoo is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: defgeneric macro
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule mop-gf0
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
                  (lambda (a) (if (atom? a) '<object> (car (cdr a))))
                  reqd))
         (name gfname)
         (target
          (cond ((symbol? gfname)
                 'deflocal)
                ((and (cons? gfname) (eq (car gfname) 'setter))
                 (setq name (car (cdr gfname)))
                 '(setter setter))
                ((and (cons? gfname) (eq (car gfname) 'converter))
                 (setq name (car (cdr gfname)))
                 '(setter converter))
                (t (error () "bad name for generic function ~a" gfname)))))
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
                        (if (atom? a) '<object> (car (cdr a))))
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
  (cond ((null? keywords) ())
        ((eq (car keywords) method:)
         (cons `(defmethod ,name ,@(cadr keywords))
               (do-defgeneric-methods name (cddr keywords))))
        (t (do-defgeneric-methods name (cddr keywords)))))

;;;-----------------------------------------------------------------------------
;;; Required arguments
;;;-----------------------------------------------------------------------------
(defun required-args (args)
  (if (atom? args) ()
    (cons (car args)
          (required-args (cdr args)))))

;;;-----------------------------------------------------------------------------
)  ;; End of module mop-gf0
;;;-----------------------------------------------------------------------------
