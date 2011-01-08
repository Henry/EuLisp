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
;;; Title: defmethod syntax operator
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule mop-meth0
  (syntax (boot0)
   import (level-1))

;;;-----------------------------------------------------------------------------
;;; Syntax: (defmethod gfname {key val}* (arglist) {form}*), where
;;  gfname is {symbol | (setter symbol)}, and arglist is
;;  {{symbol | (symbol class)}+ [ . symbol ]}
;;;-----------------------------------------------------------------------------
(defsyntax defmethod (gfname . form)
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
(defsyntax defmethod-keywords (form)
  (if (atom? (car form))
      (cons (car form)
            (cons (car (cdr form))
                  (defmethod-keywords (cdr (cdr form)))))
    ()))

(defsyntax defmethod-sig (form)
  (if (atom? (car form))
      (defmethod-sig (cdr (cdr form)))
    (car form)))

(defsyntax defmethod-body (form)
  (if (atom? (car form))
      (defmethod-body (cdr (cdr form)))
    (cdr form)))

(defsyntax defmethod-args (sig)
  ;; allows { symbol | (symbol+ [ . symbol ]) }
  (cond ((null? sig) ())
        ((atom? sig) sig)
        ((atom? (car sig)) (cons (car sig)
                                 (defmethod-args (cdr sig))))
        (t (cons (caar sig)
                 (defmethod-args (cdr sig))))))

(defsyntax defmethod-domain (sig)
  (cond ((atom? sig) ())
        ((atom? (car sig))
         (cons () (defmethod-domain (cdr sig))))
        (t (cons (cadr (car sig)) (defmethod-domain (cdr sig))))))

;;;-----------------------------------------------------------------------------
;;; Create an anonymous method.
;;  Syntax: (method-lambda {key val}* (arglist) {form}*), where arglist is
;;  {{symbol | (symbol class)}+ [ . symbol]}
;;;-----------------------------------------------------------------------------
(defsyntax method-lambda form
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
(defsyntax method-function-lambda (args . body)
  `(lambda ,args (progn ,@body)))

(defsyntax named-method-function-lambda (name args . body)
  `(named-lambda (method ,name) ,args (progn ,@body)))

;;;-----------------------------------------------------------------------------
)  ;; End of module mop-meth0
;;;-----------------------------------------------------------------------------
