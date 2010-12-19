;;; Copyright 1994 Russell Bradford
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'EuXLisp'
;;;-----------------------------------------------------------------------------
;;
;;  EuXLisp is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  EuXLisp is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: EuLisp kernel syntax
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule syntax
  (syntax (defsyntax)
   import (root)
   export (;; Imported from defsyntax
           defsyntax
           quasiquote
           unquote
           unquote-splicing
           expand-syntax1
           expand-syntax

           ;; Defined in this module
           defun
           defgeneric
           defmethod
           generic-lambda
           method-lambda
           defmodule
           import
           syntax
           letfuns))

(%defun definable-name? (name)
        (and (cons? name)
             (or (eq (car name) 'setter)
                 (eq (car name) 'converter))))

; (defun foo (x) ...)
; (defun (setter foo) (x) ...)
; (defun (converter foo) (x) ...)
(defsyntax defun (name args . body)
  (cond ((symbol? name)
         (if (symbol-exists? name)
             (progn
               (%print "*** redefining ")
               (%print name)
               (%print " in module ")
               (%print (current-module))
               (%print "\n")))
         `(%defun ,name ,args
                  ,@body))
        ((definable-name? name)
         `(progn
            ((setter ,(car name)) ,(cadr name)
             (lambda ,args ,@body))
            ',name))
        (t (error <compilation-general-error>
                  "malformed name in defun"
                  value: name))))

; (defgeneric foo (x)
;    method: ((x <fpi>) ...)
;    method: ((y <flt>) ...)
;    ...)
(defsyntax defgeneric (name args . body)
  (cond ((symbol? name)
         `(progn (define-generic ,(cons name args))
                 ,@(defgeneric-methods name body)
                 ',name))
        ((definable-name? name)
         `(progn
            (define-generic ,(cons 'setter/converter args))
            ((setter ,(car name)) ,(cadr name) setter/converter)
            ,@(defgeneric-methods
               (list 'setter (cadr name))
               body)
            ',name))
        (t (error <compilation-general-error>
                  "malformed name in defgeneric"
                  value: name))))

(%defun defgeneric-methods (name body)
        (cond ((null? body) ())
              ((not (eq (car body) method:))
               (error <compilation-general-error>
                      "unknown keyword in defgeneric"
                      value: (car body)))
              ((null? (cdr body))
               (error <compilation-general-error>
                      "odd-size keyword list in defgeneric"
                      value: name))
              (t (cons
                  `(defmethod ,name ,(caadr body) ,@(cdadr body))
                  (defgeneric-methods name (cdr (cdr body)))))))

(defsyntax defmethod (name args . body)
  (if (or (symbol? name)
          (definable-name? name))
      `(define-method ,(cons name args) ,@body)
    (error <compilation-general-error>
           "malformed name in defgeneric"
           value: name)))

(defsyntax generic-lambda (args . body)
  `(let (anonymous-generic)
     (define-generic (anonymous-generic ,@args))
     ,@(defgeneric-methods
        'anonymous-generic
        body)
     anonymous-generic))

(defsyntax method-lambda (args . body)
  `(lambda (next-methods arg-list ,@args)
     ,@body))

;; Interactive defmodule (error)
(defsyntax defmodule (name . body)
  (error <compilation-general-error>
         "only use defmodule in root module"
         value: name))

;; Interactive import directive
(defsyntax import (mod)
  (if (not (or (string? mod)
               (symbol? mod)))
      (error <compilation-general-error>
             "bad module name in import"
             value: mod)
    `(progn
       (setq curmod (find-module (current-module)))
       (%import curmod ,mod))))

;; Interactive syntax directive
(defsyntax syntax (mod)
  (if (not (or (string? mod)
               (symbol? mod)))
      (error <compilation-general-error>
             "bad module name in syntax"
             value: mod)
    `(progn
       (setq curmod (find-module (current-module)))
       (%import curmod ,mod))))

(%defun letfun-binding (binding)
        (list
         (car binding)
         (cons 'lambda (cdr binding))))

(defsyntax letfuns (funs . body)
  `(letrec
    ,(map-list letfun-binding funs)
    ,@body))

;;;-----------------------------------------------------------------------------
)  ;; End of module syntax
;;;-----------------------------------------------------------------------------
