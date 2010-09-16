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
;;; Title: EuLisp Level-0 macros0 module
;;;-----------------------------------------------------------------------------

(defmodule macros0
  (import (root
           macros)
   export (block
           return-from
           labels
           when
           unless
           while
           defun
           defgeneric
           defmethod
           generic-lambda
           method-lambda
           import
           syntax
           defmodule))

(defmacro block (tag . body)
  (if (symbol? tag)
      `(let/cc ,tag ,@body)
    (error <compilation-general-error>
           "not a symbol in block"
           value: tag)))

(defmacro return-from (tag . val)
  (if (symbol? tag)
      (if (null? val)
          `(,tag ())
        `(,tag ,@val))
    (error <compilation-general-error>
           "not a symbol in return-from"
           value: tag)))

(define (letrec-binding binding)
        (list
         (car binding)
         (cons 'lambda (cdr binding))))

(defmacro labels (bindings . body)
  `(letrec
    ,(map-list letrec-binding bindings)
    ,@body))

(defmacro when (test . body)
  `(if ,test (progn ,@body) ()))

(defmacro unless (test . body)
  `(if ,test () (progn ,@body)))

(defmacro while (test . body)
  `(let/cc {break}                    ; break can be captured in body
     (letrec
      ((loop (lambda ()
               (when ,test
                     ,@body
                     (loop)))))
      (loop))))

(define (definable-name? name)
        (and (cons? name)
             (or (eq (car name) 'setter)
                 (eq (car name) 'converter))))

; (defun foo (x) ...)
; (defun (setter foo) (x) ...)
; (defun (converter foo) (x) ...)
(defmacro defun (name args . body)
  (cond ((symbol? name)
         (if (symbol-exists? name)
             (progn
               (%display "*** redefining ")
               (%display name)
               (%display " in module ")
               (%display (current-module))
               (newline)))
         `(define ,(cons name args)
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
;    method: ((x <int>) ...)
;    method: ((y <flt>) ...)
;    ...)
(defmacro defgeneric (name args . body)
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

(define (defgeneric-methods name body)
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

(defmacro defmethod (name args . body)
  (if (or (symbol? name)
          (definable-name? name))
      `(define-method ,(cons name args) ,@body)
    (error <compilation-general-error>
           "malformed name in defgeneric"
           value: name)))

(defmacro generic-lambda (args . body)
  `(let (anonymous-generic)
     (define-generic (anonymous-generic ,@args))
     ,@(defgeneric-methods
        'anonymous-generic
        body)
     anonymous-generic))

(defmacro method-lambda (args . body)
  `(lambda (next-methods arg-list ,@args)
     ,@body))

(defmacro import (mod)
  (if (not (or (string? mod)
               (symbol? mod)))
      (error <compilation-general-error>
             "bad module name in import"
             value: mod)
    `(progn
       (setq curmod (find-module (current-module)))
       (%IMPORT curmod ,mod))))

(defmacro syntax (mod)
  (if (not (or (string? mod)
               (symbol? mod)))
      (error <compilation-general-error>
             "bad module name in syntax"
             value: mod)
    `(progn
       (setq curmod (find-module (current-module)))
       (%IMPORT curmod ,mod))))

(defmacro defmodule (name . body)
  (error <compilation-general-error>
         "only use defmodule in root module"
         value: name))

;;;-----------------------------------------------------------------------------
)  ;; End of module macros0
;;;-----------------------------------------------------------------------------
