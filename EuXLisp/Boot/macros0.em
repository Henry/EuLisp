;;; macros0.em
;;; Euscheme code Copyright (c) 1994 Russell Bradford

(defmodule macros0
    (import (root macros)
     export
     (block
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
      defmodule
      ))

  (defmacro block (tag . body)
    (if (symbol? tag)
        `(let/cc ,tag ,@body)
      (error "not a symbol in block"
             <compilation-general-error>
             value: tag)))

  (defmacro return-from (tag . val)
    (if (symbol? tag)
        (if (null? val)
            `(,tag ())
          `(,tag ,@val))
      (error "not a symbol in return-from"
             <compilation-general-error>
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
                 (prin "*** redefining ")
                 (prin name)
                 (prin " in module ")
                 (print (current-module))))
           `(define ,(cons name args)
                    ,@body))
          ((definable-name? name)
           `(progn
              ((setter ,(car name)) ,(cadr name)
               (lambda ,args ,@body))
              ',name))
          (t (error "malformed name in defun"
                    <compilation-general-error>
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
          (t (error "malformed name in defgeneric"
                    <compilation-general-error>
                    value: name))))

  (define (defgeneric-methods name body)
          (cond ((null? body) ())
                ((not (eq (car body) method:))
                 (error "unknown keyword in defgeneric"
                        <compilation-general-error>
                        value: (car body)))
                ((null? (cdr body))
                 (error "odd-length keyword list in defgeneric"
                        <compilation-general-error>
                        value: name))
                (t (cons
                     `(defmethod ,name ,(caadr body) ,@(cdadr body))
                     (defgeneric-methods name (cdr (cdr body)))))))

  (defmacro defmethod (name args . body)
    (if (or (symbol? name)
            (definable-name? name))
        `(define-method ,(cons name args) ,@body)
      (error "malformed name in defgeneric"
             <compilation-general-error>
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
        (error "bad module name in import"
               <compilation-general-error>
               value: mod)
      `(progn
         (setq curmod (find-module (current-module)))
         (%IMPORT curmod ,mod))))

  (defmacro syntax (mod)
    (if (not (or (string? mod)
                 (symbol? mod)))
        (error "bad module name in syntax"
               <compilation-general-error>
               value: mod)
      `(progn
         (setq curmod (find-module (current-module)))
         (%IMPORT curmod ,mod))))

  (defmacro defmodule (name . body)
    (error "only use defmodule in root module"
           <compilation-general-error>
           value: name))

  )
