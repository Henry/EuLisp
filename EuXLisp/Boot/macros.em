;;; macros.em
;;; Euscheme code Copyright (c) 1994 Russell Bradford
;;; those tricky macros

(defmodule macros

    (import (root))

  (define (getprop s v)
          (if (symbol? s)
              (get-syntax s v)
            (raise-macro-error "expected symbol" s)))

  (define (%expand-macros expr)
          (cond ((pair? expr)
                 (if (symbol? (car expr))
                     (let ((expander (get-syntax (car expr) '%syntax))
                           (mac (get-syntax (car expr) '%macro))
                           (rename (get-syntax (car expr) '%rename)))
                       (cond (expander (expander expr))
                             (mac (%expand-macros (mac expr)))
                             (rename (%expand-macros
                                       (cons rename (cdr expr))))
                             (t (%expand-list expr))))
                   (%expand-list expr)))
                ((symbol? expr) (or (get-syntax expr '%rename) expr))
                (t expr)))

  (define (%expand-list lyst)
          (if (atom? lyst)
              lyst
            (map-list %expand-macros lyst)))

  (define (%expand-list-or-symbol form)
          (if (symbol? form)
              (or (get-syntax form '%rename) form)
            (%expand-list form)))

  (put-syntax 'macro '%macro
              (lambda (form)
                (list 'begin
                      (list 'put-syntax
                            (list 'quote (or (getprop (cadr form) '%rename)
                                             (cadr form)))
                            (list 'quote '%macro)
                            (caddr form))
                      (list 'quote (or (getprop (cadr form) '%rename)
                                       (cadr form))))))

  (define (identity form) form)

  (put-syntax 'quote '%syntax identity)

  (define (%expand-arg-list form)
          (if (atom? form)
              (if (symbol? form)
                  (or (get-syntax form '%rename) form)
                form)
            (cons
              (if (symbol? (car form))
                  (or (get-syntax (car form) '%rename) (car form))
                (%expand-list (car form)))
              (%expand-arg-list (cdr form)))))

  (put-syntax 'lambda '%syntax
              (lambda (form)
                (cons
                  'lambda
                  (cons
                    (%expand-arg-list (cadr form))
                    (%expand-list (cddr form))))))

  (put-syntax 'define '%syntax
              (lambda (form)
                (cons
                  'define
                  (cons
                    (%expand-arg-list (cadr form))
                    (%expand-list (cddr form))))))

  (put-syntax 'setq '%syntax
              (lambda (form)
                (cons
                  'setq
                  (cons
                    (or (getprop (cadr form) '%rename) (cadr form))
                    (%expand-list (cddr form))))))

  (put-syntax 'cond '%syntax
              (lambda (form)
                (cons 'cond (map-list %expand-list (cdr form)))))

  (define (%expand-let-form form)
          (cons
            (car form)
            (let ((bindings (cadr form)))
              (cond ((symbol? bindings)
                     (cons (or (get-syntax bindings '%rename) bindings)
                           (cons (map-list %expand-list-or-symbol
                                           (caddr form))
                                 (%expand-list (cdddr form)))))
                    ((list? bindings)
                     (cons (map-list %expand-list-or-symbol (cadr form))
                           (%expand-list (cddr form))))
                    (t (raise-macro-error "bad let form" form))))))

  (put-syntax 'let '%syntax %expand-let-form)
  (put-syntax 'let* '%syntax %expand-let-form)
  (put-syntax 'letrec '%syntax %expand-let-form)

  (define (%defmacro-binds arglist n)
          (cond ((null? arglist) ())
                ((atom? arglist) (list (list arglist
                                             (list 'list-tail '(cdr form) n))))
                (t (cons
                     (list (car arglist)
                           (list 'list-ref '(cdr form) n))
                     (%defmacro-binds (cdr arglist) (+ n 1))))))

  (put-syntax 'defmacro '%macro
              (lambda (form)
                (list 'macro
                      (cadr form)
                      (list 'lambda
                            '(form)
                            (cons 'let
                                  (cons (%defmacro-binds (caddr form) 0)
                                        (cdddr form)))))))

  ; delay if begin sequence and or while access

  (put-syntax 'defmodule '%syntax identity)
  (put-syntax 'export '%syntax identity)
  (put-syntax 'expose '%syntax identity)
  (put-syntax 'enter-module '%syntax identity)
  (put-syntax '!> '%syntax identity)
  (put-syntax 'reenter-module '%syntax identity)
  (put-syntax '!>> '%syntax identity)
  (put-syntax '%IMPORT '%syntax identity)

  (put-syntax 'define-generic '%syntax
              (lambda (form)
                (cons 'define-generic
                      (cons (%expand-arg-list (cadr form))
                            ()))))

  (put-syntax 'define-method '%syntax
              (lambda (form)
                (cons 'define-method
                      (cons (%expand-arg-list (cadr form))
                            (%expand-list (cddr form))))))

  ; call-next-method next-method?

  (put-syntax 'defclass '%syntax
              (lambda (form)
                (cons 'defclass
                      (cons (or (getprop (cadr form) '%rename) (cadr form))
                            (cons (%expand-arg-list (caddr form))
                                  (%expand-list (cdddr form)))))))

  ; let/cc with-handler unwind-protect

  (put-syntax 'defcondition '%syntax
              (lambda (form)
                (cons 'defcondition
                      (%expand-list (cdr form)))))

  ;; the following is the original xscheme macro code
  ;;
  (deflocal append-me-sym (gensym))     ; must be a gensym to avoid capture in
  ; certain (pathological) situations

  (deflocal qq-expander
    (lambda (l)
      (letrec
        (
         (qq-lev 0)                     ; always >= 0
         (qq-car-cdr
           (lambda (exp)
             (let ((qq-car (qq (car exp)))
                   (qq-cdr (qq (cdr exp))))
               (if (and (pair? qq-car)
                        (eq? (car qq-car) append-me-sym))
                   (list 'append (cdr qq-car) qq-cdr)
                 (list 'cons qq-car qq-cdr)))))
         (qq
           (lambda (exp)
             (cond ((symbol? exp)
                    (list 'quote exp))
                   ((vector? exp)
                    (list 'list->vector (qq (vector->list exp))))
                   ((atom? exp)         ; nil, number or boolean
                    exp)
                   ((eq? (car exp) 'quasiquote)
                    (setq qq-lev (add1 qq-lev))
                    (let ((qq-val
                            (if (= qq-lev 1) ; min val after inc
                                ; --> outermost level
                                (qq (cadr exp))
                              (qq-car-cdr exp))))
                      (setq qq-lev (sub1 qq-lev))
                      qq-val))
                   ((or (eq? (car exp) 'unquote)
                        (eq? (car exp) 'unquote-splicing))
                    (setq qq-lev (sub1 qq-lev))
                    (let ((qq-val
                            (if (= qq-lev 0) ; min val
                                ; --> outermost level
                                (if (eq? (car exp) 'unquote-splicing)
                                    (cons append-me-sym
                                          (%expand-macros (cadr exp)))
                                  (%expand-macros (cadr exp)))
                              (qq-car-cdr exp))))
                      (setq qq-lev (add1 qq-lev))
                      qq-val))
                   (else
                     (qq-car-cdr exp)))))
         )
        (let ((expansion (qq l)))
          (if check-qq-expansion-flag
              (check-qq-expansion expansion)) ; error on failure
          expansion))))

  (deflocal check-qq-expansion
    (lambda (exp)
      (cond ((vector? exp)
             (check-qq-expansion (vector->list exp)))
            ((atom? exp)
             ())
            (else
              (if (eq? (car exp) append-me-sym)
                  (raise-macro-error "unquote-splicing in unspliceable position"
                                     (list 'unquote-splicing (cdr exp)))
                (or (check-qq-expansion (car exp))
                    (check-qq-expansion (cdr exp))))))))

  (deflocal check-qq-expansion-flag ()) ; don't do checking

  (deflocal unq-expander
    (lambda (l) (raise-macro-error "unquote outside quasiquote" l)))

  (deflocal unq-spl-expander
    (lambda (l) (raise-macro-error "unquote splicing outside quasiquote" l)))

  (put-syntax 'quasiquote '%syntax qq-expander)
  (put-syntax 'unquote '%syntax unq-expander)
  (put-syntax 'unquote-splicing '%syntax unq-spl-expander)

  (export defmacro quasiquote unquote unquote-splicing)

  (define (symbol-macro x)
          (get-syntax x '%macro))

  (export symbol-macro)

  ;  (put-syntax 'syntax '%syntax
  ;       (lambda (form)
  ;        (reintern-syntax (cadr form))))
  ;
  ;  (export syntax)

  (define (macroexpand1 expr)
          (cond ((pair? expr)
                 (if (symbol? (car expr))
                     (let ((expander (get-syntax (car expr) '%syntax))
                           (mac (get-syntax (car expr) '%macro))
                           (rename (get-syntax (car expr) '%rename)))
                       (cond (expander
                               (expander expr))
                             (mac
                               (mac expr))
                             (rename
                               (cons rename (cdr expr)))
                             (t (cons (car expr) (%expand-list (cdr expr))))))
                   (%expand-list expr)))
                ((symbol? expr) (or (get-syntax expr '%rename) expr))
                (t expr)))

  (deflocal macroexpand %expand-macros)

  (export macroexpand macroexpand1)

  (deflocal debugging ())
  (put-syntax 'dprint '%macro
              (lambda (form)
                (if debugging
                    (cons 'print (cdr form))
                  ())))

  (export debugging dprint)

  ;; just in case we get read twice somehow
  (if (not (bound? '%compile))
      (deflocal %compile compile))

  ;; use this in compile below for debugging
  (define (expand-macros expr)
          (let ((result (%expand-macros expr)))
            (prin1 expr)
            (display " ==>> ")
            (print result)
            result))

  (define (compile expr . env)
          (if (null? env)
              (%compile (%expand-macros expr))
            (%compile (%expand-macros expr) (car env))))

  ;;  (define (compile expr . env)
  ;;          (if (null? env)
  ;;              (%compile (expand-macros expr))
  ;;            (%compile (expand-macros expr) (car env))))

  )
