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
;;; Title: EuLisp kernel syntax definition functionality
;;;  Description:
;;    Fundamental syntax expansion functionality built on top of the
;;    XScheme put-syntax/get-syntax primitives.
;;
;;    Provides the ubiquitous Lisp defsyntax (equivalent to defmacro in CL),
;;    quasiquote, unquote and unquote-splicing operators as well as syntax
;;    expansion operations.
;;
;;    A syntax expanding form of define wrapped as %defun is also provided to
;;    avoid problems with the order of syntax expansion and compilation.
;;    Other syntax expanding forms are also provided but commented-out as it is
;;    not currently clear which are actually needed.
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule defsyntax
  (import (root
           system)
   export (defsyntax
           quasiquote
           unquote
           unquote-splicing
           expand-syntax1
           expand-syntax

           ;; expand-syntaxing version of the system define
           ;; Used in syntax only
           %defun))

(%defun getprop (s v)
        (if (symbol? s)
            (get-syntax s v)
          (raise-syntax-error "expected symbol" s)))

(%defun %expand-syntax (expr)
        (cond ((cons? expr)
               (if (symbol? (car expr))
                   (let ((expander (get-syntax (car expr) 'built-in-syntax))
                         (mac (get-syntax (car expr) '%syntax))
                         (rename (get-syntax (car expr) '%rename)))
                     (cond (expander (expander expr))
                           (mac (%expand-syntax (mac expr)))
                           (rename (%expand-syntax
                                    (cons rename (cdr expr))))
                           (t (%expand-list expr))))
                 (%expand-list expr)))
              ((symbol? expr) (or (get-syntax expr '%rename) expr))
              (t expr)))

(%defun %expand-list (lyst)
        (if (atom? lyst)
            lyst
          (%map-list %expand-syntax lyst)))

(%defun %expand-list-or-symbol (form)
        (if (symbol? form)
            (or (get-syntax form '%rename) form)
          (%expand-list form)))

(%defun identity (form) form)

;;(put-syntax 'quote 'built-in-syntax identity)

(%defun %expand-arg-list (form)
        (if (atom? form)
            (if (symbol? form)
                (or (get-syntax form '%rename) form)
              form)
          (cons
           (if (symbol? (car form))
               (or (get-syntax (car form) '%rename) (car form))
             (%expand-list (car form)))
           (%expand-arg-list (cdr form)))))

;; (put-syntax 'lambda 'built-in-syntax
;;             (lambda (form)
;;               (cons
;;                'lambda
;;                (cons
;;                 (%expand-arg-list (cadr form))
;;                 (%expand-list (cddr form))))))

;; Explicit syntax expansion is required here
;; otherwise syntax are not expanded inside definitions
(put-syntax '%defun 'built-in-syntax
            (lambda (form)
              (cons
               '%defun
               (cons
                (cadr form)
                (cons
                 (%expand-arg-list (caddr form))
                 (%expand-list (cdddr form)))))))

;; (put-syntax 'setq 'built-in-syntax
;;             (lambda (form)
;;               (cons
;;                'setq
;;                (cons
;;                 (or (getprop (cadr form) '%rename) (cadr form))
;;                 (%expand-list (cddr form))))))

;; (put-syntax 'cond 'built-in-syntax
;;             (lambda (form)
;;               (cons 'cond (%map-list %expand-list (cdr form)))))

;; (%defun %expand-let-form (form)
;;         (cons
;;          (car form)
;;          (let ((bindings (cadr form)))
;;            (cond ((symbol? bindings)
;;                   (cons (or (get-syntax bindings '%rename) bindings)
;;                         (cons (%map-list %expand-list-or-symbol
;;                                         (caddr form))
;;                               (%expand-list (cdddr form)))))
;;                  ((list? bindings)
;;                   (cons (%map-list %expand-list-or-symbol (cadr form))
;;                         (%expand-list (cddr form))))
;;                  (t (raise-syntax-error "bad let form" form))))))

;; (put-syntax 'let 'built-in-syntax %expand-let-form)
;; (put-syntax 'let* 'built-in-syntax %expand-let-form)
;; (put-syntax 'letrec 'built-in-syntax %expand-let-form)

(%defun %defsyntax-binds (arglist n)
        (cond ((null? arglist) ())
              ((atom? arglist) (list (list arglist
                                           (list 'list-tail '(cdr form) n))))
              (t (cons
                  (list (car arglist)
                        (list 'list-ref '(cdr form) n))
                  (%defsyntax-binds (cdr arglist) (%+ n 1))))))

(put-syntax '%defsyntax '%syntax
            (lambda (form)
              (list 'progn
                    (list 'put-syntax
                          (list 'quote (or (getprop (cadr form) '%rename)
                                           (cadr form)))
                          (list 'quote '%syntax)
                          (caddr form))
                    (list 'quote (or (getprop (cadr form) '%rename)
                                     (cadr form))))))

(put-syntax 'defsyntax '%syntax
            (lambda (form)
              (list '%defsyntax
                    (cadr form)
                    (list 'lambda
                          '(form)
                          (cons 'let
                                (cons (%defsyntax-binds (caddr form) 0)
                                      (cdddr form)))))))

; delay if progn sequence and or while access

;; Ensure defmodule is called only from the root module
(put-syntax 'defmodule 'built-in-syntax identity)

;; (put-syntax 'export 'built-in-syntax identity)
;; (put-syntax 'expose 'built-in-syntax identity)
;; (put-syntax 'enter-module 'built-in-syntax identity)
;; (put-syntax '!> 'built-in-syntax identity)
;; (put-syntax 'reenter-module 'built-in-syntax identity)
;; (put-syntax '!>> 'built-in-syntax identity)

(put-syntax '%defgeneric 'built-in-syntax
            (lambda (form)
              (cons
               '%defgeneric
               (cons
                (cadr form)
                (cons
                 (%expand-arg-list (caddr form))
                 ())))))

(put-syntax '%defmethod 'built-in-syntax
            (lambda (form)
              (cons
               '%defmethod
               (cons
                (cadr form)
                (cons
                 (%expand-arg-list (caddr form))
                 (%expand-list (cdddr form)))))))

; call-next-method next-method?

;; (put-syntax 'defclass 'built-in-syntax
;;             (lambda (form)
;;               (cons 'defclass
;;                     (cons (or (getprop (cadr form) '%rename) (cadr form))
;;                           (cons (%expand-arg-list (caddr form))
;;                                 (%expand-list (cdddr form)))))))

; let/cc with-handler unwind-protect

;; (put-syntax 'defcondition 'built-in-syntax
;;             (lambda (form)
;;               (cons 'defcondition
;;                     (%expand-list (cdr form)))))

;; (put-syntax 'defcondition 'built-in-syntax
;;             (lambda (form)
;;               (cons 'defcondition
;;                     (cons (or (getprop (cadr form) '%rename) (cadr form))
;;                           (cons (%expand-arg-list (caddr form))
;;                                 (%expand-list (cdddr form)))))))

;; the following is the original xscheme syntax code
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
           (if (and (cons? qq-car)
                    (eq (car qq-car) append-me-sym))
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
               ((eq (car exp) 'quasiquote)
                (setq qq-lev (add1 qq-lev))
                (let ((qq-val
                       (if (%= qq-lev 1) ; min val after inc
                           ; --> outermost level
                           (qq (cadr exp))
                         (qq-car-cdr exp))))
                  (setq qq-lev (sub1 qq-lev))
                  qq-val))
               ((or (eq (car exp) 'unquote)
                    (eq (car exp) 'unquote-splicing))
                (setq qq-lev (sub1 qq-lev))
                (let ((qq-val
                       (if (%= qq-lev 0) ; min val
                           ; --> outermost level
                           (if (eq (car exp) 'unquote-splicing)
                               (cons append-me-sym
                                     (%expand-syntax (cadr exp)))
                             (%expand-syntax (cadr exp)))
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
           (if (eq (car exp) append-me-sym)
               (raise-syntax-error "unquote-splicing in unspliceable position"
                                  (list 'unquote-splicing (cdr exp)))
             (or (check-qq-expansion (car exp))
                 (check-qq-expansion (cdr exp))))))))

(deflocal check-qq-expansion-flag ()) ; don't do checking

(deflocal unq-expander
  (lambda (l) (raise-syntax-error "unquote outside quasiquote" l)))

(deflocal unq-spl-expander
  (lambda (l) (raise-syntax-error "unquote splicing outside quasiquote" l)))

(put-syntax 'quasiquote 'built-in-syntax qq-expander)
(put-syntax 'unquote 'built-in-syntax unq-expander)
(put-syntax 'unquote-splicing 'built-in-syntax unq-spl-expander)

(%defun expand-syntax1 (expr)
        (cond ((cons? expr)
               (if (symbol? (car expr))
                   (let ((expander (get-syntax (car expr) 'built-in-syntax))
                         (mac (get-syntax (car expr) '%syntax))
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

(deflocal expand-syntax %expand-syntax)

;; just in case we get read twice somehow
(if (not (symbol-exists? '%compile))
    (deflocal %compile compile))

;; use this in compile below for debugging
(%defun expand-syntax (expr)
        (let ((result (%expand-syntax expr)))
          (%write expr)
          (%print " ==>> ")
          (%print result)
          (%print #\\n)
          result))

(%defun compile (expr . env)
        (if (null? env)
            (%compile (%expand-syntax expr))
          (%compile (%expand-syntax expr) (car env))))

;; (%defun compile (expr . env)
;;         (if (null? env)
;;             (%compile (expand-syntax expr))
;;           (%compile (expand-syntax expr) (car env))))

;;;-----------------------------------------------------------------------------
)  ;; End of module defsyntax
;;;-----------------------------------------------------------------------------
