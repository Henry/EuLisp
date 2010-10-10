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
;;; Title: Multiple values as in Scheme
;;;  Authors: Russell Bradford
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    not wonderfully efficient.
;;    If you pass multiple values to a continuation that only expects a single
;;    value you will probably get strange results
;;
;;    (values 1 2 3)                    multiple values
;;    (call-with-values
;;      (lambda () ...)                 thunk returning values
;;      (lambda (a b c ...) ...))       that are passed to here
;;
;;    CL-like macros:
;;    multiple-value-setq multiple-value-list multiple-value-call
;;    values-list multiple-value-bind
;;;-----------------------------------------------------------------------------

(defmodule values
  (import (level-0)
   export (values
           call-with-values
           multiple-value-setq
           multiple-value-list
           multiple-value-call
           values-list
           multiple-value-bind))

(defclass <values> ()
  ((values reader: get-values
           keyword: values:))
  constructor: (mv-values values:)
  predicate: values?)

(defmethod generic-write ((vals <values>) stream)
  (let ((vs (get-values vals)))
    (unless (null? vs)
            (write-values vs stream generic-write)))
  vals)

(defmethod generic-print ((vals <values>) stream)
  (let ((vs (get-values vals)))
    (unless (null? vs)
            (write-values vs stream generic-print)))
  vals)

(defun write-values (vs stream gfn)
  (gfn (car vs) stream)
  (when (cdr vs)
        (sprint stream nl)
        (write-values (cdr vs) stream gfn)))

(defconstant no-values (mv-values ()))

(defun values args
  (cond ((null? args) no-values)
        ((null? (cdr args)) (car args))
        (t (mv-values args))))

(defun call-with-values (producer consumer)
  (mv-call (producer) consumer))

(defgeneric mv-call (product consumer))

(defmethod mv-call ((product <values>) consumer)
  (apply consumer (get-values product)))

(defmethod mv-call (product consumer)
  (consumer product))

;; sets vars to be values
(defmacro multiple-value-setq (varlist values-form)
  `(let ((vals ,values-form))
     (if (values? vals)
         (let ((vallist (get-values vals)))
           ,@(do-m-v-s varlist 0))
       (setq ,(car varlist) vals))
     ,(car varlist)))

(defun do-m-v-s (varlist n)
  (if (null? varlist)
      ()
    (cons `(setq ,(car varlist) (list-ref vallist ,n))
          (do-m-v-s (cdr varlist) (+ n 1)))))

;; returns a list of the values
(defmacro multiple-value-list (values-form)
  `(let ((vals ,values-form))
     (if (values? vals)
         (get-values vals)
       (list vals))))

;; calls fn with all the values of forms as args
(defmacro multiple-value-call (fn . forms)
  (if (null? (cdr forms))
      `(let ((args ,(car forms)))
         (apply ,fn
                (if (values? args)
                    (get-values args)
                  (list args))))    ; can't simplify to (,fn ,args)
    `(let* ((mv-args (list ,@forms)))
       (apply ,fn (collect-args mv-args)))))

(defun collect-args (mv-args)
  (cond ((null? mv-args) ())
        ((values? (car mv-args))
         (append (get-values (car mv-args)) (collect-args (cdr mv-args))))
        (t (cons (car mv-args) (collect-args (cdr mv-args))))))

;; returns elts of list as multiple values
(defun values-list (l)
  (apply values l))

(defmacro multiple-value-bind (varlist values-form . body)
  `(call-with-values
    (lambda () ,values-form)
    (lambda ,varlist ,@body)))

;; (defun split (ls)
;;   (if (or (null? ls) (null? (cdr ls)))
;;       (values ls ())
;;     (call-with-values
;;      (lambda () (split (cddr ls)))
;;      (lambda (odds evens)
;;        (values (cons (car ls) odds)
;;                (cons (cadr ls) evens))))))

;;;-----------------------------------------------------------------------------
)  ;; End of module values
;;;-----------------------------------------------------------------------------
