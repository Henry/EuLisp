;;; Copyright 1994-2010 Fraunhofer ISST
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'Eu2C'
;;;-----------------------------------------------------------------------------
;;
;;  Eu2C is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;;  Authors: Ingo Mohr
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

#module el2lzs-basic
(import (level-1
         el2lzs-error
         (only (first get)
               common-lisp)
         (only (find
                make-instance
                mapcar
                mapc
                warn
                remove)
               common-lisp)       ;; for literal expansion only
         expand-literal
         accessors)
 syntax (level-1
         (only (declare ignore)
               common-lisp)
         apply-standard             ; for literal expansion only
         (only (push);; for literal expansion only
               common-lisp)))

;;;-----------------------------------------------------------------------------
;;;  deftrans: defines a transformation for list-expressions
;;;-----------------------------------------------------------------------------

(defmacro define-transformation (name)
  `(progn
     (defgeneric ,name (source))
     (export ,name)
     (defmethod ,name ((expr <pair>))
       (if (check-syntax (get-trans-pattern ',(make-eulisp-symbol name) (car expr))
                         expr)
           (apply (get-trans-function ',(make-eulisp-symbol name)
                                      (car expr))
                  expr expr)              ; first expr is for _whole-form_
         ()))
     (defmacro ,(make-identifier (string-append "DEF" (symbol-name name)))
       (pattern result)
       (define-trans ',name pattern result))))

(defun define-trans (trans-function pattern result)
  (let ((trans-function (make-eulisp-symbol trans-function))
        (form-keyword (first pattern)))
    (if (symbol? form-keyword)
        (progn
          (setq form-keyword (make-eulisp-symbol (car pattern)))
          `(progn (setf (get ',form-keyword ',trans-function)
                        (cons
                         (lambda (_whole-form_ form-keyword ,@(cdr pattern))
                           (declare (ignore form-keyword))
                           _whole-form_ ; to avoid warnings by the CL compiler
                           ,result)
                         ',pattern))
                  (cons ',trans-function ',pattern)))
      `(progn (setf (get ^t ',trans-function)
                    (cons
                     (lambda (_whole-form_ ,(first form-keyword) ,@(cdr pattern))
                       _whole-form_ ; to avoid error messages by the compiler
                       ,result)
                     ()))
              (cons ',trans-function ',pattern)))))

(defun get-trans-function (trans-function key)
  (car (or (and (symbol? key)
                (get key trans-function))
           (get ^t trans-function))))

(defun get-trans-pattern (trans-function key)
  (cdr (or (and (symbol? key)
                (get key trans-function))
           (get ^t trans-function))))

(defmacro whole-form () '_whole-form_)

;;;-----------------------------------------------------------------------------
;;; check syntax
;;;-----------------------------------------------------------------------------

(defun check-syntax (pattern expr)
  (if (or (null? pattern
                )(and (cons? expr)
                      (check-syntax-components pattern expr)))
      t
    (progn (error-invalid-syntax pattern expr)
           ())))

(defun check-syntax-components (pattern expr)
  (cond ((null? pattern)
         (null? expr))
        ((atom? pattern)
         (true-list? expr))
        ((null? expr)
         ())
        ((atom? expr)
         ())
        (t
         (check-syntax-components (cdr pattern) (cdr expr)))))

(defun true-list? (l)
  (cond ((cons? l) (true-list? (cdr l)))
        ((null? l) t)
        (t ())))

;;;-----------------------------------------------------------------------------
;;; syntax for el2lzs-literals
;;;-----------------------------------------------------------------------------
(export
 expand-literal-using-desc
 <literal-expansion>
 get-literal-expander-arguments
 add-literal-expander
 reset-literal-expanders
 expanded-empty-list
 ?module ?expander ?expansions ?slots ?literal-class ?class
 *literal-expanders*)

(defmacro def-literal-class (literal-class class . slots)
  ;;expansion-type: single, multiple
  `(progn
     (push (make-instance <literal-expansion>
                          :literal-class ',(make-eulisp-symbol literal-class)
                          :class ,class
                          :slots (list ,@(mapcar (lambda (slot)
                                                   `(cons ',(make-eulisp-symbol (car slot))
                                                          ,(cdr slot)))
                                                 slots)))
           *literal-expanders*)
     (defmethod expand-literal ((literal ,class))
       (expand-literal-using-desc literal
                                  (find ,class *literal-expanders* :key #'?class)))))

(defgeneric expand-literal-using-desc (literal exp-desc))

;;;-----------------------------------------------------------------------------
;;; Literal Expanders

(deflocal *literal-expanders* ())      ;list of literal-expansion
(deflocal expanded-empty-list ())

(defstandardclass <literal-expansion> ()
  (literal-class :initarg :reader)
  (class :initarg :reader)
  (expander :accessor :initform ())
  (module :accessor :initarg)
  (expansions :accessor :initform ())
  (slots :initarg :reader))

(defun get-literal-expander-arguments (literal-class)
  (let ((exp-desc (find literal-class *literal-expanders* :key #'?literal-class)))
    (if (null? exp-desc)
        (warn "invalid literal class: ~A" literal-class)
      (mapcar #'car (?slots exp-desc)))))

(defun add-literal-expander (literal-class expander)
  ;; the walk over all expanders makes it possible to bind different classes to
  ;; the same literal class
  (let ((exp-desc-list (remove literal-class *literal-expanders*
                               :key #'?literal-class :test-not #'eq)))
    (if (null? exp-desc-list)
        (warn "invalid literal class: ~A" literal-class)
      (mapc (lambda (exp-desc)
              (setf (?expander exp-desc) expander)
              (setf (?module exp-desc) (dynamic *current-module*)))
            exp-desc-list))))

(defun reset-literal-expanders ()
  (mapc (lambda (exp-desc)
          (setf (?expander exp-desc) ())
          (setf (?module exp-desc) ())
          (setf (?expansions exp-desc) ()))
        *literal-expanders*)
  (setq expanded-empty-list ()))


#module-end
