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
;;; Title: access to module bindings
;;;  Library: eval (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule p-env
  (syntax (_syntax-1)
   import (i-all
           sx-obj)
   export (get-lexical-binding
           set-lexical-binding
           get-external-binding
           set-external-binding
           get-syntax-binding
           set-syntax-binding
           ;set-interactive-macro-binding
           get-local-static-binding
           add-local-static-bindings
           get-module
           get-syntax-module
           new-module
           new-syntax-module
           find-module
           find-syntax-module
           loaded-modules
           loaded-syntax-modules))

;;;-----------------------------------------------------------------------------
;;; Lexical environment
;;;-----------------------------------------------------------------------------
(defun get-lexical-binding (name . modules)
  (let ((search-modules (or modules
                            (and (dynamic *actual-module*)
                                 (list (dynamic *actual-module*)))))
        (res ()))
    (member-list name search-modules
                 (lambda (name module)
                   (let* ((env (module-lexical-env? module))
                          (binding (env name)))
                     (if binding
                         (let ((proper-binding
                                (expand-hard-coded-level-1-binding
                                 binding name env)))
                           (setq res proper-binding))
                       ()))))
    (if res
        (let ((obj (binding-obj? res)))
          (if (var? obj)
              (var-used! obj (+ (var-used? obj) 1))
            ()))
      ())
    res))

(defun set-lexical-binding (binding . name)
  (let ((module (dynamic *actual-module*)))
    (if module
        (let* ((env (module-lexical-env? module))
               (proper-binding
                (expand-hard-coded-level-1-binding binding name env))
               (binding-name (binding-local-name? proper-binding))
               (proper-module-name (save-binding-module-name? proper-binding))
               (old-lexical-binding (get-lexical-binding binding-name))
               (old-syntax-binding (get-syntax-binding binding-name)))
          (cond
            ((binding? old-lexical-binding)
             (let ((old-module-name
                    (save-binding-module-name? old-lexical-binding))
                   (old-index (binding-local-index? old-lexical-binding)))
               (if (or *redefine-imported-bindings*
                       (eq old-module-name proper-module-name))
                   (progn
                     ;; reuse module and index of old binding
                     (binding-module! proper-binding old-module-name)
                     (binding-local-index! proper-binding old-index))
                 (ct-warning
                  () "new lexical binding ~a shadows lexical binding ~a"
                  proper-binding old-lexical-binding))))
            ((and (binding? old-syntax-binding)
                  (null? (eq old-lexical-binding old-syntax-binding))
                  (null? *interpreter*))
             (ct-warning
              () "new lexical binding ~a is shadowed by syntax binding ~a"
              proper-binding old-syntax-binding)))
          ((setter (module-lexical-env? module))
           binding-name proper-binding)
          (if (null? *interpreter*) ()
            ((setter (module-interactive-lexical-env? module))
             binding-name proper-binding)))
      (error <ct-error> "no actual module"))))

;;;-----------------------------------------------------------------------------
;;; External environment
;;;-----------------------------------------------------------------------------
(defun get-external-binding (name . modules)
  (let ((search-modules (or modules
                            (and (dynamic *actual-module*)
                                 (list (dynamic *actual-module*)))))
        (res ()))
    (member-list name search-modules
                 (lambda (name module)
                   (let* ((env (module-external-env? module))
                          (binding (env name)))
                     (if binding
                         (let ((proper-binding
                                (expand-hard-coded-level-1-binding
                                 binding name env)))
                           (setq res proper-binding))
                       ()))))
    res))

(defun set-external-binding (binding . name)
  (let ((module (dynamic *actual-module*)))
    (if module
        (let* ((env (module-external-env? module))
               (proper-binding
                (expand-hard-coded-level-1-binding binding name env))
               (binding-name (binding-local-name? proper-binding))
               (proper-module-name (save-binding-module-name? proper-binding))
               (old-binding (get-external-binding binding-name)))
          (if (binding? old-binding)
              (let ((old-module-name (save-binding-module-name? old-binding))
                    (old-index (binding-local-index? old-binding)))
                (if (or (null? (eq old-module-name proper-module-name))
                        (eq old-module-name (module-name? module)))
                    (ct-warning
                     () "conflicting exported bindings ~a and ~a"
                     proper-binding old-binding)
                  ()))
            ())
          ((setter (module-external-env? module))
           binding-name proper-binding))
      (error <ct-error> "no actual module specified"))))

;;;-----------------------------------------------------------------------------
;;; Syntax environment
;;;-----------------------------------------------------------------------------
(defextern dynamic-level-1-syntax-binding-info (ptr) ptr
           "eul_dyn_level_1_syntax_binding_info")

(defun get-syntax-binding (name . modules)
  (let ((search-modules (or modules
                            (and (dynamic *actual-module*)
                                 (list (dynamic *actual-module*)))))
        (res ()))
    (member-list name search-modules
                 (lambda (name module)
                   (let* ((env (module-syntax-env? module))
                          (binding (env name)))
                     (if binding
                         (let ((proper-binding
                                (expand-interpreter-defined-syntax-binding
                                 (expand-hard-coded-level-1-binding
                                  binding name env)
                                 module)))
                           (setq res proper-binding))
                       ()))))
    res))

(defun set-syntax-binding (binding . name)
  (let ((module (dynamic *actual-module*)))
    (if module
        (let* ((env (module-syntax-env? module))
               (proper-binding
                (expand-interpreter-defined-syntax-binding
                 (expand-hard-coded-level-1-binding binding name env)
                 module))
               (proper-module-name (save-binding-module-name? proper-binding))
               (binding-name (binding-local-name? proper-binding))
               (old-lexical-binding (get-lexical-binding binding-name))
               (old-syntax-binding (get-syntax-binding binding-name)))
          (cond
            ((binding? old-syntax-binding)
             (let ((old-module-name
                    (save-binding-module-name? old-syntax-binding))
                   (old-index (binding-local-index? old-syntax-binding)))
               (if (or *redefine-imported-bindings*
                       (eq old-module-name proper-module-name))
                   (progn
                     ;; reuse module and index of old binding
                     (binding-module! proper-binding old-module-name)
                     (binding-local-index! proper-binding old-index))
                 (ct-warning
                  () "new syntax binding ~a shadows syntax binding ~a"
                  proper-binding old-syntax-binding))))
            ((and (binding? old-lexical-binding)
                  (null? (eq old-lexical-binding binding)))
             (ct-warning
              () "new syntax binding ~a shadows lexical binding ~a"
              proper-binding old-lexical-binding)))
          ((setter (module-syntax-env? module))
           binding-name proper-binding))
      (error <ct-error> "no actual module specified"))))

;;;-----------------------------------------------------------------------------
;;; Expand hard-code level-1 bindings
;;;-----------------------------------------------------------------------------
(defun expand-hard-coded-level-1-binding (binding name env)
  ;; Hard-coded level-1 bindings may still be not expanded
  (if (cons? binding)
      (let* ((pos (car binding))
             (origin-module-name (car (cdr binding)))
             (origin-name (cdr (cdr binding)))
             (binding-name (if (cons? name)
                               (car name)
                             (or name origin-name)))
             (proper-binding (make <interface-binding>
                                   local-name: binding-name
                                   module: origin-module-name
                                   obj: origin-name
                                   imported: t
                                   local-index: pos)))
        ((setter env) binding-name proper-binding)
        proper-binding)
    ;; binding not hard-coded or ()
    binding))

;;;-----------------------------------------------------------------------------
;;; Expand interpreter defined syntax bindings (i.e. defsyntax in interpreter)
;;;-----------------------------------------------------------------------------
(defun expand-interpreter-defined-syntax-binding (binding module)
  (if (and *interpreter* (symbol? binding))
      ((module-lexical-env? module) binding)
    binding))

;;;-----------------------------------------------------------------------------
;;; Access to modules
;;;-----------------------------------------------------------------------------
(defun get-module (name) (*get-loaded-module* name))

(defun get-syntax-module (name) (*get-loaded-syntax-module* name))

(defun loaded-modules ()
  (access-table-values *get-loaded-module*))

(defun loaded-syntax-modules ()
  (access-table-values *get-loaded-syntax-module*))

;  (defun loaded-non-syntax-modules ()
;    (binary- (loaded-modules) (loaded-syntax-modules)))

;  (defun loaded-module-names ()
;    (do1-list module-name? (access-table-values *get-loaded-module*)))

;  (defun loaded-syntax-module-names ()
;    (do1-list module-name? (access-table-values *get-loaded-syntax-module*)))

;  (defun loaded-non-syntax-module-names ()
;    (binary- (loaded-module-names) (loaded-syntax-module-names)))

;  (defun all-used-module-names-of-loaded-non-syntax-modules ()
;    (let ((modules (loaded-non-syntax-modules))
;         (res ()))
;      (do1-list
;       (lambda (module)
;        (setq res (binary+ res (module-all-used-module-names? module))))
;       modules)
;      res))

(defun new-module (name module)
  (if ()  ; (get-module name)
      (ct-warning () "recompilation of module ~a" name)
    ((setter *get-loaded-module*) name module)))

(defun new-syntax-module (name module)
  (if (get-syntax-module name)
      (ct-warning () "reloading of syntax module ~a" name)
    ((setter *get-loaded-syntax-module*) name module)))

(defun find-module (name)
  ;(notify0 "Loaded modules: ~a" (loaded-module-names))
  (or (get-module name) (compile-module name)))

(defun find-syntax-module (name)
  ;(notify0 "Loaded modules: ~a" (loaded-module-names))
  (or (get-module name)
      (get-syntax-module name)
      (load-syntax-module name)))

;;;-----------------------------------------------------------------------------
;;; Access to local static bindings
;;  Local variables are always linked to bindings to be conform with
;;  global lexical binding search.
;;;-----------------------------------------------------------------------------
(defun get-local-static-binding (name env)
  (let ((entry (assoc-list-ref env name)))
    (if entry
        (let* ((binding (cdr entry))
               (obj (binding-obj? binding)))
          (if (var? obj)
              (var-used! obj (+ (var-used? obj) 1))
            ())
          binding)
      ())))

(defun add-local-static-bindings (bindings env)
  (let ((new-entries
         (map1-list (lambda (binding)
                      (cons (binding-local-name? binding) binding))
                    bindings)))
    (append new-entries env)))

;;;-----------------------------------------------------------------------------
)  ;; End of module p-env
;;;-----------------------------------------------------------------------------
