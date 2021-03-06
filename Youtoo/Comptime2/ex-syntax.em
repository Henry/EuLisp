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
;;; Title: expanding syntax import dirctives into syntax nodes
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule ex-syntax
  (syntax (_syntax-1
           _i-aux0
           _sx-obj0)
   import (i-all
           i-modify
           p-env
           sx-obj
           sx-node
           cg-interf)
   export (expand-old-syntax-imports
           expand-syntax-import
           import-syntax-module))

;;;-----------------------------------------------------------------------------
;;; Syntax import expander
;;;-----------------------------------------------------------------------------
(defconstant get-syntax-import-expander (make-access-table))

(defun install-syntax-import-expander (key fun)
  (let ((x (get-syntax-import-expander key)))
    (and x
         (ct-warning () "redefinition of expander ~a"  key))
    ((setter get-syntax-import-expander) key fun)))

(defun syntax-import-expander (x e)
  (let ((expander
         (cond
           ((symbol? x) (lambda (x e) (import-syntax-module x)))
           ((null? (cons? x)) (lambda (x e) x))
           ((symbol? (car x))
            (let ((sx-import-expander (get-syntax-import-expander (car x))))
              (or sx-import-expander
                  (error <condition>
                         (fmt "no syntax-import expander ~a available" x)))))
           (t
            (error <condition>
                   (fmt "no syntax-import expander ~a available" x))))))
    (expander x e)))

(defun expand-syntax-import (x)
  (syntax-import-expander x syntax-import-expander))

(defun expand-old-syntax-imports (x)
  (map1-list expand-syntax-import x))

(defun import-syntax-module (name)
  (notify0 "  Import syntax module ~a ..." name)
  (with-ct-handler (fmt "cannot import syntax module ~a" name)
                   (dynamic *actual-module*)
                   (let ((module (or (module? name) (find-syntax-module name))))
                     (access-table-do
                      (lambda (key binding)
                        ;; Attention -- key is ptr to C string!
                        (if (cons? binding)
                            ;; Binding yet unexpanded
                            (let ((binding-name (c-string-as-eul-symbol key)))
                              (set-syntax-binding binding binding-name))
                          (progn
                            (set-syntax-binding binding)
                            (binding-imported! binding t))))
                      (module-external-env? module))
                     (register-imported-syntax-module module))))

(defun import-syntax-binding (name module)
  (let ((binding
         (or (get-external-binding name module)
             (ct-serious-warning
              (make-dummy-binding name)
              "external syntax binding ~a not available in module"
              name module))))
    (binding-imported! binding t)
    (register-imported-syntax-module module)
    binding))

(defun register-imported-syntax-module (module)
  (let ((name (if (module? module) (module-name? module) module)))
    (labels
     ((loop (l)
            (if (null? l)
                (new-node module 'used-syntax-module)
              (let ((m (car l)))
                (if (module? m)
                    (if (eq m module)
                        m
                      (loop (cdr l)))
                  (if (eq m name)
                      ;; Replace the symbol in the module list
                      ((setter car) l module)
                    (loop (cdr l))))))))
     (loop (module-used-syntax-modules? (dynamic *actual-module*))))))

;;;-----------------------------------------------------------------------------
;;; Install syntax-import expanders
;;;-----------------------------------------------------------------------------
(install-syntax-import-expander
 'only
 (lambda (x e)
   (with-ct-handler
    "bad syntax only syntax" x
    (let ((module (find-syntax-module (caddr x))))
      (do1-list (lambda (name)
                  (set-syntax-binding (import-syntax-binding name module)))
                (cadr x))))))

(install-syntax-import-expander
 'except
 (lambda (x e)
   (with-ct-handler
    "bad syntax except syntax" x
    (let* ((module (find-syntax-module (caddr x)))
           (external-names
            (map1-list save-binding-local-name?
                       (access-table-values
                        (module-external-env? module))))
           (names (binary- external-names (cadr x))))
      (do1-list (lambda (name)
                  (set-syntax-binding (import-syntax-binding name module)))
                names)))))

(install-syntax-import-expander
 'rename
 (lambda (x e)
   (with-ct-handler
    "bad syntax rename syntax" x
    (let* ((module (find-syntax-module (caddr x)))
           (env (module-external-env? module))
           (external-names
            (map1-list save-binding-local-name?
                       (access-table-values env)))
           (other-names
            (binary- external-names (map1-list car (cadr x)))))
      (do1-list (lambda (name)
                  (set-syntax-binding (import-syntax-binding name module)))
                other-names)
      (do1-list (lambda (name-pair)
                  (let* ((name (car name-pair))
                         (binding (import-syntax-binding name module))
                         (new-binding (clone-node binding)))
                    (binding-local-name! new-binding (cadr name-pair))
                    (set-syntax-binding new-binding)))
                (cadr x))))))

(defun make-prefix (pfx name)
  (convert (concatenate (symbol-name pfx) (symbol-name name)) <symbol>))

(install-syntax-import-expander
 'prefix
 (lambda (x e)
   (with-ct-handler
    "bad syntax prefix syntax" x
    (let* ((module (find-syntax-module (cadddr x)))
           (env (module-external-env? module))
           (prefix (cadr x))
           (external-names
            (map1-list save-binding-local-name?
                       (access-table-values env)))
           (other-names
            (binary- external-names (caddr x))))
      (do1-list (lambda (name)
                  (set-syntax-binding (import-syntax-binding name module)))
                other-names)
      (do1-list (lambda (external-name)
                  (let* ((binding (import-syntax-binding external-name module))
                         (new-binding (clone-node binding)))
                    (binding-local-name! new-binding
                                         (make-prefix prefix external-name))
                    (set-syntax-binding new-binding)))
                (caddr x))))))

;;;-----------------------------------------------------------------------------
)  ;; End of module ex-syntax
;;;-----------------------------------------------------------------------------
