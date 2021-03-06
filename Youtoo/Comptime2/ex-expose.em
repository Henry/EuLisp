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
;;; Title: expanding expose dirctives into syntax nodes
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule ex-expose
  (syntax (_syntax-1
           _i-aux0)
   import (i-all
           p-env
           sx-obj
           sx-node
           cg-interf
           ex-import)
   export (expand-expose))

;;;-----------------------------------------------------------------------------
;;; Expose expander
;;;-----------------------------------------------------------------------------
(defconstant get-expose-expander (make-access-table))

(defun install-expose-expander (key fun)
  (let ((x (get-expose-expander key)))
    (and x
         (ct-warning () "redefinition of expander ~a"  key))
    ((setter get-expose-expander) key fun)))

(defun expose-expander (x e)
  (let ((expander
         (cond
           ((symbol? x) (lambda (x e) (expose-module x)))
           ((null? (cons? x)) (lambda (x e) x))
           ((symbol? (car x))
            (let ((expose-expander (get-expose-expander (car x))))
              (if expose-expander
                  expose-expander
                (error <condition> (fmt "no expose expander ~a available" x)))))
           (t
            (error <condition> (fmt "no expose expander ~a available" x))))))
    (expander x e)))

(defun expand-expose (x) (expose-expander x expose-expander))

(defun expose-module (name)
  (notify0 "  Expose module ~a ..." name)
  (let* ((module (find-imported-module name))
         (env (if (eq module (dynamic *actual-module*))
                  (module-lexical-env? module)
                (module-external-env? module))))
    (access-table-do
     (lambda (key value)
       ;; Attention -- key is ptr to C string!
       (set-external-binding value))
     env)
    (register-imported-module module)))

(defun expose-binding (name module)
  (let ((binding
         (or (if (eq module (dynamic *actual-module*))
                 (get-lexical-binding name module)
               (get-external-binding name module))
             (ct-serious-warning
              (make-dummy-binding name)
              "external binding ~a not available in module ~a"
              name module))))
    (register-imported-module module)
    binding))

;;;-----------------------------------------------------------------------------
;;; Install ONLY EXPOSE expanders
;;;-----------------------------------------------------------------------------
(install-expose-expander
 'only
 (lambda (x e)
   (with-ct-handler
    "bad expose only syntax" x
    (let* ((module (find-imported-module (caddr x))))
      (do1-list (lambda (name)
                  (set-external-binding (expose-binding name module)))
                (cadr x))))))

;;;-----------------------------------------------------------------------------
;;; Install EXCEPT EXPOSE expanders
;;;-----------------------------------------------------------------------------
(install-expose-expander
 'except
 (lambda (x e)
   (with-ct-handler
    "bad expose except syntax" x
    (let* ((module (find-imported-module (caddr x)))
           (env (if (eq module (dynamic *actual-module*))
                    (module-lexical-env? module)
                  (module-external-env? module)))
           (external-names
            (map save-binding-local-name? (access-table-values env)))
           (names (binary- external-names (cadr x))))
      (do1-list (lambda (name)
                  (set-external-binding (expose-binding name module)))
                names)))))

;;;-----------------------------------------------------------------------------
;;; Install RENAME EXPOSE expanders
;;;-----------------------------------------------------------------------------
(install-expose-expander
 'rename
 (lambda (x e)
   (with-ct-handler
    "bad expose rename syntax" x
    (let* ((module (find-imported-module (caddr x)))
           (env (if (eq module (dynamic *actual-module*))
                    (module-lexical-env? module)
                  (module-external-env? module)))
           (external-names
            (map save-binding-local-name? (access-table-values env)))
           (other-names
            (binary- external-names (map1-list car (cadr x)))))
      (do1-list (lambda (name)
                  (set-external-binding (expose-binding name module)))
                other-names)
      (do1-list (lambda (name-pair)
                  (let* ((binding (expose-binding (car name-pair) module))
                         (new-binding (clone-node binding)))
                    (binding-local-name! new-binding (cadr name-pair))
                    (set-external-binding new-binding)))
                (cadr x))))))

(defun make-prefix (pfx name)
  (convert (concatenate (symbol-name pfx) (symbol-name name)) <symbol>))

(install-expose-expander
 'prefix
 (lambda (x e)
   (with-ct-handler
    "bad expose prefix syntax" x
    (let* ((module (find-imported-module (cadddr x)))
           (env (if (eq module (dynamic *actual-module*))
                    (module-lexical-env? module)
                  (module-external-env? module)))
           (prefix (cadr x))
           (external-names
            (map save-binding-local-name? (access-table-values env)))
           (other-names
            (binary- external-names (caddr x))))
      (do1-list (lambda (name)
                  (set-external-binding (expose-binding name module)))
                other-names)
      (do1-list (lambda (internal-name)
                  (let* ((binding (expose-binding internal-name module))
                         (new-binding (clone-node binding)))
                    (binding-local-name! new-binding
                                         (make-prefix prefix
                                                      internal-name))
                    (set-external-binding new-binding)))
                (caddr x))))))

;;;-----------------------------------------------------------------------------
)  ;; End of module ex-expose
;;;-----------------------------------------------------------------------------
