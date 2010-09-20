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
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: expanding import dirctives into syntax nodes
;;;-----------------------------------------------------------------------------

(defmodule ex-import
  (syntax (_macros
           _i-aux0
           _sx-obj0)
   import (i-all
           sx-obj
           sx-node
           p-env
           cg-interf)
   export (expand-old-imports
           expand-import import-module
           register-imported-module))

;;;-----------------------------------------------------------------------------
;;; Import expander
;;;-----------------------------------------------------------------------------
(defconstant get-import-expander (make-access-table))

(defun install-import-expander (key fun)
  (let ((x (get-import-expander key)))
    (and x
         (ct-warning () "redefinition of expander ~a"  key))
    ((setter get-import-expander) key fun)))

(defun import-expander (x e)
  (let ((expander
         (cond
           ((symbol? x) (lambda (x e) (import-module x)))
           ((null? (cons? x)) (lambda (x e) x))
           ((symbol? (car x))
            (let ((import-expander (get-import-expander (car x))))
              (if import-expander
                  import-expander
                (error <condition> (fmt "no import expander ~a available" x)))))
           (t
            (error <condition> (fmt "no import expander ~a available" x))))))
    (expander x e)))

(defun expand-import (x) (import-expander x import-expander))

(defun expand-old-imports (x) (map1-list expand-import x))

(defun import-module (name)
  (notify0 "  Import module ~a ..." name)
  (with-ct-handler (fmt "cannot import lexical module ~a" name)
                   (dynamic *actual-module*)
                   (let ((module (or (module? name) (find-imported-module name))))
                     (access-table-do
                      (lambda (key binding)
                        ;; Attention -- key is ptr to C string!
                        (if (cons? binding)
                            ;; Binding yet unexpanded
                            (let ((binding-name (c-string-as-eul-symbol key)))
                              (set-lexical-binding binding binding-name))
                          (progn
                            (set-lexical-binding binding)
                            (binding-imported! binding t))))
                      (module-external-env? module))
                     (if (module? name) ()
                       (register-imported-module module))
                     module)))

(defun import-binding (name module)
  (let ((binding
         (or (get-external-binding name module)
             (ct-serious-warning
              (make-dummy-binding name)
              "external binding ~a not available in module ~a"
              name module))))
    (binding-imported! binding t)
    (register-imported-module module)
    binding))

(defun register-imported-module (module)
  (let ((actual-module (dynamic *actual-module*))
        (module-name (module-name? module)))
    (if (member1-list module-name (module-used-module-names? actual-module))
        ()
      (let ((names (module-all-used-module-names? actual-module))
            (new-names (module-all-used-module-names? module)))
        (if (member1-list module-name (get-library-names)) ()
          (setq new-names (cons module-name new-names)))
        (new-node module-name 'used-module-name)
        (module-all-used-module-names! actual-module
                                       (binary+ names new-names))))))

;;;-----------------------------------------------------------------------------
;;; Install ONLY IMPORT expander
;;;-----------------------------------------------------------------------------
(install-import-expander 'only
                         (lambda (x e)
                           (with-ct-handler "bad only import syntax" x
                                            (let* ((module-name (caddr x))
                                                   (module (find-imported-module module-name)))
                                              (do1-list (lambda (name)
                                                          (set-lexical-binding (import-binding name module)))
                                                        (cadr x))
                                              (new-node module-name 'used-module-name)))))
;;;-----------------------------------------------------------------------------
;;; Install EXCEPT IMPORT expander
;;;-----------------------------------------------------------------------------
(install-import-expander 'except
                         (lambda (x e)
                           (with-ct-handler "bad except import syntax" x
                                            (let* ((module-name (caddr x))
                                                   (module (find-imported-module module-name))
                                                   (external-names
                                                    (map1-list save-binding-local-name?
                                                               (access-table-values (module-external-env? module))))
                                                   (names (binary- external-names (cadr x))))
                                              (do1-list (lambda (name)
                                                          (set-lexical-binding (import-binding name module)))
                                                        names)
                                              (new-node module-name 'used-module-name)))))

;;;-----------------------------------------------------------------------------
;;; Install RENAME IMPORT expander
;;;-----------------------------------------------------------------------------
(install-import-expander 'rename
                         (lambda (x e)
                           (with-ct-handler "bad rename import syntax" x
                                            (let* ((module-name (caddr x))
                                                   (module (find-imported-module module-name))
                                                   (env (module-external-env? module))
                                                   (external-names (map1-list save-binding-local-name?
                                                                              (access-table-values env)))
                                                   (other-names
                                                    (binary- external-names (map1-list car (cadr x)))))
                                              (do1-list (lambda (name)
                                                          (set-lexical-binding (import-binding name module)))
                                                        other-names)
                                              (do1-list (lambda (name-pair)
                                                          (let* ((binding (import-binding (car name-pair) module))
                                                                 (new-binding (clone-node binding)))
                                                            (binding-local-name! new-binding (cadr name-pair))
                                                            (set-lexical-binding new-binding)))
                                                        (cadr x))
                                              (new-node module-name 'used-module-name)))))

(defun make-prefix (pfx name)
  (convert (concatenate (symbol-name pfx) (symbol-name name)) <symbol>))

(install-import-expander 'prefix
                         (lambda (x e)
                           (with-ct-handler "bad prefix import syntax" x
                                            (let* ((module-name (cadddr x))
                                                   (module (find-imported-module module-name))
                                                   (env (module-external-env? module))
                                                   (prefix (cadr x))
                                                   (external-names (map1-list save-binding-local-name?
                                                                              (access-table-values env)))
                                                   (other-names
                                                    (binary- external-names (caddr x))))
                                              (do1-list (lambda (name)
                                                          (set-lexical-binding (import-binding name module)))
                                                        other-names)
                                              (do1-list (lambda (external-name)
                                                          (let* ((binding (import-binding external-name module))
                                                                 (new-binding (clone-node binding)))
                                                            (binding-local-name! new-binding
                                                                                 (make-prefix prefix
                                                                                              external-name))
                                                            (set-lexical-binding new-binding)))
                                                        (caddr x))
                                              (new-node module-name 'used-module-name)))))

;;;-----------------------------------------------------------------------------
)  ;; End of module ex-import
;;;-----------------------------------------------------------------------------
