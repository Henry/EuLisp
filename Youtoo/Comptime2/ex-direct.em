;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: expanding import/export dirctives into syntax nodes
;;;-----------------------------------------------------------------------------
(defmodule ex-direct
  (syntax (_macros _i-aux0)
   import (i-all p-env sx-obj sx-node ex-import ex-syntax ex-expose)
   export (expand-directive expand-export))

;;;-----------------------------------------------------------------------------
;;; Module directive expander
;;;-----------------------------------------------------------------------------
(defconstant get-directive-expander (make-access-table))

(defun install-directive-expander (key fun)
  (let ((x (get-directive-expander key)))
    (and x
         (ct-warning () "redefinition of expander ~a"  key))
    ((setter get-directive-expander) key fun)))

(defun directive-expander (x e)
  (let ((expander
         (cond
           ((null? (cons? x)) (lambda (x e) x))
           ((symbol? (car x))
            (let ((directive-expander (get-directive-expander (car x))))
              (if directive-expander
                  directive-expander
                (error "no directive expander ~a available" x))))
           (t
            (error "no directive expander ~a available" x)))))
    (expander x e)))

(defun expand-directive (x)
  (with-ct-handler "bad directive syntax" x
                   (directive-expander x directive-expander)))

;;;-----------------------------------------------------------------------------
;;; Install IMPORT expander
;;;-----------------------------------------------------------------------------
(install-directive-expander 'import
                            (lambda (x e)
                              (map1-list expand-import (cadr x))
                              (e (cdr (cdr x)) e)))

;;;-----------------------------------------------------------------------------
;;; Install EXPORT expander
;;;-----------------------------------------------------------------------------
(install-directive-expander 'export
                            (lambda (x e)
                              (map1-list (lambda (name)
                                           ((setter (module-external-env? (dynamic *actual-module*)))
                                            name name))
                                         (cadr x))
                              (e (cdr (cdr x)) e)))

;;;-----------------------------------------------------------------------------
;;; Some bindings (which are not exposed) have to be set in the external
;;; environment. Only the keys (names) do exist yet.
;;;-----------------------------------------------------------------------------
(defun expand-export (module)
  (access-table-do
   (lambda (name binding)
     ;; Attention -- name is ptr to C string!
     (if (binding? binding)
         ()  ; exposed binding -- nothing to do
       (let ((new-binding (get-lexical-binding binding)))
         (if new-binding
             (let ((obj (binding-obj? new-binding)))
               (set-external-binding new-binding)
               (if (fun? obj)
                   (fun-has-unknown-appls! obj t)
                 ()))
           (ct-serious-warning () "exported lexical binding ~a not available"
                               binding)))))
   (module-external-env? module)))

;;;-----------------------------------------------------------------------------
;;; Install EXPOSE expander
;;;-----------------------------------------------------------------------------
(install-directive-expander 'expose
                            (lambda (x e)
                              (map1-list expand-expose (cadr x))
                              (e (cdr (cdr x)) e)))

;;;-----------------------------------------------------------------------------
;;; Install SYNTAX expander
;;;-----------------------------------------------------------------------------
(install-directive-expander 'syntax
                            (lambda (x e)
                              (map1-list expand-syntax-import (cadr x))
                              (e (cdr (cdr x)) e)))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
