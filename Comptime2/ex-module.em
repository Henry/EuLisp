;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;;  Description: pre-expand defining and top-level forms
;;; -----------------------------------------------------------------------
(defmodule ex-module
  (syntax (_macros _i-aux0 _sx-obj0 _ex-aux0)
   import (i-all p-env sx-node sx-obj ex-import ex-syntax ex-direct ex-expr
           cg-dld)
   export (expand-module expand-export))
;;; ---------------------------------------------------------------------
;;; Top-level MODULE expander
;;; ---------------------------------------------------------------------
  (defconstant get-module-expander (make-access-table))
  (defun expand-module (x)
    (module-expander x module-expander))
  (defun install-module-expander (key fun)
    (let ((x (get-module-expander key)))
      (and x
           (ct-warning () "redefinition of expander ~a"  key))
      ((setter get-module-expander) key fun)))
  (defun module-expander (x e)
    (let* ((key (and (consp x) (car x)))
           (expander
            (cond
             ((symbolp key) (or (get-module-expander key)
                                (get-top-level-macro-expander key)
                                (get-top-level-form-collector key)))
             (t (get-top-level-form-collector key)))))
      (expander x e)))
;;; --------------------------------------------------------------------
;;; MACRO expander
;;; --------------------------------------------------------------------
  (defun get-top-level-macro-expander (key)
    (let ((binding (get-syntax-binding key)))
      (and binding 
           (let ((macro-fun (as-dynamic-binding binding)))
             (and macro-fun
                  (lambda (x e)
                    (with-ct-handler (protect-tilde
                                      (format
                                       () "bad macro expansion of ~a"
                                       (cons key (cdr x)))) macro-fun
                      (notify0 "APPLY MACRO: ~a" (cons key (cdr x)))
                      (let ((macro-expanded-form (apply macro-fun (cdr x))))
                        (notify0 "RESULT: ~a" macro-expanded-form)
                        (e macro-expanded-form e)))))))))
  
;;; ---------------------------------------------------------------------
;;; Collect TOP-LEVEL FORMS
;;; ---------------------------------------------------------------------
  (defun get-top-level-form-collector (key)
    (lambda (x e)
      (new-node x 'top-level-form)))
;;; --------------------------------------------------------------------
;;; Install DEFMODULE expander
;;; --------------------------------------------------------------------
  (install-module-expander 'defmodule
    (lambda (x e)
      (with-ct-handler (format () "bad defmodule syntax ~a ..."
                               (list (car x) (cadr x) (caddr x))) x
          ;;-----------------------------------------------------------------
          ;; create empty module
          ;;-----------------------------------------------------------------
          (let* ((module-name (get-name x))
               (m (make-module (get-name x))))
            (dynamic-let ((*actual-module* m))
              (let ((d (get-directives x)))
                (and (null (eq module-name *tmp-source-file-name*))
                     (ct-serious-warning () "bad module name ~a in file ~a.em"
                                         module-name *tmp-source-file-name*))
                (expand-directive d)
                (do1-list (lambda (form) (e form e))
                          (get-top-level-forms x))))
            m))))
;;; --------------------------------------------------------------------
;;; Install EXPORT expander
;;; --------------------------------------------------------------------
  (install-module-expander 'export
    (lambda (x e)
      (with-ct-handler "bad export syntax" x
        (let ((module (dynamic *actual-module*)))
          (do1-list
           (lambda (name)
             (if (eq name above:)       ; export what's defined so far
                 (let ((module-name (module-name? module)))
                   (access-table-do
                    (lambda (binding-name binding)
                      ;; Attention -- binding-name is ptr to C string!
                      (if (binding-imported? binding) ()
                          (set-external-binding binding)))
                    (module-lexical-env? module)))
               ;; binding may not yet be available
               ((setter (module-external-env? module)) name name)))
           (cdr x))))))
;;; --------------------------------------------------------------------
;;; Install PROGN Expander
;;; --------------------------------------------------------------------
  (install-module-expander 'progn
    (lambda (x e) (map1-list (lambda (x) (e x e)) (cdr x))))
;;; --------------------------------------------------------------------
;;; Install DEFCONSTANT expander
;;; --------------------------------------------------------------------
  (install-module-expander 'defconstant
    (lambda (x e)
      (with-ct-handler "bad defconstant syntax" x
        (let ((v (get-value x)))
          (if (and (consp v) (eq (car v) 'lambda))
              (make-defined-fun (get-name x)
                                (get-lambda-params v)
                                (get-lambda-body v))
            (make-named-const (get-name x) v))))))
;;; --------------------------------------------------------------------
;;; Install DEFLOCAL expander
;;; --------------------------------------------------------------------
  (install-module-expander 'deflocal
    (lambda (x e)
      (with-ct-handler "bad deflocal syntax" x
        (make-global-var
         (get-name x) 
         (if (cddr x)
             (get-value x)
           (ct-warning () "variable ~a not initialized" (get-name x)))))))
;;; --------------------------------------------------------------------
;;; Install DEFUN expander
;;; --------------------------------------------------------------------
  (install-module-expander 'defun
    (lambda (x e)
      (with-ct-handler "bad defun syntax" x
        (let ((name (get-name x))
              (params (get-params x))
              (body (get-body x)))
          (if (symbolp name)
              (if *interpreter*
                  ;; This returns the function
                  (e `(deflocal ,name (named-lambda ,name ,params ,@body)) e)
                ;; This returns nil
                (make-defined-fun name params body))
            (if (eq (car name) 'setter)
                (let ((binding (get-lexical-binding (cadr name))))
                  (if (and binding
                           (let ((obj (binding-obj? binding)))
                             (and (funp obj)
                                  (lambda-inlined? obj))))
                      (make-inlined-setter name params body)
                    ())
                  (e `((setter setter) ,(cadr name)
                       (named-lambda ,name ,params ,@body)) e))
              (error "bad defun syntax" <condition>)))))))
;;; --------------------------------------------------------------------
;;; Install DEFMACRO expander
;;; --------------------------------------------------------------------
  (install-module-expander 'defmacro
    (lambda (x e)
      (with-ct-handler "bad defmacro syntax" x
        (let ((name (get-name x))
              (params (get-params x))
              (body (get-body x)))
          (if *interpreter*
              ;; Extend lexical env and return function in interpreter
              (let ((node (e `(deflocal ,name
                                (named-lambda ,name ,params ,@body)) e))
                    (binding (get-lexical-binding name)))
                (set-syntax-binding binding)
                node)
            ;; Extend lexical and external envs
            (let ((node (make-defined-fun name params body)))
               ;; Attention: lazy expansion of external bindings
              ((setter (module-external-env? (dynamic *actual-module*)))
               name name)))))))
;;; --------------------------------------------------------------------
;;; Install DEFOPENCODED expander
;;; --------------------------------------------------------------------
  (install-module-expander 'defopencoded
    (lambda (x e)
      (with-ct-handler (format () "bad defopencoded syntax ~a" (get-name x)) x
        (make-defined-opencoded-fun
         (get-name x) (get-params x) (get-body x)))))
;;; --------------------------------------------------------------------
;;; Install DECLARE-INLINE expander
;;; --------------------------------------------------------------------
  (install-module-expander 'declare-inline
    ;; Refers to earlier defun
    (lambda (x e)
      (if (= *inline-level* 0) ()
        (with-ct-handler (format () "bad inline declaration ~a" x) x
          (let* ((binding (or (get-lexical-binding (cadr x))
                              (ct-serious-warning
                               (make-dummy-binding (cadr x))
                               "no lexical binding ~a available" (cadr x))))
                 (obj (binding-obj? binding)))
            (if (lambdap obj)
                (progn
                  (lambda-inlined! obj t)
                  (new-node binding 'inlined-lambda))
              (if *interpreter* ()
                (ct-warning
                 () "bad inline argument ~a; or tried to inline import"
                 (cadr x)))))))))
;;; --------------------------------------------------------------------
;;; Install DEFEXTERN expander
;;; --------------------------------------------------------------------
  (install-module-expander 'defextern
    (lambda (x e)
      (with-ct-handler (format () "bad defextern syntax ~a" x) x
        (if *interpreter*
            (ct-warning () "interpreter does not support defextern")
          (make-defined-external-fun (get-name x)
                                     (get-params x)         ; arg converter
                                     (car (get-body x))     ; res converter
                                     (cdr (get-body x))))))); opt ext name
)  ; end of module
