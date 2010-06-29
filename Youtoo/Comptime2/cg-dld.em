;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: Dynamic binding access
;;;-----------------------------------------------------------------------------
(defmodule cg-dld
  (syntax (_macros _i-aux0)
   import (i-all sx-obj sx-node ex-import ex-syntax i-modify cg-interf p-env)
   export (dynamic-binding-ref dynamic-binding-set
           dynamic-load-module as-dynamic-binding
           module-loaded? as-C-module-name
           dynamic-binding-ref1 dynamic-binding-set1 dynamic-load-module1))

;;;-----------------------------------------------------------------------------
;;; Get and set an exported top-level lexical binding dynamically
;;;-----------------------------------------------------------------------------
  (defun dynamic-binding-ref (binding-name module-name)
    (let* ((module (or (get-module module-name)
                       (load-module-interface module-name)))
           (binding (or (get-syntax-binding binding-name module)
                        (get-lexical-binding binding-name module)
                        (get-external-binding binding-name module)
                        (error "binding ~a not available in module ~a"
                               binding-name module-name))))
      ;      (pprint binding) (flush)
      (as-dynamic-binding binding)))

  (defun as-dynamic-binding (binding)
    (let* ((index (binding-local-index? binding))
           (module (binding-module? binding))
           (origin-module-name (if (symbol? module)
                                   module
                                 (module-name? module))))
      (if (or (get-binding-info binding 'opencoding)
              (get-binding-info binding 'ff))
          (error "binding ~a not accessable in module ~a"
                 (binding-local-name? binding) origin-module-name)
        (dynamic-binding-ref1 (as-C-module-name origin-module-name) index))))

  (defun dynamic-binding-set (binding-name module-name x)
    (let* ((module (or (get-module module-name)
                       (load-module-interface module-name)))
           (binding (or (get-syntax-binding binding-name module)
                        (get-lexical-binding binding-name module)
                        (get-external-binding binding-name module)
                        (error "binding ~a not available in module ~a"
                               binding-name module-name)))
           (index (binding-local-index? binding))
           (origin-module-name (binding-module? binding)))
      (if (or (get-binding-info binding 'opencoding)
              (get-binding-info binding 'ff))
          (error "binding ~a not accessable in module ~a"
                 binding-name module-name)
        (dynamic-binding-set1 (as-C-module-name origin-module-name) index x))))

;;;-----------------------------------------------------------------------------
;;; Dynamically load module from a .c-file
;;;-----------------------------------------------------------------------------
   (defmethod load-syntax-module (module-name)
     (dynamic-let ((*indent* (fmt "  ~a" (dynamic *indent*))))
       (notify "Loading syntax module ~a ..." module-name)
       (setq *tmp-source-file-name* module-name)
       (let ((module (dynamic-load-module module-name)))
         (new-syntax-module module-name module))))

   (defun dynamic-load-module (module-name . reload)
    (let ((tmp-silent *silent*))
      (unwind-protect
        (if (and (null? reload) (module-loaded? module-name))
            (let ((module (or (get-module module-name)
                              (load-module-interface module-name))))
              module)
          (let* ((foo (setq *silent* ()))
                 (module (if (null? reload)
                             (or (get-module module-name)
                                 (if (file-exist?
                                      (as-interface-file-name module-name))
                                     (load-module-interface module-name)
                                   (compile-module module-name)))
                           (compile-module module-name)))
                 (foo (setq *silent* tmp-silent))
                 (import (module-used-module-names? module))
                 (syntax (module-used-syntax-modules? module))
                 (file-name (as-C-file-name module-name))
                 (module-name-str (as-C-module-name module-name))
                 (abs-file-name
                  (car (apply file-lookup file-name *load-path*))))
            ;; Make sure all imported modules are loaded;
            ;; When a module need to be
            ;; loaded its interface file need to be loaded as well
            ;; because the literals have to be initalized
            (do1-list (lambda (name)
                        (if (module-loaded? name) ()
                          (dynamic-load-module name)))
                      import)
            (notify "Dynamically linking module ~a ..." module-name)
            (let ((size (dynamic-load-module1 module-name-str abs-file-name)))
              (if (< 0 size)
                  (progn
                    (module-binding-vector-size! module size)
                    ;; Make space for 256 additional bindings
                    ;; See also eul-dld.c
                    (module-max-binding-vector-size! module (+ size 256))
                    (notify0 "initialize literals module ~a ..." module-name)
                    (dynamic-initialize-local-literals module module-name)
                    ;; run init function
                    (notify0 "run module ~a" module-name)
                    ((dynamic-binding-ref1 module-name-str 0))
                    module)
                (error "module ~a can't be loaded correctly" module-name)))))
        ;; Clean-up forms
        (if (eq module-name 'user) ()
          ((setter *get-loaded-module*) module-name ()))
        (setq *silent* tmp-silent))))

  (defun module-loaded? (module-name)
    (dynamic-binding-ref1 (as-C-module-name module-name) 0))

  (defun dynamic-initialize-local-literals (module module-name)
    (let ((module-name-str (as-C-module-name module-name))
          (lliterals (module-local-literals? module)))
      (labels
       ((loop (l)
              (if (null? l) ()
                (let ((entry (car l)))
                  (dynamic-binding-set1 module-name-str
                                        (cdr entry) (car entry))
                  (loop (cdr l))))))
       (loop lliterals))))

;;;-----------------------------------------------------------------------------
;;; Use fast binding lookup with hard-coded standard bindings
;;;-----------------------------------------------------------------------------
  (create-default-modules)

  (defun create-default-modules ()
    (notify0 "Create default modules ...")
    (let* ((info (initialize-default-binding-tables))
           (default-lexical-table (vector-ref info 0))
           (default-syntax-table (vector-ref info 1))
           (default-lexical-module-names (listify-env-string (vector-ref info 2)))
           (default-lexical-module-name
            (make <symbol> name: (vector-ref info 3))) ;; default level1
           (default-syntax-module-name
            (make <symbol> name: (vector-ref info 4)))  ;; default macros
           (default-lexical-module
             (make-module default-lexical-module-name))
           (default-syntax-module (make-module default-syntax-module-name))
           (user-module (make-module 'user))
           (math-module (make-module 'math)) ;; is empty, bindings are in default
           (level0-module (make-module 'level0))
           (default-lexical-env (make-module-env default-lexical-table))
           (default-syntax-env (make-module-env default-syntax-table)))
      (module-binding-vector-size! default-lexical-module ())
      (module-binding-vector-size! default-syntax-module ())
      (module-external-env! default-lexical-module default-lexical-env)
      (module-external-env! default-syntax-module default-syntax-env)
      ;; Set level0 environments to be the same as level1, i.e. the default
      ;; This is to allow modules which import level0 to run in level1
      (module-lexical-env! level0-module default-lexical-env)
      (module-external-env! level0-module default-lexical-env)
      (module-syntax-env! level0-module default-syntax-env)
      ;; Set user environments
      (module-binding-vector-size! user-module 2)
      (module-max-binding-vector-size! user-module 1024)
      (module-lexical-env! user-module default-lexical-env)
      (module-external-env! user-module default-lexical-env)
      (module-syntax-env! user-module default-syntax-env)
      (module-all-used-module-names! default-lexical-module
                                     default-lexical-module-names)))
   (defun make-module-env (tab)
     (let ((fun (lambda (x) (table-ref tab x))))
       ((setter setter) fun
        (lambda (x v)
          (if x
              (progn
                ((setter table-ref) tab x v)
                v)
            tab)))
       fun))

;;;-----------------------------------------------------------------------------
;;; With a little external help
;;;-----------------------------------------------------------------------------
  (defextern initialize-default-binding-tables () ptr
    "eul_initialize_level1_tables")

  (defextern dynamic-binding-ref1 (<string> <int>) ptr
    "eul_dyn_binding_ref")

  ;  (defextern dynamic-level1-binding-ref (ptr ptr) ptr
  ;    "eul_dyn_level1_binding_ref")

  (defextern dynamic-binding-set1 (<string> <int> ptr) ptr
    "eul_dyn_binding_set")

  (defextern dynamic-load-module1 (<string> <string>) <int>
    "eul_dyn_load_module")

  (defextern as-C-module-name (ptr) <string>
    "eul_module_name_as_C_module_name_string")

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
