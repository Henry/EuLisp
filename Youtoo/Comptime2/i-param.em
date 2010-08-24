;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: compiler parameters
;;;-----------------------------------------------------------------------------
(defmodule i-param
  (syntax (_macros _i-aux0)
   import (i-level1)
   export (*version*
           *ostype*
           *interpreter*
           *C-cc*
           *C-ld*
           *C-cc-flags*
           *C-cc-libs*
           *strip-stand-alone*
           *C-ar*
           *C-ranlib*
           *create-C-module*
           *create-C-library*
           *stand-alone*
           *main-link-string*
           *script*
           *silent*
           *verbose*
           *warnings*
           *errors*
           *no-ct-handlers*
           *debug*
           *inline-level*
           *peephole*
           *pass*
           *stop-after-pass* stop-after-pass
           *number-of-warnings*
           *number-of-errors*
           *get-loaded-module*
           *get-loaded-syntax-module*
           *modified-module-names*
           *get-load-dir*
           *get-full-import*
           *load-path*
           *C-library-load-path*
           *linked-C-libraries*
           *linked-C-ff-files*
           *linked-C-ff-libraries*
           *delimiter*
           *source-file-names*
           *tmp-source-file-name*
           *tmp-start-source-file-name*
           *dest-file-name*
           *object-dir*
           *script-file*
           *eulysses-dir*
           *eulysses-arch*
           *recompile*
           *no-recompile*
           *no-gc*
           *no-else*
           *redefine-imported-bindings*
           *tmp-load-dir*
           *get-literal*
           ct-reset
           get-ct-error-condition-class
           parse-module compile-module load-syntax-module
           get-named-encl-lambda))

;;;-----------------------------------------------------------------------------
;;;  Global compiler parameters with default settings
;;;-----------------------------------------------------------------------------
(defconstant *delimiter* "/")
(deflocal *load-path* (listify-env-string (getenv "EUL_LOAD_PATH")))
(deflocal *eulysses-dir* (getenv "EUL_DIR"))
(deflocal *eulysses-arch* (getenv "EUL_ARCH"))

(deflocal *C-library-load-path*
  (listify-env-string (getenv "EUL_LIBRARY_LOAD_PATH")))

(deflocal *tmp-load-dir* ())

(defun load-config-info ()
  (let* ((private-name (fmt "~a~a.eulrc.~a" (getenv "HOME") *delimiter*  *eulysses-arch*))
         (default-name (fmt "~a~a.eulrc.~a" *eulysses-dir* *delimiter* *eulysses-arch*))
         (default-info (with-input-file
                        (s default-name) (read-s-expression s))))
    (if (zero? (system (fmt "test -f ~a" private-name)))
        (let* ((private-info (with-input-file
                              (s private-name) (read-s-expression s)))
               (private-version (assoc-list-ref 'VERSION private-info))
               (default-version (assoc-list-ref 'VERSION default-info)))
          (if (binary= private-version default-version)
              private-info
            ;; Here we could add some code to update the private resource file
            ;; because the version apparently has changed
            private-info))
      default-info)))

(defun get-config-info (name)
  (let ((entry (assoc-list-ref *config-info* name)))
    (if entry (cdr entry) "")))

(defun get-cc-flags ()
  (get-config-info 'CFLAGS))

(defun get-cc-libs ()
  (get-config-info 'CLIBS))

(deflocal *config-info* (load-config-info))
(deflocal *version* (get-config-info 'VERSION))
(deflocal *ostype* (get-config-info 'OSTYPE))
(deflocal *interpreter* ())       ; start interpreter
(deflocal *C-cc* (get-config-info 'CC)) ; used C compiler
(deflocal *C-ld* *C-cc*)          ; used C linker
(deflocal *C-cc-flags* (get-cc-flags))  ; C compiler flags
(deflocal *C-cc-libs* (get-cc-libs))  ; additional C compiler libraries
(deflocal *strip-stand-alone* ())  ; strip stand-alone application?
(deflocal *C-ar* (get-config-info 'AR)) ; used C archive command
(deflocal *C-ranlib* (get-config-info 'RANLIB))  ; used C ranlib command
(deflocal *create-C-module* ())   ; create C-linkable module
(deflocal *create-C-library* ())  ; create C-linkable library
(deflocal *linked-C-libraries* ()); linked C-linkable libraries
(deflocal *linked-C-ff-files* ()) ; linked C foreign function files
(deflocal *linked-C-ff-libraries* ()) ; linked C foreign function libraries
(deflocal *stand-alone* t)        ; create stand-alone application
(deflocal *main-link-string* (main-link-string))
(deflocal *script* ())            ; interpret a script from stdin
(deflocal *silent* ())            ; compile silently
(deflocal *verbose* ())           ; compile loudly
(deflocal *warnings* t)           ; notify warnings
(deflocal *errors* t)             ; notify errors
(deflocal *number-of-warnings* 0) ; number of occured warnings
(deflocal *number-of-errors* 0)   ; number of occured errors
(deflocal *no-ct-handlers* ())    ; switch off compile time error handlers
(deflocal *debug* ())             ; produce debugging information
(deflocal *inline-level* 1)       ; 0: ignore inline declarations
(deflocal *peephole* t)           ; peephole optimization
(deflocal *recompile* ())         ; recompile modules
(deflocal *no-recompile* ())      ; don't recompile imports
(deflocal *no-gc* ())             ; don't link gc library
(deflocal *no-else* ())           ; don't expect else branch
(deflocal *redefine-imported-bindings* ()) ; don't redefine imported bindings
(deflocal *source-file-names* ())
(deflocal *tmp-source-file-name* ())
(deflocal *tmp-start-source-file-name* ())
(deflocal *dest-file-name* ())
(deflocal *object-dir* ())
(deflocal *script-file* ())
(deflocal *get-loaded-module* (make-access-table))
(deflocal *get-loaded-syntax-module* (make-access-table))
(deflocal *get-load-dir* (make-access-table comparator: binary=))
(deflocal *get-full-import* (make-access-table))
(deflocal *modified-module-names* ())
(deflocal *pass* 'start)
(deflocal *stop-after-pass* ())
(deflocal *get-literal* (make-access-table comparator: binary=))

(defglobal *actual-module* ())         ; module being in compilation
(defglobal *encl-lambda* ())           ; used during expansion phase
(defglobal *pprint* ())                ; pretty printing
(defglobal *indent* "")                ; message indentation
(defglobal *trace-indent* "")          ; trace indentation

;;;-----------------------------------------------------------------------------
;;; Reset compile state
;;;-----------------------------------------------------------------------------
(defun ct-reset ()
  (setq *interpreter* ())
  (setq *C-cc* (get-config-info 'CC))
  (setq *C-ld* *C-cc*)
  (setq *strip-stand-alone* ())
  (setq *C-cc-flags* (get-cc-flags))
  (setq *C-cc-libs* (get-cc-libs))
  (setq *C-ar* (get-config-info 'AR))
  (setq *C-ranlib* (get-config-info 'RANLIB))
  (setq *create-C-module* ())
  (setq *create-C-library* ())
  (setq *load-path*
        (listify-env-string (getenv "EUL_LOAD_PATH")))
  (setq *C-library-load-path*
        (listify-env-string (getenv "EUL_LIBRARY_LOAD_PATH")))
  (setq *linked-C-libraries* ())
  (setq *linked-C-ff-files* ())
  (setq *linked-C-ff-libraries* ())
  (setq *stand-alone* t)
  (setq *script* ())
  (setq *silent* ())
  (setq *verbose* ())
  (setq *warnings* t)
  (setq *errors* t)
  (setq *number-of-warnings* 0)
  (setq *number-of-errors* 0)
  (setq *source-file-names* ())
  (setq *dest-file-name* ())
  (setq *object-dir* ())
  ;  (access-table-clear *get-loaded-module*)
  ;  (access-table-clear *get-loaded-syntax-module*)
  (setq *modified-module-names* ())
  (setq *pass* 'start)
  (setq *stop-after-pass* ())
  (setq *debug* ())
  (setq *inline-level* 1)
  (setq *peephole* t)
  (setq *recompile* ())
  (setq *no-recompile* ())
  (setq *no-gc* ())
  (setq *tmp-load-dir* ())
  (setq *tmp-source-file-name* ())
  (setq *tmp-start-source-file-name* ())
  (access-table-clear *get-literal*)
  (access-table-clear *get-load-dir*)
  (access-table-clear *get-full-import*))

;;;-----------------------------------------------------------------------------
;;; Stop the compilation after current or a particular pass
;;;-----------------------------------------------------------------------------
(defun stop-after-pass pass-name
  (let ((pass (or (and pass-name (car pass-name)) *pass*)))
    (setq *stop-after-pass* pass)))

;;;-----------------------------------------------------------------------------
;;; To avoid circular module imports the following fuctions, which are
;;; used in more than one module, are defined as generic functions.
;;;-----------------------------------------------------------------------------
(defgeneric compile-module (name))
(defgeneric load-syntax-module (name))
(defgeneric parse-module (sexprs))
(defgeneric get-ct-error-condition-class (x))
(defgeneric get-named-encl-lambda (x))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
