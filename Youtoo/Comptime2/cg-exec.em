;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: executes compiled code
;;;-----------------------------------------------------------------------------
(defmodule cg-exec
  (syntax (_macros _i-aux0)
          import (cg-exec-word-length i-all i-modify p-env sx-obj sx-node
                  cg-state cg-asm cg-interf i-ffi ex-expr cg-dld)
   export (execute reset-interactive-module))

;;;-----------------------------------------------------------------------------
;;; Globals (not nice but good to speed up the interpreter)
;;;-----------------------------------------------------------------------------
  (deflocal *module* ())
  (deflocal *C-module-name* ())

;;;-----------------------------------------------------------------------------
;;; Execute an interactive module
;;;-----------------------------------------------------------------------------
  (defun execute (module state)
    (setq *pass* 'execute)
    (setq *module* module)
    (setq *C-module-name* (as-C-module-name (module-name? module)))
    (let* ((bv-states (asm-state-bytevectors? state))
           (init-bv-state (asm-state-init-bytevector? state)))
      (set-up-bindings)
      (set-up-bytevectors bv-states)
      ;; Run init function
      (unwind-protect (run-init-bytevector init-bv-state)
        (reset-interactive-module module))))

  (defun reset-interactive-module (module)
    (access-table-clear (module-interactive-lexical-env? module))
    (access-table-clear *get-literal*)
    (module-top-level-forms! module ())
    (module-named-lambdas! module ())
    (module-anonymous-lambdas! module ())
    (module-named-constants! module ())
    (module-static-variables! module ())
    ;; With compile-time handlers during compilation
    (setq *no-ct-handlers* ())
    (setq *pass* 'idle))

;;;-----------------------------------------------------------------------------
;;; Local variables (local to scope of module)
;;;-----------------------------------------------------------------------------
  (defun next-local-index ()
    (let ((index (module-binding-vector-size? *module*)))
      (if index ()
        (error "statically linked module ~a cannot get new bindings"
               (module-name? *module*)))
      (if (< index (module-max-binding-vector-size? *module*)) ()
        (error "module ~a cannot have more than ~a bindings"
               (module-name? *module*) index))
      (module-binding-vector-size! *module* (+ index 1))
      index))

  (defun register-new-local (obj . default-index)
    (let* ((index (or (and default-index (car default-index))
                      (next-local-index))))
      (dynamic-binding-set1 *C-module-name* index obj)
      index))

  ;  (defun get-local-index (binding)
  ;    (or (binding-local-index? binding)
  ;       (let ((index (next-local-index)))
  ;         (binding-local-index! binding index)
  ;         index)))

;;;-----------------------------------------------------------------------------
;;; Set up local bindings
;;;-----------------------------------------------------------------------------
  (defun set-up-bindings ()
    (let* ((env (module-interactive-lexical-env? *module*))
           (bindings
            (select-list true-local-binding? (access-table-values env))))
      (do1-list (lambda (binding)
                  (if (binding-local-index? binding)
                      ()  ; redefined binding
                    (binding-local-index! binding (next-local-index)))
                  (notify0 "  set-up-binding ~a ~a"
                           (binding-local-name? binding)
                           (binding-local-index? binding)))
                bindings)))

;;;-----------------------------------------------------------------------------
;;; Compute and run init bytevector
;;;-----------------------------------------------------------------------------
  (defun run-init-bytevector (state)
    (let* ((n (asm-function-state-pc? state))
           (code (asm-function-state-code? state))
           ;; Add (return 0) = bytecode 172
           (bv (compute-bytevector (append code '(172)) (+ n 1) ()))
           (lambda-name
            (make-symbol
             (string-append
              "initialize-" (symbol-name (module-name? *module*)))))
           (fun (allocate-lambda lambda-name 0 bv)))
      (fun)))

  (defextern allocate-lambda (ptr ptr ptr) ptr "eul_allocate_lambda2")

;;;-----------------------------------------------------------------------------
;;; Set-up bytevectors
;;;-----------------------------------------------------------------------------
  (defun set-up-bytevectors (states)
    (do1-list
     (lambda (state)
       (let ((code (asm-function-state-code? state))
             (handle (asm-function-state-handle? state))
             (binding-name (asm-function-state-binding-name? state))
             (n (asm-function-state-pc? state)))
         (set-bytevector-pos
          handle
          (register-new-local (compute-bytevector code n binding-name)))))
     states))

;;;-----------------------------------------------------------------------------
;;; Local bytevector positions
;;;-----------------------------------------------------------------------------
  (deflocal *local-bytevectors* (make <table>))

  (defun get-bytevector-pos (name)
    (table-ref *local-bytevectors* name))

  (defun set-bytevector-pos (name pos)
    ((setter table-ref) *local-bytevectors* name pos))

;;;-----------------------------------------------------------------------------
;;; Compute bytevector (label size byte ... byte)
;;;-----------------------------------------------------------------------------
  (defun compute-bytevector (code size binding-name)
    (notify0 "  compute-bytevector ~a ~a" binding-name code)
    (open-bytevector size)
    (compute-bytevector-aux code))

  (defun compute-bytevector-aux (code)
    (if (null? code) (bytevector)
      (let ((x (car code)))
        (cond
         ((number? x)
          (write-next-bv-byte x))
         ((consp x)
          (let ((key (car x))
                (args (cdr x))
                (arg1 (car (cdr x))))
            (cond
             ((eq key 'CODE-VECTOR)
              (compute-code-vector arg1))
             ((eq key 'STATIC)
              (compute-static arg1))
             ((eq key 'BINDING)
              (compute-binding (car (cdr args)) arg1))
             ((integer? key)
              (compute-bytevector-aux x))
             ((eq key 'FF)
              (compute-foreign-function-binding arg1))
             ;; must be a branch with key=branch-code and arg1=offset
             (t
              (write-next-bv-byte key)
              (write-next-bv-byte arg1))))))
        (compute-bytevector-aux (cdr code)))))

  (defun compute-code-vector (binding-name)
    (let ((local-index (get-bytevector-pos binding-name)))
      (write-next-bv-binding-ref *C-module-name* local-index)))

  (defun compute-binding (binding-name module-name)
    (with-ct-handler (format () "can't compute binding ~a of module ~a"
                             binding-name (module-name? *module*))
                     binding-name
      (notify0 "  compute-binding ~a ~a" module-name binding-name)
      (let* ((binding (get-lexical-binding binding-name))
             (origin-module-name
              (if (eq module-name '?)
                  (save-binding-module-name? binding)
                module-name))
             (origin-module-name-str (as-C-module-name origin-module-name))
             (local-index (binding-local-index? binding)))
        (write-next-bv-binding-ref origin-module-name-str local-index))))

  (defun get-imported-module-or-library ()
    (let ((module-name (module-name? *module*)))
      (or (get-module module-name)
          (labels
           ((loop (ll)
                  (if (null? ll) ()
                  (let ((lib (get-module (car ll))))
                    (if (member1-list module-name
                                      (module-all-used-module-names? lib))
                        lib
                      (loop (cdr ll)))))))
           (loop *linked-C-libraries*)))))

  (defun compute-static (x)
    (let ((index (register-new-local x)))
      (write-next-bv-binding-ref *C-module-name* index)))

;;;-----------------------------------------------------------------------------
;;; Foreign function call
;;;-----------------------------------------------------------------------------
  (defun compute-foreign-function-binding (binding-name)
    (let* ((binding
            (or (get-lexical-binding binding-name)
                (ct-error
                 -1 "body of inlined function contains non exported binding ~a"
                 binding-name)))
           (module-name (binding-origin-module-name binding))
           (local-index (binding-local-index? binding))
           (module-name-str (as-C-module-name module-name)))
      (if (intp local-index)
          (write-next-bv-binding-ref module-name-str local-index)
        (ct-error -1 "bad index ~a of foreign function ~a"
                  local-index binding-name))))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
