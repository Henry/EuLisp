;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind
;;; Description: root module of the compiler
;;;-----------------------------------------------------------------------------
(defmodule eval
  (syntax (_macros _i-aux0)
   import (i-all i-args i-compile i-modify cg-interf i-rep)
   export (main eval rep ?
                dynamic-binding-ref dynamic-binding-set
                dynamic-load-module as-dynamic-binding
                module-loaded? as-C-module-name
                dynamic-binding-ref1 dynamic-binding-set1 dynamic-load-module1
                *redefine-imported-bindings*))

;;;-----------------------------------------------------------------------------
;;; Main function to invoke the interpreter or compiler
;;;-----------------------------------------------------------------------------
(defun main (argv)
  ;; Error handler
  (with-handler (generic-lambda (c k)
                                method: ((c k)) ; pass signal to next handler
                                method: (((c <ct-error>) k)
                                         (print c stderr)
                                         (ct-exit -1)))
                (ct-reset)
                (parse-args argv)
                (check-stop)
                (if *script* ()
                  ;; Don't want interleaving messages
                  (setq stderr stdout))
                (start-eulysses))
  (sflush stdout)
  (sflush stderr))

;;;-----------------------------------------------------------------------------
;;; Check if compiler or interpreter should be invoked
;;;-----------------------------------------------------------------------------
(defun start-eulysses ()
  (notify "")  ; newline
  (notify0 "Start EuLysses ...")
  (if *interpreter*
      ;; Invoke interpreter
      (progn
        (rep)
        (exit 0))
    ;; Invoke compiler
    (progn
      (load-library-interfaces)
      (do1-list
       (lambda (module-name)
         (setq *tmp-start-source-file-name* module-name)
         ;; Check if recompilation is necessary
         (if (directly-or-indirectly-modified? module-name)
             (compile module-name)
           (notify "Module ~a need not be recompiled." module-name))
         (link module-name))
       *source-file-names*)
      (ct-exit))))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
