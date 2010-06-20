;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind
;;; Description: read-eval-print loop
;;;-----------------------------------------------------------------------------
(defmodule i-rep
  (syntax (_macros)
   import (i-all i-args sx-obj sx-node i-compile cg-interf cg-dld ex-expr
           ex-import ex-syntax p-env sx-obj cg-exec read)
   export (rep eval debug-eval
           show-module-bindings ? show-help show-class-hierarchy)
   expose (cg-dld))

;;;-----------------------------------------------------------------------------
;;; To be removed!
;;;-----------------------------------------------------------------------------
  (defmethod binary< ((str1 <string>) (str2 <string>))
    (int-binary< (string-compare str1 str2) 0))

  (defmethod binary< ((sym1 <symbol>) (sym2 <symbol>))
    (int-binary< (string-compare (symbol-name sym1)
                                 (symbol-name sym2)) 0))

;;;-----------------------------------------------------------------------------
;;; Interpreter initialization
;;;-----------------------------------------------------------------------------
  ;; Can't use dynamic variables, because application might be multi-threaded
  (deflocal *current-module-name* ())
  (deflocal *reset-k* ())
  (deflocal *resume-k* ())
  (deflocal *continue-k* ())

  ;; Readline history initializer
  (defextern initialize-rl () <int> "eul_rl_initialize")

  (defun initialize-interpreter ()

    (if *script* ()
      (format t "EuLisp System 'youtoo ~a'\n" *version*))

    (initialize-rl)

    (setq *current-module-name* 'user)
    (dynamic-setq *actual-module* (get-module *current-module-name*))

    ;; Make all errors continuable
    ;;(setq *error* cerror)
    (setq *error*
          (named-lambda
           cerror (str class . rest)
           (if (and (class? class) (subclass? class <condition>))
               (let/cc k (signal (apply make class message: str rest) k))
             ;; Not EuLisp but very comfortable
             (let/cc
              k (signal
                 (make <condition> message: (apply format () str class rest))
                 k)))))

    ;; Reset the default error handler to catch errors on all threads
    (setq *default-error-handler*
          (named-lambda default-error-handler (cndt continue-k)
            (output-condition-contents cndt)
            (if (eq *pass* 'execute)
                (progn
                  (setq *continue-k* (cons continue-k *continue-k*))
                  (debug-rep))
              (if *resume-k*
                  (let ((resume-k (car *resume-k*)))
                    (setq *resume-k* (cons 'dummy *resume-k*))
                    (resume-k ()))
                (*reset-k* ())))))

    ;; Enable signals
    (eul-signal-enable t)

    ;; If interpreting a script open the script file
    ;; and set lispin to read and parse that stream
    (if *script*
        (setq lispin
              (make <stream>
                    source: (make <file-stream>
                                  file-name: *script-file*
                                  mode: 'r)
                    read-action: (lambda (s eos-error? eos-value)
                                   (parse (stream-source s)
                                          eos-error?
                                          eos-value))))
      ())
    )

  (defextern eul-signal-enable (boolean) boolean "eul_signal_enable")

;;;-----------------------------------------------------------------------------
;;; Evaluation of sexprs
;;;-----------------------------------------------------------------------------
  ;; Previous result of eval
  (deflocal ? ())

  (defun eval (x)
    (setq
     ?
     (cond ((symbol? x)
            (cond ((eq x 't) t)
                  ((eq x '?) ?)
                  (t (dynamic-binding-ref x *current-module-name*))))
           ((cons? x)
            (let ((key (car x))
                  (rest (cdr x)))
              (cond ((eq key 'quote)
                     (car rest))
                    ((eq key 'eval)
                     (eval `(@ eval ,@rest)))
                    ((eq key '@)
                     (let ((op (car rest))
                           (args (cdr rest)))
                       (if (eq op 'dynamic-binding-ref)
                           (apply dynamic-binding-ref args)
                         (if (eq op 'dynamic-binding-ref1)
                             (apply dynamic-binding-ref1 args)
                           (apply (eval op) (map eval args))))))
                    (t
                     (interactive-compile x)))))
           ((eq x :)
            (let* ((new-module-name (read lispin () (eos-default-value)))
                   (module (get-module new-module-name)))
              (if (module? module)
                  (dynamic-setq *actual-module* module)
                (progn
                  (setq module (dynamic-load-module new-module-name))
                  ((setter *get-loaded-module*) new-module-name module)
                  (dynamic-setq *actual-module* module)
                  (check-module-envs module)))
              (setq *current-module-name* new-module-name)))
           ((eq x ::)
            (let* ((new-module-name (read lispin () (eos-default-value)))
                   (module (get-module new-module-name)))
              (if (eq new-module-name 'user) ()
                ;; user module is hard-coded and cannot be loaded
                (progn
                  (if (module? module)
                      ((setter *get-loaded-module*) new-module-name ())
                    ())
                  (setq module (dynamic-load-module new-module-name t))
                  ((setter *get-loaded-module*) new-module-name module)))
              (dynamic-setq *actual-module* module)
              (setq *current-module-name* new-module-name)))
           ((eq x load:)
            (let ((file-name (read lispin () (eos-default-value))))
              (eval (load-file-exprs
                     (convert (format () "~a" file-name) <string>)))))
           ((eq x exit:)
            (exit 0))
           ((eq x trace:)
            (let ((function-name (read lispin () (eos-default-value))))
              (eval `(trace ,function-name))))
           ((eq x untrace:)
            (let ((function-name (read lispin () (eos-default-value))))
              (eval `(untrace ,function-name))))
           ((eq x backtrace:)
            (backtrace))
           ((eq x values:)
            (stack-values))
           ((eq x defined-lexical-bindings:)
            (show-module-bindings () ()))
           ((eq x defined-syntax-bindings:)
            (show-module-bindings t ()))
           ((eq x lexical-bindings:)
            (show-module-bindings () t))
           ((eq x syntax-bindings:)
            (show-module-bindings t t))
           ((eq x lexical-import:)
            (show-imported-modules ()))
           ((eq x syntax-import:)
            (show-imported-modules t))
           ((eq x import:)
            (let ((module-name (read lispin () (eos-default-value))))
              (import-module module-name)))
           ((eq x verbose:)
            (setq *silent* ())
            (setq *verbose* t))
           ((eq x silent:)
            (setq *silent* t)
            (setq *verbose* ()))
           ((eq x hierarchy:)
            (if (class? ?)
                (show-class-hierarchy ?)
              (show-class-hierarchy)))
           ((eq x redefine:)
            (setq *redefine-imported-bindings*
                  (null? *redefine-imported-bindings*)))
           ((eq x -:)
            (system
             (convert
              (format () "~a" (read lispin () (eos-default-value))) <string>)))
           ((eq x help:)
            (show-help))
           (t x)))
    (thread-reschedule)
    ?)

  (defun show-help ()
    (print "load: <file-name>          evaluate file expressions")
    (print "?                          previous value")
    (print "lexical-bindings:          show lexical environment")
    (print "verbose:                   run verbose")
    (print "silent:                    run silent")
    (print "trace: <function-name>     trace function invocation")
    (print "untrace: <functon-name>    stop tracing function invocation")
    (print "backtrace:                 show backtrace")
    (print "values:                    show stack values")
    (print "continue:                  continue computation")
    (print "reset:                     resume from all errors")
    (print "resume:                    resume from previous error")
    (print "[Ctrl-d]                   exit interpreter or resume from previous error")
    (print "[Ctrl-c]                   interrupt computation")
    (print "[Ctrl-z]                   suspend interpreter")
    (print "exit:                      exit interpreter")
    )

  (defun check-module-envs (module)
    ;; Update lexical and syntax environments if necessary;
    ;; Lexical and syntax envs need not to be updated if module was compiled
    (if (or (null? module)
            (access-table-values (module-lexical-env? module))) ()
        (let ((import (module-used-module-names? module))
              (syntax (module-used-syntax-modules? module)))
          (module-lexical-env! module (module-external-env? module))
          (do1-list import-module import)
          (do1-list import-syntax-module syntax))))

  (defun debug-eval (x)
    (cond ((eq x reset:)
           (*reset-k* ()))
          ((eq x resume:)
           (if (and *resume-k* (cdr *resume-k*))
               ((car (cdr *resume-k*)) ())
             (*reset-k* ())))
          ((eq x continue:)
           (let ((continue-k (car *continue-k*)))
             (setq *continue-k* (cdr *continue-k*))
             (setq *resume-k* (cdr *resume-k*))
             (continue-k ())))
          (t
           (eval x))))

;;;-----------------------------------------------------------------------------
;;; Read/eval/print loop
;;;-----------------------------------------------------------------------------
  (defun rep ()
    (initialize-interpreter)
    (rep-aux))

  (defun rep-aux ()
    (labels
     ((loop (x)
            (format t "~a> " *current-module-name*)
            (flush)
            (reset-interactive-module (dynamic *actual-module*))
            (setq *number-of-warnings* 0)
            (setq *number-of-errors* 0)
            (setq *stop-after-pass* ())
            (setq x (read lispin () (eos-default-value)))
            (if (eq x (eos-default-value))
                (progn
                  (notify "")  ; newline
                  (exit 0))
              ;; Ok, we're really going to eval something now!
              (if *script*
                  (eval x)
                (format t "-> ~s\n" (eval x))))
            (loop ())))
     (let/cc reset-k
             (setq *reset-k* reset-k)
             (setq *resume-k* ())
             (loop ())))
    (rep-aux))

  (defun debug-rep ()
    (labels
     ((loop (x)
            (format t "[error~a]~a> "
                    (list-size *resume-k*)
                    *current-module-name*)
            (flush)
            (reset-interactive-module (dynamic *actual-module*))
            (setq x (read lispin () (eos-default-value)))
            (if (eq x (eos-default-value))
                (progn
                  (newline)
                  (debug-eval resume:))
              (format t "-> ~s\n" (debug-eval x)))
            (loop ())))
     (let/cc resume-k
             (setq *resume-k* (cons resume-k *resume-k*))
             (loop ()))
     (setq *resume-k* (cdr *resume-k*))
     (setq *continue-k* (cdr *continue-k*))
     (loop ())))

;;;-----------------------------------------------------------------------------
;;; Auxiliary functions
;;;-----------------------------------------------------------------------------
  (defun show-module-bindings (syntax all . module-names)
    (labels
     ((loop1 (x m i)
            (cond ((null? x)
                   ())
                  ((< i 256)
                   (if syntax
                       (print (get-syntax-binding (car x) m))
                     (print (get-lexical-binding (car x) m)))
                   (loop1 (cdr x) m (+ i 1)))
                  (t
                   (prin "Continue? (y/n) ")
                   (flush)
                   (if (eq 'y (read))
                       (loop1 x m 0)
                     ()))))
      (loop2 (x m res)
             (if (null? x)
                 res
               (let* ((binding (car x))
                      (binding-name (save-binding-local-name? binding))
                      (proper-binding
                       (if syntax
                           (get-syntax-binding binding-name m)
                         (get-lexical-binding binding-name m))))
                 (if (or all (null? (binding-imported? proper-binding)))
                     (loop2 (cdr x) m (cons binding-name res))
                   (loop2 (cdr x) m res))))))
     (let* ((module
             (or (and module-names
                      (get-module (car module-names)))
                 (dynamic *actual-module*)))
             (bindings
              (if syntax
                  (access-table-values (module-syntax-env? module))
                (access-table-values (module-lexical-env? module))))
             (selected-binding-names (loop2 bindings module ()))
             (sorted-binding-names (sort selected-binding-names binary<)))
       (loop1 sorted-binding-names module 0))))

  (defun show-imported-modules (syntax)
    (let ((module (dynamic *actual-module*)))
      (do print
        (sort
         (if syntax
             (module-used-syntax-modules? module)
           (map get-module (module-used-module-names? module)))))))

  (defun show-class-hierarchy classes
    (let ((root-class (if classes (car classes) <object>)))
      (labels
        ((loop (cl indent)
          (let ((subclasses (class-direct-subclasses cl))
                (new-indent (concatenate indent "  ")))
            (format t "~a~a<~a>\n"
                    (if (class-abstract? cl) "A" " ")
                    new-indent
                    (class-name cl))
            (do (lambda (x) (loop x new-indent)) subclasses))))
        (loop root-class ""))))

  (defun load-file-exprs (name)
    (with-input-file-of-path (s name dir *load-path*)
      (labels
       ((loop (exprs)
              (let ((expr (read-s-expression s () (eos-default-value))))
                (if (eq expr (eos-default-value))
                    (cons 'progn (reverse exprs))
                  (loop (cons expr exprs))))))
       (loop ()))))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
