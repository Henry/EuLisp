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
;;; Title: read-eval-print loop
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule i-rep
  (syntax (_macros)
   import (i-all
           i-args
           sx-obj
           sx-node
           i-compile
           cg-interf
           cg-dld ex-expr
           ex-import
           ex-syntax
           p-env
           sx-obj
           cg-exec
           read)
   export (rep
           eval/cm
           eval
           debug-eval/cm
           prompt-string
           show-module-bindings
           ?
           show-help
           show-class-hierarchy
           macroexpand-1
           macroexpand)
   expose (cg-dld))

;;;-----------------------------------------------------------------------------
;;; To be removed!
;;;-----------------------------------------------------------------------------
(defmethod binary< ((str1 <string>) (str2 <string>))
  (fpi-binary< (string-compare str1 str2) 0))

(defmethod binary< ((sym1 <symbol>) (sym2 <symbol>))
  (fpi-binary< (string-compare (symbol-name sym1)
                               (symbol-name sym2)) 0))

;;;-----------------------------------------------------------------------------
;;; Interpreter initialization
;;;-----------------------------------------------------------------------------
;; Can't use dynamic variables, because application might be multi-threaded
(deflocal *current-module-name* ())
(deflocal *reset-k* ())
(deflocal *resume-k* ())
(deflocal *continue-k* ())
(deflocal *rl* 0)

;; Prompt-string used by i-rep and readline
(deflocal *prompt-string* "prompt> ")
(defun prompt-string ()
  *prompt-string*)

;; Readline history initializer
(defextern initialize-rl () <fpi> "eul_rl_initialize")

(defun initialize-interpreter ()

  (if *script* ()
    (progn
      (format "EuLisp System Youtoo - Version ~a\n" *version*)
      (setq *rl* (initialize-rl))
      (print nl)))

  (setq *current-module-name* 'user)
  (dynamic-setq *actual-module* (get-module *current-module-name*))

  ;; Make all errors continuable
  ;;(setq *error* cerror)
  (setq *error*
        (named-lambda
         cerror (class str . rest)
         (if (and (class? class) (subclass? class <condition>))
             (let/cc k (signal (apply make class message: str rest) k))
           ;; Not EuLisp but very comfortable
           (let/cc
             k (signal
                (make <condition> message: (apply fmt str class rest))
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
;; Previous result of eval/cm
(deflocal ? ())

(defun set-eval-module (module-name)
  (setq *interpreter* t)
  (setq *silent* t)
  (let* ((module (get-module module-name)))
    (if (module? module)
        (dynamic-setq *actual-module* module)
      (progn
        (setq module (dynamic-load-module module-name))
        (unless (module-binding-vector-size? module)
                (module-binding-vector-size! module 2)
                (module-max-binding-vector-size! module 1024))
        ((setter *get-loaded-module*) module-name module)
        (dynamic-setq *actual-module* module)
        (check-module-envs module)))))

(defun eval/cm (x)
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
                  ((eq key 'eval/cm)
                   (eval/cm `(@ eval/cm ,@rest)))
                  ((eq key '@)
                   (let ((op (car rest))
                         (args (cdr rest)))
                     (if (eq op 'dynamic-binding-ref)
                         (apply dynamic-binding-ref args)
                       (if (eq op 'dynamic-binding-ref1)
                           (apply dynamic-binding-ref1 args)
                         (apply (eval/cm op) (map eval/cm args))))))
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
            (eval/cm (load-file-exprs
                   (convert (fmt "~a" file-name) <string>)))))
         ((eq x exit:)
          (exit 0))
         ((eq x trace:)
          (let ((function-name (read lispin () (eos-default-value))))
            (eval/cm `(trace ,function-name))))
         ((eq x untrace:)
          (let ((function-name (read lispin () (eos-default-value))))
            (eval/cm `(untrace ,function-name))))
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
            (fmt "~a" (read lispin () (eos-default-value))) <string>)))
         ((eq x help:)
          (show-help))
         (t x)))
  (thread-reschedule)
  ?)

(defun eval (x . mod)
  (let ((curmod (dynamic *actual-module*))
        (evalmod (and mod (car mod))))
    (set-eval-module evalmod)
    (let ((res (eval/cm x)))
      (dynamic-setq *actual-module* curmod)
      res)))


(defun show-help ()
  (print "load: <file-name>          evaluate file expressions" nl)
  (print "?                          previous value" nl)
  (print "lexical-bindings:          show lexical environment" nl)
  (print "verbose:                   run verbose" nl)
  (print "silent:                    run silent" nl)
  (print "trace: <function-name>     trace function invocation" nl)
  (print "untrace: <functon-name>    stop tracing function invocation" nl)
  (print "backtrace:                 show backtrace" nl)
  (print "values:                    show stack values" nl)
  (print "continue:                  continue computation" nl)
  (print "reset:                     resume from all errors" nl)
  (print "resume:                    resume from previous error" nl)
  (print "[Ctrl-d]                   exit interpreter or resume from previous error" nl)
  (print "[Ctrl-c]                   interrupt computation" nl)
  (print "[Ctrl-z]                   suspend interpreter" nl)
  (print "exit:                      exit interpreter" nl)
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

(defun debug-eval/cm (x)
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
         (eval/cm x))))

;;;-----------------------------------------------------------------------------
;;; Read/eval/print loop
;;;-----------------------------------------------------------------------------
(defun rep ()
  (initialize-interpreter)
  (rep-aux))

(defun rep-aux ()
  (labels
   ((loop (x)
          ;; If readline is active it prints the prompt using `prompt-string'
          (setq *prompt-string* (fmt "~a> " *current-module-name*))
          (unless (or *script* (= *rl* 1))
                  (print *prompt-string*)
                  (flush))
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
                (eval/cm x)
              (format "-> ~s\n" (eval/cm x))))
          (loop ())))
   (let/cc reset-k
     (setq *reset-k* reset-k)
     (setq *resume-k* ())
     (loop ())))
  (rep-aux))

(defun debug-rep ()
  (labels
   ((loop (x)
          ;; If readline is active it prints the prompt using `prompt-string'
          (setq *prompt-string*
                (fmt "[error~a] ~a> "
                     (list-size *resume-k*)
                     *current-module-name*))
          (unless (= *rl* 1)
                  (print *prompt-string*)
                  (flush))
          (reset-interactive-module (dynamic *actual-module*))
          (setq x (read lispin () (eos-default-value)))
          (if (eq x (eos-default-value))
              (progn
                (print nl)
                (debug-eval/cm resume:))
            (format "-> ~s\n" (debug-eval/cm x)))
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
                      (print (get-syntax-binding (car x) m) nl)
                    (print (get-lexical-binding (car x) m) nl))
                  (loop1 (cdr x) m (+ i 1)))
                 (t
                  (print "Continue? (y/n) ")
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
              (format "~a~a<~a>\n"
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
)  ;; End of module i-rep
;;;-----------------------------------------------------------------------------
