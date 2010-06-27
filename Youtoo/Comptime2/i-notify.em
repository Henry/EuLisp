;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: compile-time notifications and warnings
;;;-----------------------------------------------------------------------------
(defmodule i-notify
   (syntax (_macros)
    import (i-level1 i-param)
    export (notify0 notify ct-warning ct-serious-warning))

;;;-----------------------------------------------------------------------------
;;;  Notification ...
;;;-----------------------------------------------------------------------------
   (defun basic-notify (s var str . args)
     (if (null? var) ()
       (progn
         (apply format s str args)
         (snewline s)
         (sflush s))))

   (defun notify0 (str . args)
     (apply basic-notify stderr *verbose* str args))

   (defun notify (str . args)
     (apply basic-notify stderr (null? *silent*)
            (string-append (dynamic *indent*) str) args))

   (defun basic-warning (var prompt str . args)
     (dynamic-let ((*pprint* 1))
       (let* ((m (dynamic *actual-module*))
              (pre-str (if m
                           (format () "[~a]" (primitive-ref m 0))
                         ""))
              (fun (get-named-encl-lambda (dynamic *encl-lambda*))))
         (if (null? fun) ()
             (setq pre-str (format () "~a[~a]" pre-str fun)))
         (apply basic-notify stderr var
                (format () "*** ~a ~a: ~a" prompt pre-str str) args))))

;;;-----------------------------------------------------------------------------
;;;  Warning (compilation can be continued)
;;;-----------------------------------------------------------------------------
   (defun ct-warning (value str . args)
     (setq *number-of-warnings* (+ *number-of-warnings* 1))
     (apply basic-warning *warnings* "WARNING" str args)
     value)

;;;-----------------------------------------------------------------------------
;;;  Serious warning; compilation can only be continued up to next
;;  check-stop; see ct-error in i-errror for immediate compilation exit
;;;-----------------------------------------------------------------------------
   (defun ct-serious-warning (value str . args)
     (setq *number-of-errors* (+ *number-of-errors* 1))
     (stop-after-pass)
     (if (null? *interpreter*)
         (progn
           (apply basic-warning *errors* "ERROR" str args)
           value)
       ;; Force error to be catched by interpreter loop
       (error (apply format () str args) (get-ct-error-condition-class ())
              ct-error-value: (dynamic *actual-module*))))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
