;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: level1 (EuLisp Language Level1 Implementation)
;;;  Authors: Andreas Kind, Julian Padget
;;;  Description: condition
;;; -----------------------------------------------------------------------
(defmodule condition
  (syntax (_macros)
   import (telos thread dynamic let-cc)
   export (<condition> conditionp condition-message
           <general-condition> signal error cerror *default-error-handler*
           output-condition-contents push-error-handler pop-error-handlers))
;;; --------------------------------------------------------------------
;;; Class <condition> and <general-condition>
;;; --------------------------------------------------------------------
  (defclass <condition> (<object>)
    ((message keyword: message: accessor: condition-message))
    predicate: conditionp)
  (defclass <general-condition> (<condition>)
    ((arguments keyword: arguments:)))
;;; --------------------------------------------------------------------
;;; Errors and signals
;;; --------------------------------------------------------------------
  ;; error already defined in module boot
  (setq *error*
        (named-lambda error (str class . rest)
          (if (and (classp class) (subclassp class <condition>))
              (signal (apply make class message: str rest) ())
            ;; Not EuLisp but very comfortable
            (signal
             (make <condition> message: (apply format () str class rest))
             ()))))
  (defun cerror (str class . rest)
    (if (and (classp class) (subclassp class <condition>))
        (let/cc k (signal (apply make class message: str rest) k))
      ;; Not EuLisp but very comfortable
      (let/cc k (signal
                 (make <condition> message: (apply format () str class rest))
                 k))))
  (defun signal (cndt k . thrds)
    (let* ((thrd (or (and thrds (car thrds)) (current-thread)))
           (hdls (thread-error-handlers thrd)))
      (labels
       ((loop (ll)
              (if (null ll)
                  (*default-error-handler* cndt k)
                (progn
                  (let/cc handler-error
                          ;; Beware of errors occuring in an error handler
                          (with-handler (lambda (c kk)
                                          (setq cndt c)
                                          (setq k kk)
                                          (handler-error ()))
                            ((car ll) cndt k)))
                  ;; Pass signal to next handler if previous returns
                  (loop (cdr ll))))))
       (loop hdls))))
;;; --------------------------------------------------------------------
;;; Default error handling
;;; --------------------------------------------------------------------
  (deflocal *default-error-handler*
    (named-lambda default-error-handler (cndt k)
      (output-condition-contents cndt)
      (if (null k) ()
        (progn
          (format 2 "***    Do you want to continue? (y/n) ")
          (if (eq (getchar) #\y) (k cndt) ())))
      (format 2 "***    See Backtrace? (y/n) ")
      (if (eq (getchar) #\y) (backtrace) ())
      (exit -1)))
  (defun output-condition-contents (cndt)
    (let* ((cl (class-of cndt))
           (name (class-name cl))
           (str (condition-message cndt)))
      (labels
       ((loop (sds)
              (and sds
                   (let* ((sd (car sds))
                          (name (slot-name sd))
                          (value (slot-value-using-slot sd cndt)))
                     (format 2 "    ~a: ~a\n" name value)
                     (loop (cdr sds))))))
       (format 2 "\n*** ERROR [~a]: ~a\n" name str)
       (loop (cdr (class-slots cl))))))
)  ; end of module
