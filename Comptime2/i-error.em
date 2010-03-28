;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;;  Description: error handling
;;; -----------------------------------------------------------------------
(defmodule i-error
  (syntax (_macros _i-aux0)
   import (i-level1 i-param i-notify)
   export (<ct-error> ct-error ct-exit))

;;; --------------------------------------------------------------------
;;; Compile-time error conditions
;;; --------------------------------------------------------------------
  (defclass <ct-error> (<condition>)
    ((value keyword: ct-error-value: accessor: ct-error-value)))

  (defmethod get-ct-error-condition-class (x) <ct-error>)

  (defmethod generic-prin ((c <ct-error>) (s <stream>))
    (ct-serious-warning (ct-error-value c) (condition-message c)))

  (defun ct-error (value str . args)
    (error (apply format () str args) <ct-error> ct-error-value: value))

;;; --------------------------------------------------------------------
;;; Exit from compilation
;;; --------------------------------------------------------------------
  (defun ct-exit values
    (let ((value (if values (car values) ())))
      (if (= *number-of-warnings* 0) ()
          (format stderr "*** TOTAL NUMBER OF WARNINGS: ~a\n"
                  *number-of-warnings*))
      (if (= *number-of-errors* 0) ()
        (progn
          (format stderr "*** TOTAL NUMBER OF ERRORS: ~a\n"
                  *number-of-errors*)
          (if values ()
            (setq values -1))))
      (exit value)))

)  ; end of module
