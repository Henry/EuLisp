;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;; ---                  EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: misc
;;;  Authors: Andreas Kind
;;;  Description: foreign function test
;;;  Compilation: youtoo ffi1 -l level1
;;; -----------------------------------------------------------------------
(defmodule ffi1
  (syntax (macros)
   import (level1))
;;; --------------------------------------------------------------------
;;; Make use of C's atoi function
;;; --------------------------------------------------------------------
  (defextern lisp-atoi (<string>) <int> "atoi")
  (if (< 1 *argc*)
      (progn
        (print (+ (lisp-atoi (vector-ref *argv* 1))
                       (lisp-atoi "123")))
        0) ;; Return a no error
    (error "no parameter passed" <condition>))
)
