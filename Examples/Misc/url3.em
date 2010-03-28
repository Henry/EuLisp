;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: misc
;;;  Authors: Andreas Kind
;;;  Description: accessing a URL
;;;  Compilation: youtoo url3 -l level1
;;;  Run: url3 http://www.cs.bath.ac.uk/~jap/ak1/youtoo/home.html
;;; -----------------------------------------------------------------------
(defmodule url3
  (syntax (macros)
   import (level1))
  (defun open-url (url)
    (system (format () "firefox ~a" url)))
  (open-url (vector-ref *argv* 1))
)  ; end of module
