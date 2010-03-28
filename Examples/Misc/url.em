;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: misc
;;;  Authors: Andreas Kind
;;;  Description: accessing a URL
;;;  Compilation: youtoo url -l level1
;;;  Run: url www.cs.bath.ac.uk /~jap/ak1/youtoo/home.html
;;; -----------------------------------------------------------------------
(defmodule url
  (syntax (macros)
   import (level1))
  (let* ((host (vector-ref *argv* 1))
         (file-name (vector-ref *argv* 2))
         (c (make <connection> host: host port: 80))
         x)
    (format c "GET ~a\n" file-name)
    (while (null (eq (setq x (read-line c () (eos-default-value)))
                     (eos-default-value)))
      (prin x))
    (disconnect c))
)  ; end of module
