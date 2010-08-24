;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: misc
;;;  Authors: Andreas Kind
;;; Description: accessing a URL
;;;  Compilation
;;    youtoo url2 -l level1
;;;  Run
;;    url2
;;;-----------------------------------------------------------------------------
(defmodule url2
  (syntax (macros)
   import (level1))

(let ((c (make <connection> host: "www.cs.bath.ac.uk" port: 80))
      x)
  (sprint c "GET /~jap/ak1/youtoo/home.html")
  (while (setq x (read-line c () ()))
    (prin x))
  (disconnect c))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
