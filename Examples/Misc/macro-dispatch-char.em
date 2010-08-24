;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp system 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: misc
;;;  Authors: Andreas Kind
;;; Description: Common Lisp like dispatch macro characters
;;;  Compilation
;;    Needs file macro-dispatch-char.dat
;;    with (1 2 3 #M(4 5 6) 7 8 9)
;;;-----------------------------------------------------------------------------
(defmodule macro-dispatch-char
  (syntax (macros)
   import (level1)
   export ())

(set-dispatch-macro-character #\# #\M
                              (lambda (s key dummy)
                                (reverse (read-s-expression s))))

(let ((file (make <file-stream> file-name: "macro-dispatch-char.dat")))
  (print (read-s-expression file))
  (disconnect file))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
