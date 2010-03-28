;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;                 OPS5 for EuLisp System 'youtoo'
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; File   : macros-tag.em
;;; Date   : 11 Jul 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Desc.  : macros
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defmodule macros-tag
  ()
; (syntax (macros)
;        import (level1))
  
;  (defmacro while (condition . body)
;    `(let/cc break                      ; (syntax break)
;       (labels ((| do it again | ()
;                 (when ,condition
;                      ,@body
;                      (| do it again |))))
;               (| do it again |))))
;  (defmacro for (init condition inc . body)
;    `(progn
;       ,init
;       (while ,condition
;         ,@body
;         ,inc)))
) ;; module: macros-tag
