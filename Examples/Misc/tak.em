;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         tak.em
; Description:  TAK benchmark from the Gabriel tests
; Author:       Richard Gabriel
; Created:      12-Apr-85
; Modified:     12-Apr-85 09:58:18 (Bob Shaw)
;               22-Jul-87 (Will Clinger)
;               27-Jul-95 (Andreas Kind)
; Language:     EuLisp
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TAK -- A vanilla version of the TAKeuchi function
(defmodule tak
  (syntax (macros)
   import (level1))

  (defun tak (x y z)
    (if (null? (< y x))
        z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

  (print (tak 18 12 6))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
