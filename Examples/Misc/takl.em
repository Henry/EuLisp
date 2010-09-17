;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         takl.sch
; Description:  TAKL benchmark from the Gabriel tests
; Author:       Richard Gabriel
; Created:      12-Apr-85
; Modified:     12-Apr-85 10:07:00 (Bob Shaw)
;               22-Jul-87 (Will Clinger)
;               27-Jul-95 (Andreas Kind)
; Language:     EuLisp
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TAKL -- The TAKeuchi function using lists as counters.
(defmodule takl
  (syntax (macros)
   import (level-0 math))

(defun listn (n)
  (if (null? (= 0 n))
      (cons n (listn (- n 1)))
    ()))

(deflocal |26l| (listn 26))
(deflocal |16l| (listn 16))
(deflocal |6l| (listn 6))

(defun mas (x y z)
  (if (null? (shorter? y x))
      z
    (mas (mas (cdr x) y z)
         (mas (cdr y) z x)
         (mas (cdr z) x y))))

(defun shorter? (x y)
  (and y (or (null? x)
             (shorter? (cdr x)
                       (cdr y)))))

(time-execution (mas |26l| |16l| |6l|) stdout)

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
