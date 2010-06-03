;;;-----------------------------------------------------------------------------
;;;                 OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;; File   : basic.em
;;; Date   : 11 Jul 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Description: Basic functions etc.
;;;-----------------------------------------------------------------------------
(defmodule basic
  (syntax (macros)
   import (level1)
   export (cddddr cadadr assoc NIL))

  (print "### basic")

  (defconstant NIL ())

;;;-----------------------------------------------------------------------------
;;; List operations
;;;-----------------------------------------------------------------------------
  (defun cddddr (x) (cdr (cdr (cdr (cdr x)))))
  (declare-inline cddddr)

  (defun cadadr (x) (car (cdr (car (cdr x)))))
  (declare-inline cadadr)

;;;-----------------------------------------------------------------------------
;;; assoc
;;;-----------------------------------------------------------------------------

  (defun assoc (obj list . comp)
    (assoc-loop obj list (if (null comp) eql (car comp))))

  (defun assoc-loop (obj list comp)
    (cond ((atom list) ())
          ((comp obj (caar list)) (car list))
          (t (assoc-loop obj (cdr list) comp))))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
