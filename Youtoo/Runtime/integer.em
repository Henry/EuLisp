;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;; Description: integers
;;;-----------------------------------------------------------------------------
(defmodule integer
  (syntax (_telos0)
   import (telos number)
   export (<integer> integerp evenp oddp))

;;;-----------------------------------------------------------------------------
;;; Class <integer>
;;;-----------------------------------------------------------------------------
  (defclass <integer> (<number>) ()
    abstractp: t
    predicate: integerp)

;;;-----------------------------------------------------------------------------
;;; Even and odd
;;;-----------------------------------------------------------------------------
  (defun evenp (x) (zerop (binary-mod x 2)))
  ;;(declare-inline evenp)

  (defun oddp (x) (null? (zerop (binary-mod x 2))))
  ;;(declare-inline oddp)

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
