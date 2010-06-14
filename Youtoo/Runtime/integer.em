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
   export (<integer> integer? evenp oddp))

;;;-----------------------------------------------------------------------------
;;; Class <integer>
;;;-----------------------------------------------------------------------------
  (defclass <integer> (<number>) ()
    abstractp: t
    predicate: integer?)

;;;-----------------------------------------------------------------------------
;;; Even and odd
;;;-----------------------------------------------------------------------------
  (defun evenp (x) (zero? (binary-mod x 2)))
  ;;(declare-inline evenp)

  (defun oddp (x) (null? (zero? (binary-mod x 2))))
  ;;(declare-inline oddp)

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
