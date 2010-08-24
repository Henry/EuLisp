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
   export (<integer> integer? even? odd?))

;;;-----------------------------------------------------------------------------
;;; Class <integer>
;;;-----------------------------------------------------------------------------
(defclass <integer> (<number>) ()
  abstract?: t
  predicate: integer?)

;;;-----------------------------------------------------------------------------
;;; Even and odd
;;;-----------------------------------------------------------------------------
(defun even? (x) (zero? (binary-mod x 2)))
;;(declare-inline even?)

(defun odd? (x) (null? (zero? (binary-mod x 2))))
;;(declare-inline odd?)

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
