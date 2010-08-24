;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;; Description: EuLisp's primitive-* functions
;;;-----------------------------------------------------------------------------
(defmodule mop-prim
  (import (boot)
   export (primitive-allocate primitive-class-of primitive-ref))

;;;-----------------------------------------------------------------------------
(defun primitive-allocate (class size)
  ((opencoded-lambda (c n) (primitive-allocate)) class size))
(declare-inline primitive-allocate)

(defun primitive-class-of (obj)
  ((opencoded-lambda (o) (primitive-class-of)) obj))
(declare-inline primitive-class-of)

(defun (setter primitive-class-of) (obj class)
  ((opencoded-lambda (o c) (set-primitive-class-of)) obj class))

(defun primitive-ref (obj index)
  ((opencoded-lambda (o i) (primitive-ref)) obj index))
(declare-inline primitive-ref)

(defun (setter primitive-ref) (obj index value)
  ((opencoded-lambda (o i v) (set-primitive-ref)) obj index value))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
