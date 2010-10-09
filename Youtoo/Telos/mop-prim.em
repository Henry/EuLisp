;;; Copyright 1997 A. Kind & University of Bath
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;
;;  Youtoo is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: EuLisp's primitive-* functions
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule mop-prim
  (import (boot)
   export (primitive-allocate
           primitive-class-of
           primitive-ref))

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
)  ;; End of module mop-prim
;;;-----------------------------------------------------------------------------
