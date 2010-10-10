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
;;; Title: Test object streams
;;;  Library: serial
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule test1
  (syntax (syntax-0)
   import (level-0
           serial))

(defun foo (x) x)

(defun foo1 (x) (print x nl))
(defun foo2 (x) (print x nl) (print x nl))

(defun foo3 (x) (print x nl) (print x))
(format "$$$ foo ~a\n" (eul_lambda_refs foo))
(format "$$$ foo1 ~a\n" (eul_lambda_refs foo1))
(format "$$$ foo2 ~a\n" (eul_lambda_refs foo2))
(format "$$$ foo3 ~a\n" (eul_lambda_refs foo3))
(format "$$$ + ~a\n" (eul_lambda_refs +))

;;;-----------------------------------------------------------------------------
)  ;; End of module test1
;;;-----------------------------------------------------------------------------
