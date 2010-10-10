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
;;; Title: Macros for match
;;;  Library: match
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    See match.em
;;;-----------------------------------------------------------------------------

(defmodule match0
  (syntax (syntax-0)
   import (level-0))

(defmacro letrec (inits . body)
  `(let (,@(map (lambda (init) `(,(car init) '())) inits))
     ,@(map (lambda (init) `(setq ,(car init) ,(cadr init))) inits)
     ,@body))

;;;-----------------------------------------------------------------------------
)  ;; End of module match0
;;;-----------------------------------------------------------------------------
