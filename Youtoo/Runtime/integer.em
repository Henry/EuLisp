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
;;; Title: integers
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule integer
  (syntax (_telos0)
   import (telos
           number)
   export (<integer>
           integer?
           even?
           odd?))

;;;-----------------------------------------------------------------------------
;;; Class <integer>
;;;-----------------------------------------------------------------------------
(defclass <integer> <number> ()
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
)  ;; End of module integer
;;;-----------------------------------------------------------------------------
