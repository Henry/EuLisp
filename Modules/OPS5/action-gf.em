;;; Copyright 1995 Tracy Gardner & University of Bath
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
;;; Title: Generic action functions
;;;  Library: ops5
;;;  Authors: Tracy Gardner
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------

(defmodule action-gf
  (syntax (syntax-0
           macros-tag)
   import (level-0
           basic)
   export (execute
           <action>
           <halt-action>
           make-halt-action
           get-ts))

(defclass <action> ()
  ())

(defclass <halt-action> <action>
  ()
  constructor: (make-halt-action))

(defgeneric execute ((action <action>) pi
                     wm-manager ce-manager cr-manager))

(defgeneric get-ts (ce-num prod ce-ts))

;;;-----------------------------------------------------------------------------
)  ;; End of module action-gf
;;;-----------------------------------------------------------------------------
