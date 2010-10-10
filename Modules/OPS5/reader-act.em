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
;;; Title: Functions to set up actions
;;;  Library: ops5
;;;  Authors: Tracy Gardner
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------

(defmodule reader-act
  (syntax (syntax-0
           macros-tag)
   import (level-0
           basic
           prod
           action
           ops-out)
   export (read-action))

(defconstant ops-warn format)

;;;-----------------------------------------------------------------------------
;;; read-action
; Process an action
;;;-----------------------------------------------------------------------------
(defun read-action (reader action prod)
  ;;(sformat ops-out "action: ~a~%" action)
  (let* ((type (car action))
         (act (cond
                ((eql type 'make) (read-make-action (cdr action)))
                ((eql type 'remove) (read-remove-action (cdr action)))
                ((eql type 'modify) (read-modify-action (cdr action)))
                ((eql type 'write) (read-write-action (cdr action)))
                ((eql type 'bind) (read-bind-action (cdr action)))
                ((eql type 'halt) (read-halt-action (cdr action)))
                (t (ops-warn "Action ~a not supported~%" type)))))
    (set-prod-actions prod (append (prod-actions prod) (list act))))
  reader)

(defun read-make-action (action)
  (sformat ops-out "make: ~a~%" action)
  (make-make-action (car action) (cdr action)))

(defun read-remove-action (action)
  ;;(sformat ops-out "remove: ~a~%" action)
  (make-remove-action action))

(defun read-modify-action (action)
  ;;(sformat ops-out "modify: ~a~%" action)
  (make-modify-action (car action) (cdr action)))

(defun read-write-action (action)
  ;;(sformat ops-out "write: ~a~%" action)
  (make-write-action action))

(defun read-bind-action (action)
  ;;(sformat ops-out "bind: ~a~%" action)
  (make-bind-action (car action) (cadr action)))

(defun read-halt-action (action)
  ;;(sformat ops-out "halt: ~a~%" action)
  (make-halt-action))

;;;-----------------------------------------------------------------------------
)  ;; End of module reader-act
;;;-----------------------------------------------------------------------------
