;;; Copyright 1994-2010 Fraunhofer ISST
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'Eu2C'
;;;-----------------------------------------------------------------------------
;;
;;  Eu2C is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title:
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Horst Friedrich
;;;-----------------------------------------------------------------------------
(defmodule closure
  (import (apply-level-1
           (only (<cons>
                  cons
                  car
                  cdr
                  <list>)
                 basic-list)
           basic-compare
           function)
   syntax (apply-level-1
           basic-syntax)
   export (%closure-push
           %closure-value
           %set-closure-value
           %make-function))

(defun %closure-push (value closure)
  (cons value closure))

(%define-function (%closure-value <object>)
  ((closure <cons>)  (nr %signed-word-integer))
  (if (%eq nr #%i0) (car closure)
    (%closure-value (cdr closure) (%minus nr #%i1))))

(%define-function (%set-closure-value <object>)
  ((closure <cons>)
   (nr %signed-word-integer)
   (value <object>))
  (if (eq nr #%i0)  (%setf (%select closure <cons> car) value)
    (%set-closure-value (cdr closure)
                        (%minus nr #%i1)
                        value)))

(%define-function (%make-function <object>)
  ((arg %signed-word-integer)
   (closure <list>)
   (fun %function))
  (make-function arg closure fun))

); end of module closure
