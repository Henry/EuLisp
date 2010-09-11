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
;;;  Title: Example 'test-introspection'
;;;  Description:
;;    Basic tests of class introspection
;;;  Authors: Henry G. Weller
;;;-----------------------------------------------------------------------------
(defmodule test-introspection
  (import (level-0
           tail
           function ; <function>
           basic-list
           ;; printf-1 ; for debug
           basic-compare ; eq
           tail-introspection ; %class-of, %member
           ;; string ; for error messages
           ;; symbol
           ;; vector
           (rename ((no-applicable-method-error
                     no-applicable-method))
                   basic-condition))
   syntax (level-0 apply-level-1
           basic-syntax))

(%define-function (class-precedence-list <cons>)
  ((cl <class>))
  (%select cl <class> class-precedence-list))

(%define-function (class-name <symbol>)
  ((cl <class>))
  (%select cl <class> class-name))

;; (%define-function (class-name <symbol>)
;;   ((cla <class>))
;;   (%class-name cla))

(print (class-name (car (cdr (class-precedence-list <list>)))))
(print (class-name <list>))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
