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
;;;  Title: contains class definition and accessors
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: E. Ulrich Kriegel, Horst Friedrich
;;;-----------------------------------------------------------------------------

(defmodule function-i
  (import (apply-level-1
           (only (<list>) basic-list))
   syntax (%tail)
   export (<function>
           function-closure-vars
           function-address
           function-arg-descr
           function-name
           setter
           make-function))

;;;-----------------------------------------------------------------------------
;;; Class <function>
;;;-----------------------------------------------------------------------------
(%define-standard-class (<function> <class>)
  <object>
  ((closure-vars type <list>
                 default ()
                 accessor function-closure-vars
                 keyword closure-vars)
   (address type %function
            accessor function-address
            keyword address)
   (arg-descr type %signed-word-integer
              accessor function-arg-descr
              keyword arg-descr)
   ;; (defun foo () ..) --> arg-descr = 0
   ;; (defun foo (x) ..) --> arg-descr = 1
   ;; (defun foo x ..) --> arg-descr = -1
   ;; (defun foo (x y) ..) --> arg-descr = 2
   ;; (defun foo (x . y) ..) --> arg-descr = -2 ...
   (setter type <object>
           ;;type <function>: recursive use not yet possible
           accessor setter
           default
           ;; error-no-setter-defined
           () )
   (name type %string
         reader function-name)
   )
  constructor (make-function arg-descr closure-vars address)
  representation pointer-to-struct
  allocation single-card)

;;;-----------------------------------------------------------------------------
); end of module function-i
;;;-----------------------------------------------------------------------------
