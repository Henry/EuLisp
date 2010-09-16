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
;;;  Authors: Rainer Rosenmuller
;;;-----------------------------------------------------------------------------

(defmodule stream-ii
  (import (tail
           (only (<function>)
                 function-i))
   syntax (tail)
   export (<stream>
           stream?
           stream-direction
           setf-stream-direction
           stream-transaction-unit
           setf-stream-transaction-unit
           stream-positionable
           setf-stream-positionable
           stream-opened
           setf-stream-opened
           stream-eos-action
           setf-stream-eos-action))

(%define-standard-class (<stream> <class>)
  <object>
  ((direction type <symbol>
              reader stream-direction
              writer setf-stream-direction
              keyword direction)
   (transaction-unit type <object>
                     reader stream-transaction-unit
                     writer setf-stream-transaction-unit)
   (positionable type <object>
                 reader stream-positionable
                 writer setf-stream-positionable)
   (opened type <object>
           reader stream-opened
           writer setf-stream-opened)
   (eos-action type <function> reader stream-eos-action
               writer setf-stream-eos-action))
  predicate stream?
  representation pointer-to-struct)

;;;-----------------------------------------------------------------------------
)  ;; End of module stream-ii
;;;-----------------------------------------------------------------------------
