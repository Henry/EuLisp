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
;;;  Notes:any changes of tat type must be done conformely with the generation of
;;    trace functions for <%pointer-to-struct>- thingies in module mm-initialize.em
;;;  Requires:
;;;  Problems:any changes of tat type must be done conformely with the generation of
;;    trace functions for <%pointer-to-struct>- thingies in module mm-initialize.em
;;;  Authors: E. Ulrich Kriegel
;;    CONTACT:e. E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------
(defmodule pointer-to-void

  (import ()

   syntax (%tail)

   expose ((rename ((%object <pointer-to-void>))
                   (only (%object) %tail)))
   )

;;(export <pointer-to-void>)
;;
;;(%define-tail-class (<pointer-to-void> %tail-class)
;;                      ((ptr-to-void-value type %signed-word-integer))
;;                      representation direct)

)
