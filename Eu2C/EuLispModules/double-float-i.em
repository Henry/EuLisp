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
;;;  Title: class definition and accessors for <double-float>
;;;  Authors: E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------

(defmodule double-float-i
  (import (eulisp-kernel
           (only (<float>)float-i))
   syntax (eulisp-kernel)
   export (<double-float> dble set-dble make-dble double-float-p))



;;;-----------------------------------------------------------------------------
;;; Class definition
;;;-----------------------------------------------------------------------------



(%define-standard-class (<double-float> <class>)
  <float>
  (

   (dble type %double-float
         reader dble
         writer set-dble
         keyword dble))
  constructor (make-dble dble)

  representation pointer-to-struct
  ;;hw requires that double are 8 byte aligned
  allocation single-card)





(defun double-float-p
  (i)
  (%instance-of-p i <double-float>))


(%define-literal-expansion float
  `(%literal ,<double-float> dble (%literal ,%double-float ,value)))



(defgeneric (converter <double-float>)
  (object))
;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------

(%define-lattice-type df-list (mono-list) (bottom) t)

;;  (%annotate-function
;;   double-float-p new-signature
;;   (((var0 var1)
;;     ((var var0) (atom (and <object> (not <null>))))
;;     ((var var1) (atom <double-float>)))
;;    ((var0 var1)
;;     ((var var0) (atom <null>))
;;     ((var var1) (atom (and <object> (not <double-float>)))))))

(%annotate-function
  double-float-p new-signature
  (((var0 var1)
    ((var var0) (atom <double-float>))
    ((var var1) (atom <double-float>)))
   ((var0 var1)
    ((var var0) (atom <null>))
    ((var var1) (atom (and <object> (not <double-float>)))))))


)
