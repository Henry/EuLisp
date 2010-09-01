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
;;;  Authors: E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------

(defmodule integer
  (import (eulisp-kernel
           (only (binary-mod
                  zero?)
                 number-generic))
   syntax (eulisp-kernel)
   expose (integer-generic)
   export (integer?
           <integer>
           even?
           odd?))

(defun integer?
  (i)
  (%instance-of? i <integer>))

;;;-----------------------------------------------------------------------------
;;; Even and odd
;;;-----------------------------------------------------------------------------
(defun even? (x)
  (if (zero? (binary-mod x 2))
      t
    ()))

(defun odd? (x)
  (null? (zero? (binary-mod x 2))))

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------

(%annotate-function
  integer? new-signature
  (((var0 var1)
    ((var var0) (atom? <integer>))
    ((var var1) (atom? <integer>)))
   ((var0 var1)
    ((var var0) (atom? <null>))
    ((var var1) (atom? (and <object> (not <integer>)))))))

(%annotate-function
  even? new-signature
  (((var0 var1)
    ((var var0) (atom? <object>))
    ((var var1) (atom? <integer>)))))

(%annotate-function
  odd? new-signature
  (((var0 var1)
    ((var var0) (atom? <object>))
    ((var var1) (atom? <integer>)))))

;;;-----------------------------------------------------------------------------
)
;;;-----------------------------------------------------------------------------
