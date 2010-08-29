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
;;;-----------------------------------------------------------------------------

(defmodule fixed-precision-integer-i
  (import (eulisp-kernel)
   syntax (eulisp-kernel)
   export (<fixed-precision-integer>
           fixed-precision-integer-p
           make-fpint
           make-swi))

(defun fixed-precision-integer-p
  (i)
  (%instance-of-p i <fixed-precision-integer>))

(defgeneric (converter <fixed-precision-integer>)
  (object))


;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------

;;(%annotate-function
;; fixed-precision-integer-p new-signature
;; (((var0 var1)
;;   ((var var0) (atom (and <object> (not <null>))))
;;   ((var var1) (atom <fixed-precision-integer>)))
;;  ((var0 var1)
;;   ((var var0) (atom <null>))
;;   ((var var1) (atom (and <object> (not <fixed-precision-integer>)))))))

(%annotate-function
  fixed-precision-integer-p new-signature
  (((var0 var1)
    ((var var0) (atom <fixed-precision-integer>))
    ((var var1) (atom <fixed-precision-integer>)))
   ((var0 var1)
    ((var var0) (atom <null>))
    ((var var1) (atom (and <object> (not <fixed-precision-integer>)))))))

;;;-----------------------------------------------------------------------------
)
;;;-----------------------------------------------------------------------------
