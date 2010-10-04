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
;;; Title: int-i
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule int-i
  (import (eulisp-kernel)
   syntax (eulisp-kernel)
   export (<int>
           int?
           make-fpint
           make-swi))

(defun int?
  (i)
  (%instance-of? i <int>))

(defgeneric (converter <int>)
  (object))

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------
;;(%annotate-function
;; int? new-signature
;; (((var0 var1)
;;   ((var var0) (atom? (and <object> (not <null>))))
;;   ((var var1) (atom? <int>)))
;;  ((var0 var1)
;;   ((var var0) (atom? <null>))
;;   ((var var1) (atom? (and <object> (not <int>)))))))

(%annotate-function
  int? new-signature
  (((var0 var1)
    ((var var0) (atom? <int>))
    ((var var1) (atom? <int>)))
   ((var0 var1)
    ((var var0) (atom? <null>))
    ((var var1) (atom? (and <object> (not <int>)))))))

;;;-----------------------------------------------------------------------------
)  ;; End of module int-i
;;;-----------------------------------------------------------------------------
