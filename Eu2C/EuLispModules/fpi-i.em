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
;;; Title: fpi-i
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule fpi-i
  (import (eulisp-kernel)
   syntax (eulisp-kernel)
   export (<fpi>
           fpi?
           make-fpi
           make-swi))

(defun fpi?
  (i)
  (%instance-of? i <fpi>))

(defgeneric (converter <fpi>)
  (object))

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------
;;(%annotate-function
;; fpi? new-signature
;; (((var0 var1)
;;   ((var var0) (atom? (and <object> (not <null>))))
;;   ((var var1) (atom? <fpi>)))
;;  ((var0 var1)
;;   ((var var0) (atom? <null>))
;;   ((var var1) (atom? (and <object> (not <fpi>)))))))

(%annotate-function
  fpi? new-signature
  (((var0 var1)
    ((var var0) (atom? <fpi>))
    ((var var1) (atom? <fpi>)))
   ((var0 var1)
    ((var var0) (atom? <null>))
    ((var var1) (atom? (and <object> (not <fpi>)))))))

;;;-----------------------------------------------------------------------------
)  ;; End of module fpi-i
;;;-----------------------------------------------------------------------------
