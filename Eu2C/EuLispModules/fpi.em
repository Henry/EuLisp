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
;;; Title: Level-0 module fpi
;;;  Authors: E. Ulrich Kriegel
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule fpi
  (import (tail
           eulisp-kernel
           fpi-i
           integer
           (only (<double-float>
                  make-dble)
                 double-float-i)
           convert
           (only (binary+
                  binary-
                  binary/
                  binary*
                  binary%
                  binary-mod
                  binary-gcd
                  binary-lcm
                  negate
                  zero?)
                 number-generic)
           (only (binary<
                  binary=
                  equal)
                 compare-generic))
   syntax (eulisp-kernel
           (only (and)
                 syntax-i))
   export (binary+
           binary-
           binary/
           binary*
           binary%
           binary-mod
           binary<
           binary=
           binary-gcd
           binary-lcm
           negate
           zero?
           equal
           fpi?
           <fpi>
           most-positive-fpi
           most-negative-fpi))

#-(:int :big)
(defconstant most-positive-fpi #xffff)

#-(:int :big)
(defconstant most-negative-fpi #x-10000)

#+(:int :big)
(defconstant most-positive-fpi #x3fffffff)

#+(:int :big)
(defconstant most-negative-fpi #x-40000000)

;;;-----------------------------------------------------------------------------
;;; methods and functions
;;;-----------------------------------------------------------------------------
(defmethod binary+
  ((a <fpi>)
   (b <fpi>))
  (make-fpi (%plus (make-swi a)
                     (make-swi b))))

(defmethod binary-
  ((a <fpi>)
   (b <fpi>))
  (make-fpi (%minus (make-swi a)
                      (make-swi b))))

(defmethod binary*
  ((a <fpi>)
   (b <fpi>))
  (make-fpi (%mult (make-swi a)
                     (make-swi b))))

(defmethod binary/
  ((a <fpi>)
   (b <fpi>))
  (make-fpi (%div (make-swi a)
                    (make-swi b))))
(defmethod binary%
  ((a <fpi>)
   (b <fpi>))
  (make-fpi (%rem (make-swi a)
                    (make-swi b))))

(defmethod binary-mod
  ((a <fpi>)
   (b <fpi>))
  (make-fpi (%rem (make-swi a)
                    (make-swi b))))

(defmethod binary=
  ((a <fpi>)
   (b <fpi>))
  (if (%eq (%cast %signed-word-integer a)
           (%cast %signed-word-integer b))
      a
    ()))

(defmethod binary<
  ((a <fpi>)
   (b <fpi>))
  (if (%lt (%cast %signed-word-integer a)
           (%cast %signed-word-integer b))
      a
    ()))

(defmethod binary-gcd ((a <fpi>)
                       (b <fpi>))
  (make-fpi (fpi-gcd (%swi-abs (make-swi a))
                       (%swi-abs (make-swi b)))))

(defmethod binary-lcm ((a <fpi>)
                       (b <fpi>))
  (%let ((aa %signed-word-integer (%swi-abs(make-swi a)))
         (bb %signed-word-integer (%swi-abs(make-swi b))))
        (make-fpi (%div (%mult aa bb)
                          (fpi-gcd aa bb)))))

(%define-function (%swi-abs %signed-word-integer)
  ((a %signed-word-integer))
  (if (%ge a #%i0)
      a
    (%neg a)))

(%define-function (fpi-gcd %signed-word-integer)
  ((a %signed-word-integer)
   (b %signed-word-integer))
  (if (%gt a b)
      (fpi-gcd (%div a b) b)
    (if (%gt b a)
        (fpi-gcd (%div b a) a)
      a)
    ))
(defmethod negate ((a <fpi>))
  (make-fpi (%neg (make-swi a))))

(defmethod zero? ((a <fpi>))
  (binary= a 0))

(defmethod equal
  ((a <fpi>)
   (b <fpi>))
  (binary= a b))

;;;-----------------------------------------------------------------------------
;;; Converter
;;;-----------------------------------------------------------------------------
(defmethod (converter <double-float>)
  ((object <fpi>))
  (make-dble(%citod (make-swi object))))

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------
(%annotate-function
  binary+ new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <fpi>))
    ((var var1) (atom? <fpi>))
    ((var var2) (atom? <fpi>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (not <fpi>))))
    ((var var1) (atom? (and <number> (not <fpi>))))
    ((var var2) (atom? <number>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (not <fpi>))))
    ((var var1) (atom? <number>))
    ((var var2) (atom? (and <number> (not <fpi>)))))))

(%annotate-function
  binary- new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <fpi>))
    ((var var1) (atom? <fpi>))
    ((var var2) (atom? <fpi>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (not <fpi>))))
    ((var var1) (atom? (and <number> (not <fpi>))))
    ((var var2) (atom? <number>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (not <fpi>))))
    ((var var1) (atom? <number>))
    ((var var2) (atom? (and <number> (not <fpi>)))))))

(%annotate-function
  binary/ new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <fpi>))
    ((var var1) (atom? <fpi>))
    ((var var2) (atom? <fpi>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (not <fpi>))))
    ((var var1) (atom? (and <number> (not <fpi>))))
    ((var var2) (atom? <number>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (not <fpi>))))
    ((var var1) (atom? <number>))
    ((var var2) (atom? (and <number> (not <fpi>)))))))

(%annotate-function
  binary* new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <fpi>))
    ((var var1) (atom? <fpi>))
    ((var var2) (atom? <fpi>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (not <fpi>))))
    ((var var1) (atom? (and <number> (not <fpi>))))
    ((var var2) (atom? <number>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (not <fpi>))))
    ((var var1) (atom? <number>))
    ((var var2) (atom? (and <number> (not <fpi>)))))))

(%annotate-function
  binary% new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <fpi>))
    ((var var1) (atom? <fpi>))
    ((var var2) (atom? <fpi>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (not <fpi>))))
    ((var var1) (atom? (and <number> (not <fpi>))))
    ((var var2) (atom? <number>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (not <fpi>))))
    ((var var1) (atom? <number>))
    ((var var2) (atom? (and <number> (not <fpi>)))))))

(%annotate-function
  binary-mod new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <fpi>))
    ((var var1) (atom? <fpi>))
    ((var var2) (atom? <fpi>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (not <fpi>))))
    ((var var1) (atom? (and <number> (not <fpi>))))
    ((var var2) (atom? <number>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (not <fpi>))))
    ((var var1) (atom? <number>))
    ((var var2) (atom? (and <number> (not <fpi>)))))))

(%annotate-function
  binary-gcd new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <fpi>))
    ((var var1) (atom? <fpi>))
    ((var var2) (atom? <fpi>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (not <fpi>))))
    ((var var1) (atom? (and <number> (not <fpi>))))
    ((var var2) (atom? <number>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (not <fpi>))))
    ((var var1) (atom? <number>))
    ((var var2) (atom? (and <number> (not <fpi>)))))))

(%annotate-function
  binary-lcm new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <fpi>))
    ((var var1) (atom? <fpi>))
    ((var var2) (atom? <fpi>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (not <fpi>))))
    ((var var1) (atom? (and <number> (not <fpi>))))
    ((var var2) (atom? <number>)))
   ((var0 var1 var2)
    ((var var0) (atom? (and <number> (not <fpi>))))
    ((var var1) (atom? <number>))
    ((var var2) (atom? (and <number> (not <fpi>)))))))

;;;-----------------------------------------------------------------------------
)  ;; End of module fpi
;;;-----------------------------------------------------------------------------
