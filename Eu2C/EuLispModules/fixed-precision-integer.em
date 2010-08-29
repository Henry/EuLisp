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
;;;  Title: fixed-precision-integer.am
;;;  Problems:
;;;  Authors: E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------

(defmodule fixed-precision-integer
  (import (tail eulisp-kernel
                fixed-precision-integer-i
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
                       zerop)
                      number-generic)
                (only (binary<
                       binary=
                       equal)
                      compare-generic))
   syntax (eulisp-kernel
           (only (and)
                 syntax-0))
   export
   (;;functions or methods
    binary+
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
    zerop
    equal
    fixed-precision-integer-p
    ;;constants
    <fpi>
    most-positive-fixed-precision-integer
    most-negative-fixed-precision-integer
    ;;classes
    <fixed-precision-integer>))

(defconstant <fpi> <fixed-precision-integer>)
#-(:fixed-precision-integer :big)
(defconstant most-positive-fixed-precision-integer #xffff)

#-(:fixed-precision-integer :big)
(defconstant most-negative-fixed-precision-integer #x-10000)

#+(:fixed-precision-integer :big)
(defconstant most-positive-fixed-precision-integer #x3fffffff)

#+(:fixed-precision-integer :big)
(defconstant most-negative-fixed-precision-integer #x-40000000)

;;;-----------------------------------------------------------------------------
;;; methods and functions
;;;-----------------------------------------------------------------------------
(defmethod binary+
  ((a <fixed-precision-integer>)
   (b <fixed-precision-integer>))
  (make-fpint (%plus (make-swi a)
                     (make-swi b))))

(defmethod binary-
  ((a <fixed-precision-integer>)
   (b <fixed-precision-integer>))
  (make-fpint (%minus (make-swi a)
                      (make-swi b))))


(defmethod binary*
  ((a <fixed-precision-integer>)
   (b <fixed-precision-integer>))
  (make-fpint (%mult (make-swi a)
                     (make-swi b))))


(defmethod binary/
  ((a <fixed-precision-integer>)
   (b <fixed-precision-integer>))
  (make-fpint (%div (make-swi a)
                    (make-swi b))))
(defmethod binary%
  ((a <fixed-precision-integer>)
   (b <fixed-precision-integer>))
  (make-fpint (%rem (make-swi a)
                    (make-swi b))))

(defmethod binary-mod
  ((a <fixed-precision-integer>)
   (b <fixed-precision-integer>))
  (make-fpint (%rem (make-swi a)
                    (make-swi b))))

(defmethod binary=
  ((a <fixed-precision-integer>)
   (b <fixed-precision-integer>))
  (if (%eq (%cast %signed-word-integer a)
           (%cast %signed-word-integer b))
      a
    ()))

(defmethod binary<
  ((a <fixed-precision-integer>)
   (b <fixed-precision-integer>))
  (if (%lt (%cast %signed-word-integer a)
           (%cast %signed-word-integer b))
      a
    ()))

(defmethod binary-gcd ((a <fixed-precision-integer>)
                       (b <fixed-precision-integer>))
  (make-fpint (fpi-gcd (%swi-abs (make-swi a))
                       (%swi-abs (make-swi b)))))

(defmethod binary-lcm ((a <fixed-precision-integer>)
                       (b <fixed-precision-integer>))
  (%let ((aa %signed-word-integer (%swi-abs(make-swi a)))
         (bb %signed-word-integer (%swi-abs(make-swi b))))
        (make-fpint (%div (%mult aa bb)
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
(defmethod negate ((a <fixed-precision-integer>))
  (make-fpint (%neg (make-swi a))))

(defmethod zerop ((a <fixed-precision-integer>))
  (binary= a 0))

(defmethod equal
  ((a <fixed-precision-integer>)
   (b <fixed-precision-integer>))
  (binary= a b))

;;;-----------------------------------------------------------------------------
;;; Converter
;;;-----------------------------------------------------------------------------
(defmethod (converter <double-float>)
  ((object <fixed-precision-integer>))
  (make-dble(%citod (make-swi object))))

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------
(%annotate-function
  binary+ new-signature
  (((var0 var1 var2)
    ((var var0) (atom <fixed-precision-integer>))
    ((var var1) (atom <fixed-precision-integer>))
    ((var var2) (atom <fixed-precision-integer>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var1) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var2) (atom <number>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var1) (atom <number>))
    ((var var2) (atom (and <number> (not <fixed-precision-integer>)))))))

(%annotate-function
  binary- new-signature
  (((var0 var1 var2)
    ((var var0) (atom <fixed-precision-integer>))
    ((var var1) (atom <fixed-precision-integer>))
    ((var var2) (atom <fixed-precision-integer>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var1) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var2) (atom <number>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var1) (atom <number>))
    ((var var2) (atom (and <number> (not <fixed-precision-integer>)))))))

(%annotate-function
  binary/ new-signature
  (((var0 var1 var2)
    ((var var0) (atom <fixed-precision-integer>))
    ((var var1) (atom <fixed-precision-integer>))
    ((var var2) (atom <fixed-precision-integer>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var1) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var2) (atom <number>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var1) (atom <number>))
    ((var var2) (atom (and <number> (not <fixed-precision-integer>)))))))

(%annotate-function
  binary* new-signature
  (((var0 var1 var2)
    ((var var0) (atom <fixed-precision-integer>))
    ((var var1) (atom <fixed-precision-integer>))
    ((var var2) (atom <fixed-precision-integer>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var1) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var2) (atom <number>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var1) (atom <number>))
    ((var var2) (atom (and <number> (not <fixed-precision-integer>)))))))

(%annotate-function
  binary% new-signature
  (((var0 var1 var2)
    ((var var0) (atom <fixed-precision-integer>))
    ((var var1) (atom <fixed-precision-integer>))
    ((var var2) (atom <fixed-precision-integer>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var1) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var2) (atom <number>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var1) (atom <number>))
    ((var var2) (atom (and <number> (not <fixed-precision-integer>)))))))

(%annotate-function
  binary-mod new-signature
  (((var0 var1 var2)
    ((var var0) (atom <fixed-precision-integer>))
    ((var var1) (atom <fixed-precision-integer>))
    ((var var2) (atom <fixed-precision-integer>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var1) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var2) (atom <number>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var1) (atom <number>))
    ((var var2) (atom (and <number> (not <fixed-precision-integer>)))))))

(%annotate-function
  binary-gcd new-signature
  (((var0 var1 var2)
    ((var var0) (atom <fixed-precision-integer>))
    ((var var1) (atom <fixed-precision-integer>))
    ((var var2) (atom <fixed-precision-integer>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var1) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var2) (atom <number>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var1) (atom <number>))
    ((var var2) (atom (and <number> (not <fixed-precision-integer>)))))))

(%annotate-function
  binary-lcm new-signature
  (((var0 var1 var2)
    ((var var0) (atom <fixed-precision-integer>))
    ((var var1) (atom <fixed-precision-integer>))
    ((var var2) (atom <fixed-precision-integer>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var1) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var2) (atom <number>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <fixed-precision-integer>))))
    ((var var1) (atom <number>))
    ((var var2) (atom (and <number> (not <fixed-precision-integer>)))))))

;;;-----------------------------------------------------------------------------
)
;;;-----------------------------------------------------------------------------
