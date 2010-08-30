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
;;;  Title: int.am
;;;  Problems:
;;;  Authors: E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------

(defmodule int
  (import (tail eulisp-kernel
                int-i
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
    int-p
    ;;constants
    <fpi>
    most-positive-int
    most-negative-int
    ;;classes
    <int>))

(defconstant <fpi> <int>)
#-(:int :big)
(defconstant most-positive-int #xffff)

#-(:int :big)
(defconstant most-negative-int #x-10000)

#+(:int :big)
(defconstant most-positive-int #x3fffffff)

#+(:int :big)
(defconstant most-negative-int #x-40000000)

;;;-----------------------------------------------------------------------------
;;; methods and functions
;;;-----------------------------------------------------------------------------
(defmethod binary+
  ((a <int>)
   (b <int>))
  (make-fpint (%plus (make-swi a)
                     (make-swi b))))

(defmethod binary-
  ((a <int>)
   (b <int>))
  (make-fpint (%minus (make-swi a)
                      (make-swi b))))


(defmethod binary*
  ((a <int>)
   (b <int>))
  (make-fpint (%mult (make-swi a)
                     (make-swi b))))


(defmethod binary/
  ((a <int>)
   (b <int>))
  (make-fpint (%div (make-swi a)
                    (make-swi b))))
(defmethod binary%
  ((a <int>)
   (b <int>))
  (make-fpint (%rem (make-swi a)
                    (make-swi b))))

(defmethod binary-mod
  ((a <int>)
   (b <int>))
  (make-fpint (%rem (make-swi a)
                    (make-swi b))))

(defmethod binary=
  ((a <int>)
   (b <int>))
  (if (%eq (%cast %signed-word-integer a)
           (%cast %signed-word-integer b))
      a
    ()))

(defmethod binary<
  ((a <int>)
   (b <int>))
  (if (%lt (%cast %signed-word-integer a)
           (%cast %signed-word-integer b))
      a
    ()))

(defmethod binary-gcd ((a <int>)
                       (b <int>))
  (make-fpint (fpi-gcd (%swi-abs (make-swi a))
                       (%swi-abs (make-swi b)))))

(defmethod binary-lcm ((a <int>)
                       (b <int>))
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
(defmethod negate ((a <int>))
  (make-fpint (%neg (make-swi a))))

(defmethod zerop ((a <int>))
  (binary= a 0))

(defmethod equal
  ((a <int>)
   (b <int>))
  (binary= a b))

;;;-----------------------------------------------------------------------------
;;; Converter
;;;-----------------------------------------------------------------------------
(defmethod (converter <double-float>)
  ((object <int>))
  (make-dble(%citod (make-swi object))))

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------
(%annotate-function
  binary+ new-signature
  (((var0 var1 var2)
    ((var var0) (atom <int>))
    ((var var1) (atom <int>))
    ((var var2) (atom <int>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <int>))))
    ((var var1) (atom (and <number> (not <int>))))
    ((var var2) (atom <number>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <int>))))
    ((var var1) (atom <number>))
    ((var var2) (atom (and <number> (not <int>)))))))

(%annotate-function
  binary- new-signature
  (((var0 var1 var2)
    ((var var0) (atom <int>))
    ((var var1) (atom <int>))
    ((var var2) (atom <int>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <int>))))
    ((var var1) (atom (and <number> (not <int>))))
    ((var var2) (atom <number>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <int>))))
    ((var var1) (atom <number>))
    ((var var2) (atom (and <number> (not <int>)))))))

(%annotate-function
  binary/ new-signature
  (((var0 var1 var2)
    ((var var0) (atom <int>))
    ((var var1) (atom <int>))
    ((var var2) (atom <int>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <int>))))
    ((var var1) (atom (and <number> (not <int>))))
    ((var var2) (atom <number>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <int>))))
    ((var var1) (atom <number>))
    ((var var2) (atom (and <number> (not <int>)))))))

(%annotate-function
  binary* new-signature
  (((var0 var1 var2)
    ((var var0) (atom <int>))
    ((var var1) (atom <int>))
    ((var var2) (atom <int>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <int>))))
    ((var var1) (atom (and <number> (not <int>))))
    ((var var2) (atom <number>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <int>))))
    ((var var1) (atom <number>))
    ((var var2) (atom (and <number> (not <int>)))))))

(%annotate-function
  binary% new-signature
  (((var0 var1 var2)
    ((var var0) (atom <int>))
    ((var var1) (atom <int>))
    ((var var2) (atom <int>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <int>))))
    ((var var1) (atom (and <number> (not <int>))))
    ((var var2) (atom <number>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <int>))))
    ((var var1) (atom <number>))
    ((var var2) (atom (and <number> (not <int>)))))))

(%annotate-function
  binary-mod new-signature
  (((var0 var1 var2)
    ((var var0) (atom <int>))
    ((var var1) (atom <int>))
    ((var var2) (atom <int>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <int>))))
    ((var var1) (atom (and <number> (not <int>))))
    ((var var2) (atom <number>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <int>))))
    ((var var1) (atom <number>))
    ((var var2) (atom (and <number> (not <int>)))))))

(%annotate-function
  binary-gcd new-signature
  (((var0 var1 var2)
    ((var var0) (atom <int>))
    ((var var1) (atom <int>))
    ((var var2) (atom <int>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <int>))))
    ((var var1) (atom (and <number> (not <int>))))
    ((var var2) (atom <number>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <int>))))
    ((var var1) (atom <number>))
    ((var var2) (atom (and <number> (not <int>)))))))

(%annotate-function
  binary-lcm new-signature
  (((var0 var1 var2)
    ((var var0) (atom <int>))
    ((var var1) (atom <int>))
    ((var var2) (atom <int>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <int>))))
    ((var var1) (atom (and <number> (not <int>))))
    ((var var2) (atom <number>)))
   ((var0 var1 var2)
    ((var var0) (atom (and <number> (not <int>))))
    ((var var1) (atom <number>))
    ((var var2) (atom (and <number> (not <int>)))))))

;;;-----------------------------------------------------------------------------
)
;;;-----------------------------------------------------------------------------
