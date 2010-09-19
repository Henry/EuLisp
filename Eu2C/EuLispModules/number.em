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
;;; Title: the number module
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;    Explicit Identifiers for the C-level are given as annotations to +,
;;    -... to achieve better readability.
;;;  Requires:
;;;  Problems:
;;    no conditions til now
;;;  Authors: E. Ulrich Kriegel
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule number
  (import (eulisp-kernel
           number-i
           number-generic
           (only (accumulate accumulate1)
                 collection)
           (only (binary= binary<)
                 compare-generic))
   syntax (tail)
   expose (number-generic
           (only (binary=
                  binary<)
                 compare-generic))
   export (<number>
           number?
           +
           -
           *
           /
           %
           gcd
           lcm
           abs
           signum
           positive?
           negative?))


(defun + numbers
  (accumulate binary+ 0 numbers))
(%annotate-function + code-identifier |eu2c_plus|)

(defun - (number . more-numbers)
  (binary- number (accumulate1 binary+ more-numbers)))
(%annotate-function - code-identifier |eu2c_minus|)

(defun * numbers
  (accumulate binary* 1 numbers))
(%annotate-function * code-identifier |eu2c_mult|)

(defun / (number . more-numbers)
  (if (null? more-numbers)
      (binary/ 1 number)
    (binary/ number (accumulate1 binary* more-numbers))))
(%annotate-function / code-identifier |eu2c_div|)

(defun % (number . more-numbers)
  (if (null? more-numbers)
      number
    (binary% number (accumulate1 binary* more-numbers))))
(%annotate-function % code-identifier |eu2c_rem|)

(defun gcd (number . more-numbers)
  (if (null? more-numbers)
      number
    (accumulate binary-gcd number more-numbers)))

(defun lcm (number . more-numbers)
  (if (null? more-numbers)
      number
    (accumulate binary-lcm number more-numbers)))

(defun abs (number)
  (if (binary< number 0)
      (negate number)
    number))

(defun positive? (number)
  (if (binary< 0 number)
      t
    ()))
(%annotate-function positive? code-identifier |eu2c_positivep|)

(defun negative? (number)
  (if (binary< number 0)
      t
    ()))
(%annotate-function negative? code-identifier |eu2c_negativep|)

(defun signum(number)
  (if (zero? number)
      number
    (binary/ number (abs number))))

;;;-----------------------------------------------------------------------------
;;; annotation to reduce functions
;;;-----------------------------------------------------------------------------

;;(%annotate-function /foo/ reduce
;;       (binary-foo one-argument-tanslation zero-argument-translation
;;        translation-type)
;; at time: translation-type = acc (accumulate)

(%annotate-function + reduce   (binary+    self             0     acc))

(%annotate-function - reduce   (binary-    (negate self)    error acc))

(%annotate-function * reduce   (binary*    self             1     acc))

(%annotate-function / reduce   (binary/    (binary/ 1 self) error acc))

(%annotate-function % reduce   (binary%    self             error acc))

(%annotate-function gcd reduce (binary-gcd self             error acc))

(%annotate-function lcm reduce (binary-lcm self             error acc))

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------

;; could be more precise with compound types
(%annotate-function
  + new-signature
  (((var0 var1)
    ((var var0) (atom? fpi-zero))
    ((var var1) (atom? <null>)))
   ((var0 var1)
    ((var var0) (atom? <number>))
    ((var var1) (atom? <cons>)))))

(%annotate-function
  - new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <number>))
    ((var var1) (var var0))
    ((var var2) (atom? <null>)))
   ((var0 var1 var2)
    ((var var0) (atom? <number>))
    ((var var1) (atom? <number>))
    ((var var2) (atom? <cons>)))))

(%annotate-function
  * new-signature
  (((var0 var1)
    ((var var0) (atom? fpi-one))
    ((var var1) (atom? <null>)))
   ((var0 var1)
    ((var var0) (atom? <number>))
    ((var var1) (atom? <cons>)))))

(%annotate-function
  / new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <number>))
    ((var var1) (var var0))
    ((var var2) (atom? <null>)))
   ((var0 var1 var2)
    ((var var0) (atom? <number>))
    ((var var1) (atom? <number>))
    ((var var2) (atom? <cons>)))))

(%annotate-function
  % new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <number>))
    ((var var1) (var var0))
    ((var var2) (atom? <null>)))
   ((var0 var1 var2)
    ((var var0) (atom? <number>))
    ((var var1) (atom? <number>))
    ((var var2) (atom? <cons>)))))

(%annotate-function
  gcd new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <number>))
    ((var var1) (var var0))
    ((var var2) (atom? <null>)))
   ((var0 var1 var2)
    ((var var0) (atom? <number>))
    ((var var1) (atom? <number>))
    ((var var2) (atom? <cons>)))))

(%annotate-function
  lcm new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <number>))
    ((var var1) (var var0))
    ((var var2) (atom? <null>)))
   ((var0 var1 var2)
    ((var var0) (atom? <number>))
    ((var var1) (atom? <number>))
    ((var var2) (atom? <cons>)))))

(%annotate-function
  abs new-signature
  (((var0 var1)
    ((var var0) (atom? <number>))
    ((var var1) (var var0)))))

(%annotate-function
  positive? new-signature
  (((var0 var1)
    ((var var0) (atom? <number>))
    ((var var1) (atom? <object>)))))

(%annotate-function
  negative? new-signature
  (((var0 var1)
    ((var var0) (atom? <number>))
    ((var var1) (atom? <object>)))))

;;;-----------------------------------------------------------------------------
)
;;;-----------------------------------------------------------------------------
