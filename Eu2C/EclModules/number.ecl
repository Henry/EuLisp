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
;;;  Title: EL-in-CL: the abstract number classes and its operations
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module number

(import (eulisp-kernel
         number-i
         (only (binary< binary=)
               compare-generic)
         (only (< = + - * / mod gcd lcm rem plusp minusp)
               common-lisp))

 syntax (eulisp-kernel
         number-i)

 expose ((only (numberp
                integerp
                floatp
                + - * / < > <= >=
                max
                min
                gcd
                lcm
                abs
                zerop
                signum
                ceiling
                floor
                round
                truncate
                evenp
                oddp)
               common-lisp)
         number-i)

 export (binary<
         binary=
         binary+
         binary-
         binary*
         binary/
         binary-mod
         binary-gcd
         binary-lcm
         binary%
         %
         negate
         positivep
         negativep))


;;(make-eulisp-class number) ; now in `number-i'
(make-eulisp-class integer)
(make-eulisp-class float)

(defmethod binary< ((n1 <number>) (n2 <number>))
  (< n1 n2))

(defmethod binary= ((n1 <number>) (n2 <number>))
  (= n1 n2))

(defun binary+ (n1 n2)
  (+ n1 n2))

(defun binary- (n1 n2)
  (- n1 n2))

(defun binary* (n1 n2)
  (* n1 n2))

(defun binary/ (n1 n2)
  (/ n1 n2))

(defun binary-mod (n1 n2)
  (mod n1 n2))

(defun binary-gcd (n1 n2)
  (gcd n1 n2))

(defun binary-lcm (n1 n2)
  (lcm n1 n2))

(defun binary% (n1 n2)
  (rem n1 n2))

(defun % (n1 n2)
  (rem n1 n2))

(defun negate (n)
  (- n))

(defun positivep (n)
  (plusp n))

(defun negativep (n)
  (minusp n))

#module-end
