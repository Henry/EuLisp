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
;;;  Title: Encoding Lattice Types
;;;  Description:
;;    Bit codes are assigned to lattice types in order to use fast low-level
;;    bit operations to implement the lattice operations meet, join, and
;;    complement. This modules provides the bit codes and operations on
;;    them.
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Andreas Kind
;;;-----------------------------------------------------------------------------


#module ti-codes
(import (ti
         (only (logand logior logxor logandc2 expt minusp) common-lisp))
 syntax (ti)
 export (bottom-code
         reset-used-codes
         next-code
         join-codes
         meet-codes
         complement-code
         substract-codes
         xor-codes
         eq-code-p
         meet-codes-p
         subcode-p
         bottom-code?
         complement-codes?))

;;;-----------------------------------------------------------------------------
;;; GENERATING NEW LATTICE TYPE CODES
;;;-----------------------------------------------------------------------------

;;; Answer the constant bottom code.
(defun bottom-code ()
  0)

(deflocal *used-codes* (bottom-code))

(defun reset-used-codes () (setq *used-codes* (bottom-code)))

;;; Answer a new type code; increment number of used codes.
(defun next-code ()
  (let ((new-code (expt 2 *used-codes*)))
    (setf *used-codes* (+ 1 *used-codes*))
    new-code))

;;; Meet lattice type codes.
(defun join-codes (code1 code2)
  (if (minusp code1)
      (if (minusp code2)
          (- (logand (abs code1) (abs code2)))
        (- (logandc2 (abs code1) code2)))
    (if (minusp code2)
        (- (logandc2 (abs code2) code1))
      (logior code1 code2))))

;;; Meet lattice type codes.
(defun meet-codes (code1 code2)
  (if (minusp code1)
      (if (minusp code2)
          (- (logior (abs code1) (abs code2)))
        (logandc2 code2 (abs code1)))
    (if (minusp code2)
        (logandc2 code1 (abs code2))
      (logand code1 code2))))

;;; Complement lattice type codes.
(defun complement-code (code)
  (- code))

;;; Xor lattice type codes.
(defun xor-codes (code1 code2)
  (logxor code1 code2))

;;; Substract lattice type codes.
(defun substract-codes (code1 code2)
  (logandc2 code1 code2))

;;;-----------------------------------------------------------------------------
;;; TYPE CODE PREDICATES
;;;-----------------------------------------------------------------------------

;;; Answer whether two type codes are equal.
(defun eq-code-p (code1 code2)
  (= code1 code2))

;;; Answer whether a type code is the bottom type code.
(defun bottom-code? (code)
  (eq-code-p code (bottom-code)))

;;; Answer whether two type codes do meet, e.g. do have set the same bits.
(defun meet-codes-p (code1 code2)
  (null? (bottom-code? (meet-codes code1 code2))))

;;; Answer whether one type code is a subcode of another.
(defun subcode-p (code1 code2)
  (eq-code-p (meet-codes code1 code2) code1))

;;; Answer whether two lattice type codes are complementary.
(defun complement-codes? (code1 code2)
  (zero? (+ code1 code2)))


#module-end
