;;; Copyright 1997 A. Kind & University of Bath
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;
;;  Youtoo is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: Bitwise operators
;;;  Library: level1
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule bit
  (import (telos)
   export (bit-and
           bit-ior
           bit-xor
           bit-not
           bit-shift))

;;;-----------------------------------------------------------------------------
;;; All external functions
;;;-----------------------------------------------------------------------------
(defextern eul_bit_and (<fpi> <fpi>) <fpi>)
(defun bit-and (x y) (eul_bit_and x y))

(defextern eul_bit_ior (<fpi> <fpi>) <fpi>)
(defun bit-ior (x y) (eul_bit_ior x y))

(defextern eul_bit_xor (<fpi> <fpi>) <fpi>)
(defun bit-xor (x y) (eul_bit_xor x y))

(defextern eul_bit_not (<fpi>) <fpi>)
(defun bit-not (x) (eul_bit_not x))

(defextern eul_bit_shift (<fpi> <fpi>) <fpi>)
(defun bit-shift (x n) (eul_bit_shift x n))

;;;-----------------------------------------------------------------------------
)  ;; End of module bit
;;;-----------------------------------------------------------------------------
