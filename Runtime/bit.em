;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Andreas Kind
;;;  Description: Bitwise operators
;;; -----------------------------------------------------------------------
(defmodule bit
  (import (telos)
   export (bit-and bit-ior bit-xor bit-not bit-shift))
;;; --------------------------------------------------------------------
;;; All external functions
;;; --------------------------------------------------------------------
  (defextern eul_bit_and (<int> <int>) <int>)
  (defun bit-and (x y) (eul_bit_and x y))
  (defextern eul_bit_ior (<int> <int>) <int>)
  (defun bit-ior (x y) (eul_bit_ior x y))
  (defextern eul_bit_xor (<int> <int>) <int>)
  (defun bit-xor (x y) (eul_bit_xor x y))
  (defextern eul_bit_not (<int>) <int>)
  (defun bit-not (x) (eul_bit_not x))
  (defextern eul_bit_shift (<int> <int>) <int>)
  (defun bit-shift (x n) (eul_bit_shift x n))
)  ;; end of module