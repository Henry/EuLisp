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
;;;  Library: bignum
;;;  Authors: Danius Michaelides, Andreas Kind
;;; Description: bignums
;;;  Compilation
;;    see Makefile
;;;-----------------------------------------------------------------------------
(defmodule bignum
  (syntax (macros)
   import (level1 bigint bigrat)
   expose (bigint bigrat))

;;;-----------------------------------------------------------------------------
;;; Initialization
;;;-----------------------------------------------------------------------------
(defextern eul-gmp-initialize () boolean "eul_gmp_init")
(eul-gmp-initialize)

;;;-----------------------------------------------------------------------------
;;; Handle integer under/overflow with bigints
;;;-----------------------------------------------------------------------------
;(defconstant *callback-vector* (get-global-register callbacks))
;(defconstant CB-sum-overflow 30)
;(defconstant CB-difference-underflow 31)
;(defconstant CB-product-overflow 32)
;(defconstant CB-read-overflow 34)

(install-callback CB-sum-overflow
                  (lambda (x y)
                    (binary+ (make <bigint> value: x)
                             (make <bigint> value: y))))

(install-callback CB-difference-underflow
                  (lambda (x y)
                    (binary- (make <bigint> value: x)
                             (make <bigint> value: y))))

(install-callback CB-product-overflow
                  (lambda (x y)
                    (binary* (make <bigint> value: x)
                             (make <bigint> value: y))))

(install-callback CB-read-overflow
                  (lambda (str negp)
                    (make <bigint>
                          value: (if negp
                                     (string-append "-" str)
                                   str))))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
