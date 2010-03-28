;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: bignum
;;;  Authors: Danius Michaelides, Andreas Kind
;;;  Description: bignums
;;;  Compilation: see Makefile
;;; -----------------------------------------------------------------------
(defmodule bignum
  (syntax (macros)
   import (level1 bigint bigrat)
   expose (bigint bigrat))
;;; --------------------------------------------------------------------
;;; Initialization
;;; --------------------------------------------------------------------
  (defextern eul-gmp-initialize () boolean "eul_gmp_init")
  (eul-gmp-initialize)
;;; --------------------------------------------------------------------
;;; Handle integer under/overflow with bigints
;;; --------------------------------------------------------------------
;  (defconstant *callback-vector* (get-global-register callbacks))
;  (defconstant CB-sum-overflow 30)
;  (defconstant CB-difference-underflow 31)
;  (defconstant CB-product-overflow 32)
;  (defconstant CB-read-overflow 34)
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
)  ; end of module
