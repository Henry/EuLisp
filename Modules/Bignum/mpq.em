;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: bignum
;;;  Authors: Danius Michaelides, Andreas Kind
;;; Description: bignums
;;;-----------------------------------------------------------------------------
(defmodule mpq
  (export (mpq-init mpq-init-set mpq-init-set-z
                    mpq-init-set-ui mpq-init-set-si mpq-clear mpq-get-num mpq-get-den
                    mpq-get-d mpq-set-num mpq-set-den mpq-mul mpq-mul-init mpq-add
                    mpq-add-init mpq-sub mpq-sub-init mpq-div mpq-div-init mpq-neg
                    mpq-neg-init mpq-inv mpq-inv-init mpq-cmp mpq-cmp-ui mpq-sgn
                    mpq-equal mpq-canonicalize))

;;;-----------------------------------------------------------------------------
;;;
;;;-----------------------------------------------------------------------------
(defextern mpq-init () ptr "C_mpq_init")
(defextern mpq-init-set (ptr) ptr "C_mpq_init_set")
(defextern mpq-init-set-z (ptr) ptr "C_mpq_init_set_z")
(defextern mpq-init-set-ui (<int> <int>) ptr "C_mpq_init_set_ui")
(defextern mpq-init-set-si (<int> <int>) ptr "C_mpq_init_set_si")
(defextern mpq-clear (ptr) boolean "C_mpq_clear")
(defextern mpq-get-num (ptr) ptr "C_mpq_get_num_init")
(defextern mpq-get-den (ptr) ptr "C_mpq_get_den_init")
(defextern mpq-get-d (ptr) <double> "C_mpq_get_d")
(defextern mpq-set-num (ptr ptr) boolean "C_mpq_set_num")
(defextern mpq-set-den (ptr ptr) boolean "C_mpq_set_den")
(defextern mpq-mul (ptr ptr ptr) boolean "C_mpq_mul")
(defextern mpq-mul-init (ptr ptr) ptr "C_mpq_mul_init")
(defextern mpq-add (ptr ptr ptr) boolean "C_mpq_add")
(defextern mpq-add-init (ptr ptr) ptr "C_mpq_add_init")
(defextern mpq-sub (ptr ptr ptr) boolean "C_mpq_sub")
(defextern mpq-sub-init (ptr ptr) ptr "C_mpq_sub_init")
(defextern mpq-div (ptr ptr ptr) boolean "C_mpq_div")
(defextern mpq-div-init (ptr ptr) ptr "C_mpq_div_init")
(defextern mpq-neg (ptr ptr) boolean "C_mpq_neg")
(defextern mpq-neg-init (ptr)  ptr "C_mpq_neg_init")
(defextern mpq-inv (ptr ptr) boolean "C_mpq_inv")
(defextern mpq-inv-init (ptr)  ptr "C_mpq_inv_init")
(defextern mpq-cmp (ptr ptr) <int> "C_mpq_cmp")
(defextern mpq-cmp-ui (ptr <int>) <int> "C_mpq_cmp_ui")
(defextern mpq-sgn (ptr) <int> "C_mpq_sgn")
(defextern mpq-equal (ptr ptr) boolean "C_mpq_equal")
(defextern mpq-canonicalize (ptr) boolean "C_mpq_canonicalize")

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
