;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: bignum
;;;  Authors: Danius Michaelides, Andreas Kind
;;; Description: bignums
;;;-----------------------------------------------------------------------------
(defmodule mpz
  (export (mpz-init
           mpz-init-set
           mpz-init-set-si
           mpz-init-set-d
           mpz-init-set-str
           mpz-clear
           mpz-get-si
           mpz-get-d
           mpz-get-str
           mpz-out-str
           ;mpz-mul
           mpz-mul-init
           ;mpz-mul-ui
           mpz-mul-ui-init
           ;mpz-add
           mpz-add-init
           ;mpz-add-ui
           mpz-add-ui-init
           ;mpz-sub
           mpz-sub-init
           ;mpz-sub-ui
           mpz-sub-ui-init
           ;mpz-mul-2exp
           mpz-mul-2exp-init
           ;mpz-neg
           mpz-neg-init
           ;mpz-abs
           mpz-abs-init
           ;mpz-fac-ui
           mpz-fac-ui-init
           ;mpz-tdiv-q
           ;mpz-tdiv-q-ui
           mpz-tdiv-q-init
           mpz-tdiv-q-ui-init
           ;mpz-tdiv-r
           mpz-tdiv-r-ui
           mpz-tdiv-r-init
           mpz-tdiv-r-ui-init
           ;mpz-tdiv-qr
           mpz-tdiv-qr-ui
           ;mpz-fdiv-q
           ;mpz-fdiv-q-ui
           mpz-fdiv-q-init
           mpz-fdiv-q-ui-init
           ;mpz-fdiv-r
           ;mpz-fdiv-r-ui
           mpz-fdiv-r-init
           ;mpz-fdiv-qr
           ;mpz-fdiv-qr-ui
           ;mpz-fdiv-ui
           ;mpz-cdiv-q
           ;mpz-cdiv-q-ui
           mpz-cdiv-q-init
           mpz-cdiv-q-ui-init
           ;mpz-cdiv-r
           ;mpz-cdiv-r-ui
           mpz-cdiv-r-init
           ;mpz-cdiv-qr
           ;mpz-cdiv-qr-ui
           ;mpz-cdiv-ui
           ;mpz-mod
           mpz-mod-init
           mpz-mod-ui
           ;mpz-divexact
           mpz-divexact-init
           ;mpz-tdiv-q-2exp
           mpz-tdiv-q-2exp-init
           ;mpz-tdiv-r-2exp
           mpz-tdiv-r-2exp-init
           ;mpz-fdiv-q-2exp
           mpz-fdiv-q-2exp-init
           ;mpz-fdiv-r-2exp
           mpz-fdiv-r-2exp-init
           ;mpz-powm
           mpz-powm-init
           ;mpz-powm-ui
           mpz-powm-ui-init
           ;mpz-pow-ui
           mpz-pow-ui-init
           ;mpz-ui-pow-ui
           mpz-ui-pow-ui-init
           ;mpz-sqrt
           mpz-sqrt-init
           ;mpz-sqrtrem
           mpz-perfect-square-p
           mpz-probab-prime-p
           ;mpz-gcd
           mpz-gcd-init
           mpz-gcd-ui
           ;mpz-gcdext
           mpz-invert
           mpz-jacobi
           mpz-legendre
           mpz-cmp
           mpz-cmp-ui
           mpz-cmp-si
           mpz-sgn
           ;mpz-and
           mpz-and-init
           ;mpz-ior
           mpz-ior-init
           ;mpz-com
           mpz-com-init
           mpz-popcount
           mpz-hamdist
           mpz-scan0
           mpz-scan1
           mpz-setbit
           mpz-clrbit
           ;mpz-random
           mpz-random-init
           ;mpz-random2
           mpz-random2-init
           mpz-sizeinbase ) )

;;;-----------------------------------------------------------------------------
;;;
;;;-----------------------------------------------------------------------------
(defextern mpz-init () ptr "C_mpz_init")
(defextern mpz-init-set (ptr) ptr "C_mpz_init_set")
(defextern mpz-init-set-si (<int>) ptr "C_mpz_init_set_si")
(defextern mpz-init-set-d (<double>) ptr "C_mpz_init_set_d")
(defextern mpz-init-set-str (<string> <int>) ptr "C_mpz_init_set_str")
(defextern mpz-clear (ptr) boolean "C_mpz_clear")
(defextern mpz-get-si (ptr) <int> "C_mpz_get_si")
(defextern mpz-get-d (ptr) <double> "C_mpz_get_d")
(defextern mpz-get-str (<int> ptr) <string> "C_mpz_get_str")
(defextern mpz-out-str (<int> ptr) boolean "C_mpz_out_str")
;(defextern mpz-mul  (ptr ptr ptr) boolean "C_mpz_mul")
(defextern mpz-mul-init (ptr ptr) ptr "C_mpz_mul_init")
;(defextern mpz-mul-ui (ptr ptr <int>) boolean "C_mpz_mul_ui")
(defextern mpz-mul-ui-init (ptr <int>) ptr "C_mpz_mul_ui_init")
;(defextern mpz-add (ptr ptr ptr) boolean "C_mpz_add")
(defextern mpz-add-init (ptr ptr) ptr "C_mpz_add_init")
;(defextern mpz-add-ui (ptr ptr <int>) boolean "C_mpz_add_ui")
(defextern mpz-add-ui-init (ptr <int>) ptr "C_mpz_add_ui_init")
;(defextern mpz-sub (ptr ptr ptr) boolean "C_mpz_sub")
(defextern mpz-sub-init (ptr ptr) ptr "C_mpz_sub_init")
;(defextern mpz-sub-ui (ptr ptr <int>) boolean "C_mpz_sub_ui")
(defextern mpz-sub-ui-init (ptr <int>) ptr "C_mpz_sub_ui_init")
;(defextern mpz-mul-2exp (ptr ptr <int>) boolean "C_mpz_mul_2exp")
(defextern mpz-mul-2exp-init (ptr <int>) ptr "C_mpz_mul_2exp_init")
;(defextern mpz-neg (ptr ptr) boolean "C_mpz_neg")
(defextern mpz-neg-init (ptr) ptr "C_mpz_neg_init")
;(defextern mpz-abs (ptr ptr) boolean "C_mpz_abs")
(defextern mpz-abs-init (ptr) ptr "C_mpz_abs_init")
;(defextern mpz-fac-ui (ptr <int>) boolean "C_mpz_fac_ui")
(defextern mpz-fac-ui-init (<int>) ptr "C_mpz_fac_ui_init")
;(defextern mpz-tdiv-q (ptr ptr ptr) boolean "C_mpz_tdiv_q")
;(defextern mpz-tdiv-q-ui (ptr ptr <int>) boolean "C_mpz_tdiv_q_ui")
(defextern mpz-tdiv-q-init (ptr ptr) ptr "C_mpz_tdiv_q_init")
(defextern mpz-tdiv-q-ui-init (ptr <int>) ptr "C_mpz_tdiv_q_ui_init")
;(defextern mpz-tdiv-r (ptr ptr ptr) boolean "C_mpz_tdiv_r")
(defextern mpz-tdiv-r-ui (ptr ptr <int>) boolean "C_mpz_tdiv_r_ui")
(defextern mpz-tdiv-r-init  (ptr ptr) ptr "C_mpz_tdiv_r_init")
(defextern mpz-tdiv-r-ui-init (ptr <int>) ptr "C_mpz_tdiv_r_ui_init")
;(defextern mpz-tdiv-qr (ptr ptr ptr ptr) boolean "C_mpz_tdiv_qr")
(defextern mpz-tdiv-qr-ui (ptr ptr ptr <int>) boolean "C_mpz_tdiv_qr_ui")
;(defextern mpz-fdiv-q (ptr ptr ptr) boolean "C_mpz_fdiv_q")
(defextern mpz-fdiv-q-ui (ptr ptr <int>) boolean "C_mpz_fdiv_q_ui")
(defextern mpz-fdiv-q-init (ptr ptr) ptr "C_mpz_fdiv_q_init")
(defextern mpz-fdiv-q-ui-init (ptr <int>) ptr "C_mpz_fdiv_q_ui_init")
;(defextern mpz-fdiv-r (ptr ptr ptr) boolean "C_mpz_fdiv_r")
;(defextern mpz-fdiv-r-ui (ptr ptr <int>) <int> "C_mpz_fdiv_r_ui")
(defextern mpz-fdiv-r-init  (ptr ptr) ptr "C_mpz_fdiv_r_init")
;(defextern mpz-fdiv-qr  (ptr ptr ptr ptr) boolean "C_mpz_fdiv_qr")
;(defextern mpz-fdiv-qr-ui (ptr ptr ptr <int>) <int> "C_mpz_fdiv_qr_ui")
;(defextern mpz-fdiv-ui  (ptr <int>) <int> "C_mpz_fdiv_ui")
(defextern mpz-cdiv-q (ptr ptr ptr) boolean "C_mpz_cdiv_q")
(defextern mpz-cdiv-q-ui  (ptr ptr <int>) boolean "C_mpz_cdiv_q_ui")
(defextern mpz-cdiv-q-init  (ptr ptr) ptr "C_mpz_cdiv_q_init")
(defextern mpz-cdiv-q-ui-init (ptr <int>) ptr "C_mpz_cdiv_q_ui_init")
;(defextern mpz-cdiv-r (ptr ptr ptr) boolean "C_mpz_cdiv_r")
;(defextern mpz-cdiv-r-ui  (ptr ptr <int>) <int> "C_mpz_cdiv_r_ui")
(defextern mpz-cdiv-r-init  (ptr ptr) ptr "C_mpz_cdiv_r_init")
;(defextern mpz-cdiv-qr  (ptr ptr ptr ptr) boolean "C_mpz_cdiv_qr")
;(defextern mpz-cdiv-qr-ui (ptr ptr ptr <int>) <int> "C_mpz_cdiv_qr_ui")
;(defextern mpz-cdiv-ui  (ptr <int>) <int> "C_mpz_cdiv_ui")
;(defextern mpz-mod (ptr ptr ptr) boolean "C_mpz_mod")
(defextern mpz-mod-init (ptr ptr) ptr "C_mpz_mod_init")
(defextern mpz-mod-ui (ptr ptr <int>) <int> "C_mpz_mod_ui")
;(defextern mpz-divexact (ptr ptr ptr) boolean "C_mpz_divexact")
(defextern mpz-divexact-init (ptr ptr) ptr "C_mpz_divexact_init")
;(defextern mpz-tdiv-q-2exp (ptr ptr <int>) boolean "C_mpz_tdiv_q_2exp")
(defextern mpz-tdiv-q-2exp-init (ptr <int>) ptr "C_mpz_tdiv_q_2exp_init")
;(defextern mpz-tdiv-r-2exp (ptr ptr <int>) boolean "C_mpz_tdiv_r_2exp")
(defextern mpz-tdiv-r-2exp-init (ptr <int>) ptr "C_mpz_tdiv_r_2exp_init")
;(defextern mpz-fdiv-q-2exp (ptr ptr <int>) boolean "C_mpz_fdiv_q_2exp")
(defextern mpz-fdiv-q-2exp-init (ptr <int>) ptr "C_mpz_fdiv_q_2exp_init")
;(defextern mpz-fdiv-r-2exp (ptr ptr <int>) boolean "C_mpz_fdiv_r_2exp")
(defextern mpz-fdiv-r-2exp-init (ptr <int>) ptr "C_mpz_fdiv_r_2exp_init")
;(defextern mpz-powm (ptr ptr ptr ptr) boolean "C_mpz_powm")
(defextern mpz-powm-init (ptr ptr ptr) ptr "C_mpz_powm_init")
;(defextern mpz-powm-ui (ptr <int> ptr) boolean "C_mpz_powm_ui")
(defextern mpz-powm-ui-init (<int> <int>)  ptr "C_mpz_powm_ui_init")
;(defextern mpz-pow-ui (ptr ptr <int>)  boolean "C_mpz_pow_ui")
(defextern mpz-pow-ui-init (ptr <int>)  ptr  "C_mpz_pow_ui_init")
;(defextern mpz-ui-pow-ui (ptr <int> <int>) boolean "C_mpz_ui_pow_ui")
(defextern mpz-ui-pow-ui-init (<int> <int>)  ptr "C_mpz_ui_pow_ui_init")

;;;-----------------------------------------------------------------------------
;;;
;;;-----------------------------------------------------------------------------
;(defextern mpz-sqrt (ptr ptr) boolean "C_mpz_sqrt")
(defextern mpz-sqrt-init (ptr) ptr "C_mpz_sqrt_init")
;(defextern mpz-sqrtrem (ptr ptr ptr) boolean "C_mpz_sqrtrem")
(defextern mpz-perfect-square-p (ptr) boolean "C_mpz_perfect_square_p")

;;;-----------------------------------------------------------------------------
;;;
;;;-----------------------------------------------------------------------------
(defextern mpz-probab-prime-p (ptr <int>) boolean "C_mpz_probab_prime_p")
;(defextern mpz-gcd (ptr ptr ptr) boolean "C_mpz_gcd")
(defextern mpz-gcd-init (ptr ptr) ptr "C_mpz_gcd_init")
(defextern mpz-gcd-ui (ptr ptr <int>) <int> "C_mpz_gcd_ui")
;(defextern mpz-gcdext (ptr ptr ptr ptr ptr) boolean "C_mpz_gcdext")
(defextern mpz-invert (ptr ptr ptr) <int> "C_mpz_invert")
(defextern mpz-jacobi (ptr ptr) <int> "C_mpz_jacobi")
(defextern mpz-legendre (ptr ptr) <int> "C_mpz_legendre")

;;;-----------------------------------------------------------------------------
;;;
;;;-----------------------------------------------------------------------------
(defextern mpz-cmp (ptr ptr) <int> "C_mpz_cmp")
(defextern mpz-cmp-ui (ptr <int>) <int> "C_mpz_cmp_ui")
(defextern mpz-cmp-si (ptr <int>) <int> "C_mpz_cmp_si")
(defextern mpz-sgn (ptr) <int> "C_mpz_sgn")

;;;-----------------------------------------------------------------------------
;;;
;;;-----------------------------------------------------------------------------
;(defextern mpz-and (ptr ptr ptr) boolean "C_mpz_and")
(defextern mpz-and-init (ptr ptr) ptr "C_mpz_and_init")
;(defextern mpz-ior (ptr ptr ptr) boolean "C_mpz_ior")
(defextern mpz-ior-init (ptr ptr) ptr "C_mpz_ior_init")
;(defextern mpz-com (ptr ptr) boolean "C_mpz_com")
(defextern mpz-com-init (ptr) ptr "C_mpz_com_init")
(defextern mpz-popcount (ptr) <int> "C_mpz_popcount")
(defextern mpz-hamdist (ptr ptr) <int> "C_mpz_hamdist")
(defextern mpz-scan0 (ptr <int>) <int> "C_mpz_scan0")
(defextern mpz-scan1 (ptr <int>) <int> "C_mpz_scan1")
(defextern mpz-setbit (ptr <int>) boolean "C_mpz_setbit")
(defextern mpz-clrbit (ptr <int>) boolean "C_mpz_clrbit")

;;;-----------------------------------------------------------------------------
;;;
;;;-----------------------------------------------------------------------------
;(defextern mpz-random (ptr <int>) boolean "C_mpz_random")
(defextern mpz-random-init (<int>) ptr "C_mpz_random_init")
;(defextern mpz-random2 (ptr <int>) boolean "C_mpz_random2")
(defextern mpz-random2-init (<int>) ptr "C_mpz_random2_init")
(defextern mpz-sizeinbase (ptr <int>) <int> "C_mpz_sizeinbase")

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
