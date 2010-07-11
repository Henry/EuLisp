;;; EuLisp system 'youtoo'
;;;   Interface file for module double

(definterface double
  (import (telos compare number integer fpi string float double1)
   syntax (_telos0)
   full-import (mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos callback let-cc dynamic thread event condition compare copy number integer fpi collect convert string float double1)
   export (
    ((name . double-binary=) (pos . 12) (origin double . double-binary=) (class . ff) (arity . 2) (ff (2 2) 6 (eul_dbl_equal . ff_stub_eul_dbl_equal514)))
    ((name . most-positive-double-float) (pos . 6) (origin double . most-positive-double-float) (class . constant))
    ((name . double-truncate) (pos . 11) (origin double1 . double-truncate) (class . ff) (arity . 1) (ff (2) 0 (eul_dbl_truncate . ff_stub_eul_dbl_truncate235)))
    ((name . double-binary-lcm) (pos . 7) (origin double . double-binary-lcm))
    ((name . double-binary+) (pos . 2) (origin double1 . double-binary+) (class . ff) (arity . 2) (ff (2 2) 2 (eul_dbl_sum . ff_stub_eul_dbl_sum226)))
    ((name . most-negative-double-float) (pos . 3) (origin double . most-negative-double-float) (class . constant))
    ((name . double-binary*) (pos . 4) (origin double1 . double-binary*) (class . ff) (arity . 2) (ff (2 2) 2 (eul_dbl_product . ff_stub_eul_dbl_product228)))
    ((name . double-binary-mod) (pos . 7) (origin double1 . double-binary-mod) (class . ff) (arity . 2) (ff (2 2) 0 (eul_dbl_mod . ff_stub_eul_dbl_mod231)))
    ((name . double-round) (pos . 10) (origin double1 . double-round) (class . ff) (arity . 1) (ff (2) 0 (eul_dbl_round . ff_stub_eul_dbl_round234)))
    ((name . double-binary/) (pos . 5) (origin double1 . double-binary/) (class . ff) (arity . 2) (ff (2 2) 2 (eul_dbl_quotient . ff_stub_eul_dbl_quotient229)))
    ((name . int-as-double) (pos . 13) (origin double1 . int-as-double) (class . ff) (arity . 1) (ff (0) 2 (eul_fpi_as_dbl . ff_stub_eul_fpi_as_dbl237)))
    ((name . double?) (pos . 6) (origin float . double?))
    ((name . double-binary%) (pos . 6) (origin double1 . double-binary%) (class . ff) (arity . 2) (ff (2 2) 2 (eul_dbl_remainder . ff_stub_eul_dbl_remainder230)))
    ((name . least-positive-double-float) (pos . 4) (origin double . least-positive-double-float) (class . constant))
    ((name . least-negative-double-float) (pos . 5) (origin double . least-negative-double-float) (class . constant))
    ((name . double-ceiling) (pos . 8) (origin double1 . double-ceiling) (class . ff) (arity . 1) (ff (2) 2 (eul_dbl_ceiling . ff_stub_eul_dbl_ceiling232)))
    ((name . <double-float>) (pos . 9) (origin float . <double-float>) (class . constant))
    ((name . double-binary<) (pos . 13) (origin double . double-binary<) (class . ff) (arity . 2) (ff (2 2) 6 (eul_dbl_less . ff_stub_eul_dbl_less515)))
    ((name . double-binary-gcd) (pos . 2) (origin double . double-binary-gcd))
    ((name . double-binary-) (pos . 3) (origin double1 . double-binary-) (class . ff) (arity . 2) (ff (2 2) 2 (eul_dbl_difference . ff_stub_eul_dbl_difference227)))
    ((name . double-as-string) (pos . 12) (origin double1 . double-as-string) (class . ff) (arity . 1) (ff (2) 3 (eul_dbl_as_str . ff_stub_eul_dbl_as_str236)))
    ((name . double-floor) (pos . 9) (origin double1 . double-floor) (class . ff) (arity . 1) (ff (2) 2 (eul_dbl_floor . ff_stub_eul_dbl_floor233)))
    ((name . <double>) (pos . 7) (origin float . <double>) (class . constant))
   )
   local-literals (
    (top-level . 46)
    (double-binary-gcd . 45)
    (double-binary-lcm . 44)
    (|(method zero?)| . 40)
    (|(method binary-lcm)| . 39)
    (|(method binary-gcd)| . 38)
    (|(method truncate)| . 37)
    (|(method round)| . 36)
    (|(method floor)| . 35)
    (|(method ceiling)| . 34)
    (|(method binary<)| . 33)
    (|(method binary=)| . 32)
    (0.000000 . 14)
   )
   literals (
   )
))
