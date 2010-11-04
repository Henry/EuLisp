;;; EuLisp system 'youtoo'
;;;   Interface file for module double1

(definterface double1
  (import (telos number integer fpi float string)
   syntax (_telos0)
   full-import ()
   export (
    ((name . double-truncate) (pos . 11) (origin double1 . double-truncate) (class . ff) (arity . 1) (ff (2) 0 ("eul_dbl_truncate" . "ff_stub_eul_dbl_truncate344")))
    ((name . double-binary+) (pos . 2) (origin double1 . double-binary+) (class . ff) (arity . 2) (ff (2 2) 2 ("eul_dbl_sum" . "ff_stub_eul_dbl_sum335")))
    ((name . double-binary*) (pos . 4) (origin double1 . double-binary*) (class . ff) (arity . 2) (ff (2 2) 2 ("eul_dbl_product" . "ff_stub_eul_dbl_product337")))
    ((name . double-binary-mod) (pos . 7) (origin double1 . double-binary-mod) (class . ff) (arity . 2) (ff (2 2) 0 ("eul_dbl_mod" . "ff_stub_eul_dbl_mod340")))
    ((name . double-round) (pos . 10) (origin double1 . double-round) (class . ff) (arity . 1) (ff (2) 0 ("eul_dbl_round" . "ff_stub_eul_dbl_round343")))
    ((name . double-binary/) (pos . 5) (origin double1 . double-binary/) (class . ff) (arity . 2) (ff (2 2) 2 ("eul_dbl_quotient" . "ff_stub_eul_dbl_quotient338")))
    ((name . double-binary%) (pos . 6) (origin double1 . double-binary%) (class . ff) (arity . 2) (ff (2 2) 2 ("eul_dbl_remainder" . "ff_stub_eul_dbl_remainder339")))
    ((name . double-ceiling) (pos . 8) (origin double1 . double-ceiling) (class . ff) (arity . 1) (ff (2) 2 ("eul_dbl_ceiling" . "ff_stub_eul_dbl_ceiling341")))
    ((name . double-binary-) (pos . 3) (origin double1 . double-binary-) (class . ff) (arity . 2) (ff (2 2) 2 ("eul_dbl_difference" . "ff_stub_eul_dbl_difference336")))
    ((name . fpi-as-double) (pos . 13) (origin double1 . fpi-as-double) (class . ff) (arity . 1) (ff (0) 2 ("eul_fpi_as_dbl" . "ff_stub_eul_fpi_as_dbl346")))
    ((name . double-as-string) (pos . 12) (origin double1 . double-as-string) (class . ff) (arity . 1) (ff (2) 3 ("eul_dbl_as_str" . "ff_stub_eul_dbl_as_str345")))
    ((name . double-floor) (pos . 9) (origin double1 . double-floor) (class . ff) (arity . 1) (ff (2) 2 ("eul_dbl_floor" . "ff_stub_eul_dbl_floor342")))
   )
   local-literals (
    (top-level . 46)
    (|(method (converter <fpi>))| . 44)
    (|(method (converter <string>))| . 43)
    (|(method (converter <double-float>))| . 42)
    (|(method binary-mod)| . 41)
    (|(method binary%)| . 40)
    (|(method binary/)| . 39)
    (|(method binary*)| . 38)
    (|(method binary-)| . 37)
    (|(method binary+)| . 36)
   )
   literals (
   )
))
