;;; EuLisp system 'youtoo'
;;;   Interface file for module fpi

(definterface fpi
  (import (telos compare number integer)
   syntax (_telos0)
   full-import (mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos callback let-cc dynamic thread event condition compare copy number integer)
   export (
    ((name . fixed-precision-integer-p) (pos . 7) (origin fpi . fixed-precision-integer-p))
    ((name . int-binary%) (pos . 34) (origin boot1 . %) (inline (G0068 (fpi-remainder))))
    ((name . intp) (pos . 32) (origin boot1 . intp) (inline (G0049 (fpip))))
    ((name . <fpi>) (pos . 6) (origin fpi . <fpi>) (class . constant))
    ((name . most-negative-fixed-precision-integer) (pos) (origin fpi . most-negative-fixed-precision-integer) (class . constant) (value -536870911))
    ((name . int-binary-) (pos . 18) (origin boot1 . -) (inline (G0062 (fpi-difference))))
    ((name . int-binary-mod) (pos . 40) (origin boot1 . mod) (inline (G0070 (fpi-remainder))))
    ((name . int-as-string) (pos . 8) (origin fpi . int-as-string) (class . ff) (arity . 1) (ff (0) 3 (eul_int_as_str . ff_stub_eul_int_as_str2296)))
    ((name . int-binary/) (pos . 14) (origin boot1 . /) (inline (G0066 (fpi-quotient))))
    ((name . int-binary*) (pos . 35) (origin boot1 . *) (inline (G0064 (fpi-product))))
    ((name . most-positive-fixed-precision-integer) (pos) (origin fpi . most-positive-fixed-precision-integer) (class . constant) (value 536870911))
    ((name . <int>) (pos . 5) (origin fpi . <int>) (class . constant))
    ((name . <fixed-precision-integer>) (pos . 2) (origin fpi . <fixed-precision-integer>) (class . constant))
   )
   local-literals (
    (top-level . 46)
    (int-binary-lcm . 45)
    (int-binary-gcd . 44)
    (fixed-precision-integer-p . 43)
    ((converter <int>) . 38)
    (<int> . 37)
    (converter . 36)
    (|(method zerop)| . 35)
    (|(method binary-lcm)| . 34)
    (|(method binary-gcd)| . 33)
    (|(method binary-mod)| . 32)
    (|(method binary%)| . 31)
    (|(method binary/)| . 30)
    (|(method binary*)| . 29)
    (|(method binary-)| . 28)
    (|(method binary<)| . 27)
    (|(method binary=)| . 26)
    (|(method binary+)| . 25)
    (direct-keywords: . 24)
    (direct-slots: . 23)
    (direct-superclasses: . 22)
    (int . 21)
    (name: . 20)
   )
   literals (
   )
))
