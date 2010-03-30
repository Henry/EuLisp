;;; EuLisp system 'youtoo'
;;;   Interface file for module compare

(definterface compare
  (import (telos callback)
   syntax (_telos0)
   full-import (mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos condition event thread dynamic let-cc callback)
   export (
    ((name . >=) (pos . 11) (origin compare . >=))
    ((name . <=) (pos . 8) (origin compare . <=))
    ((name . >) (pos . 4) (origin compare . >))
    ((name . equal) (pos . 7) (origin compare . equal))
    ((name . min) (pos . 10) (origin compare . min))
    ((name . max) (pos . 5) (origin compare . max))
    ((name . =) (pos . 2) (origin compare . =))
    ((name . eq) (pos . 24) (origin boot1 . eq) (inline (G0035 (eq))))
    ((name . eql) (pos . 44) (origin boot1 . eql) (inline (G0037 (eq))))
    ((name . <) (pos . 3) (origin compare . <))
    ((name . binary=) (pos . 9) (origin compare . binary=))
    ((name . binary<) (pos . 6) (origin compare . binary<))
   )
   local-literals (
    (top-level . 38)
    (= . 37)
    (< . 36)
    (> . 35)
    (max . 34)
    (<= . 33)
    (min . 32)
    (>= . 31)
    (anonymous . 19)
    (|(method equal)| . 16)
    (binary< . 15)
    (binary= . 14)
    (equal . 13)
   )
   literals (
   )
))