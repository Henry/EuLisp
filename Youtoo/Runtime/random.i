;;; EuLisp system 'youtoo'
;;;   Interface file for module random

(definterface random
  (import (telos)
   syntax (_telos0)
   full-import (mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos)
   export (
    ((name . random) (pos . 4) (origin random . random))
    ((name . random-true-nil) (pos . 2) (origin random . random-true-nil))
    ((name . *random-max*) (pos . 3) (origin random . *random-max*) (class . constant))
    ((name . random-seed) (pos . 5) (origin random . random-seed))
   )
   local-literals (
    (top-level . 16)
    (random-true-nil . 15)
    (random . 14)
    (random-seed . 13)
   )
   literals (
   )
))
