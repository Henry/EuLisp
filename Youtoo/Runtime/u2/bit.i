;;; EuLisp system 'youtoo'
;;;   Interface file for module bit

(definterface bit
  (import (telos)
   syntax ()
   full-import (mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos)
   export (
    ((name . bit-and) (pos . 2) (origin bit . bit-and))
    ((name . bit-xor) (pos . 6) (origin bit . bit-xor))
    ((name . bit-not) (pos . 4) (origin bit . bit-not))
    ((name . bit-ior) (pos . 5) (origin bit . bit-ior))
    ((name . bit-shift) (pos . 3) (origin bit . bit-shift))
   )
   local-literals (
    (bit-and . 21)
    (bit-shift . 20)
    (bit-not . 19)
    (bit-ior . 18)
    (bit-xor . 17)
   )
   literals (
   )
))
