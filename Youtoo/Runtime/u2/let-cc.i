;;; EuLisp system 'youtoo'
;;;   Interface file for module let-cc

(definterface let-cc
  (import (telos thread dynamic)
   syntax (_syntax-1)
   full-import (mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos event thread dynamic)
   export (
    ((name . call/ep) (pos . 2) (origin let-cc . call/ep))
   )
   local-literals (
    (call/ep . 9)
    (k . 7)
    (*clean-ups* . 5)
    (anonymous . 4)
   )
   literals (
   )
))
