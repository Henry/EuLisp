;;; EuLisp system 'youtoo'
;;;   Interface file for module copy

(definterface copy
  (import (telos thread condition)
   syntax (_telos0)
   full-import (mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos event thread let-cc dynamic condition)
   export (
    ((name . deep-copy) (pos . 3) (origin copy . deep-copy))
    ((name . shallow-copy) (pos . 2) (origin copy . shallow-copy))
   )
   local-literals (
    (top-level . 22)
    (|(method deep-copy)| . 20)
    (|(method shallow-copy)| . 19)
    (deep-copy . 18)
    (shallow-copy . 17)
    (anonymous . 5)
   )
   literals (
   )
))
