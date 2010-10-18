;;; EuLisp system 'youtoo'
;;;   Interface file for module convert

(definterface convert
  (import (telos condition)
   syntax (_telos0)
   full-import (mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos let-cc dynamic thread event condition)
   export (
    ((name . converter) (pos . 14) (origin mop-class . converter))
    ((name . convert) (pos . 2) (origin convert . convert))
   )
   local-literals (
    (convert . 4)
   )
   literals (
   )
))
