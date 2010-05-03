;;; EuLisp system 'youtoo'
;;;   Interface file for module cg-bycode0

(definterface cg-bycode0
  (import (level1)
   syntax (macros)
   full-import (symbol stream3 random handler read table table1 vector convert1 format list socket stream2 lock stream1 stream float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos level1)
   export (
    ((name . def-bytecode) (pos . 3) (origin cg-bycode0 . def-bytecode))
    ((name . def-register) (pos . 2) (origin cg-bycode0 . def-register))
   )
   local-literals (
    (def-register . 17)
    (def-bytecode . 16)
    (make . 14)
    (<bytecode> . 13)
    (name: . 12)
    (args: . 11)
    (code: . 10)
    (properties: . 9)
    (get-bytecode . 8)
    (quote . 6)
    (setter . 5)
    (get-register . 4)
   )
   literals (
   )
))
