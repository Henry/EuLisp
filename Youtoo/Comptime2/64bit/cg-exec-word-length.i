;;; EuLisp system 'youtoo'
;;;   Interface file for module cg-exec-word-length

(definterface cg-exec-word-length
  (import (i-all i-modify p-env sx-obj sx-node cg-state cg-asm cg-interf i-ffi ex-expr cg-dld)
   syntax (_macros _i-aux0)
   full-import (i-error i-notify i-param i-level1 boot1 symbol stream3 random handler read table table1 vector convert1 format list socket stream2 lock stream1 stream float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot telos level1 aux-table i-all i-modify sx-obj sx-obj1 sx-obj2 p-env i-ffi sx-node cg-state op-peep-r op-peep cg-bycode2 cg-bycode1 cg-bycode cg-asm cg-interf cg-dld ex-direct ex-expose ex-syntax ex-import ex-expr)
   export (
    ((name . open-bytevector) (pos . 5) (origin cg-exec-word-length . open-bytevector))
    ((name . write-next-bv-byte) (pos . 6) (origin cg-exec-word-length . write-next-bv-byte))
    ((name . bytevector) (pos . 3) (origin cg-exec-word-length . bytevector))
    ((name . write-next-bv-binding-ref) (pos . 2) (origin cg-exec-word-length . write-next-bv-binding-ref))
   )
   local-literals (
    (top-level . 22)
    (write-next-bv-binding-ref . 21)
    (bytevector . 20)
    (open-bytevector . 19)
    (write-next-bv-byte . 18)
    (fill-value: . 15)
    (size: . 14)
    ("  open-bytevector ~a" . 13)
    ("  write-next-bv-binding-ref ~a ~a ~a" . 10)
   )
   literals (
   )
))
