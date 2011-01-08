;;; EuLisp system 'youtoo'
;;;   Library interface file for module eval

(definterface eval
  (import ()
   syntax ()
   full-import (eval i-compile i-modify p-read ex-body ex-module cg-interf ex-import ex-syntax ex-expose ex-direct sx-write cg-dld ex-expr p-parse an-side cg-stack cg-state cg-gen op-peep-r op-peep cg-bycode2 cg-bycode1 cg-bycode cg-asm cg-link cg-exec-word-length cg-exec sx-node i-ffi p-env sx-obj sx-obj1 sx-obj2 i-args i-all aux-table level-1 telos mop-defcl mop-meth mop-gf mop-inspect mop-init mop-class mop-key mop-prim mop-access mop-alloc bit condition event thread dynamic let-cc callback string convert copy integer number fpi collect compare character float stream stream1 lock stream2 socket list format convert1 vector table1 table read handler random stream3 symbol boot boot1 i-level-1 i-param i-notify i-error i-rep)
   export (
    ((name . set-eval-module) (pos . 4) (origin i-rep . set-eval-module))
    ((name . expand-syntax-1) (pos . 23) (origin ex-expr . expand-syntax-1))
    ((name . expand-syntax) (pos . 24) (origin ex-expr . expand-syntax))
    ((name . get-module) (pos . 14) (origin p-env . get-module))
    ((name . eval/cm) (pos . 3) (origin i-rep . eval/cm))
   )
   literals (
   )
  )
)  ; end of interface