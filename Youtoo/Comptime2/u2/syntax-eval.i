;;; EuLisp system 'youtoo'
;;;   Interface file for module syntax-eval

(definterface syntax-eval
  (import (level-1 sx-obj1)
   syntax (syntax-0)
   full-import (symbol stream3 random handler read table table1 vector convert1 format list socket stream2 lock stream1 stream float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos level-1 i-level-1 aux-table sx-obj1)
   export (
    ((name . eval) (pos . 2) (origin syntax-eval . eval))
   )
   local-literals (
    (eval . 14)
    (let . 12)
    (dynamic-setq . 11)
    (get-module . 10)
    (res . 9)
    (eval/cm . 8)
    (set-eval-module . 7)
    (eval-module-name . 6)
    (or . 5)
    (quasiquote . 4)
    (*actual-module* . 3)
   )
   literals (
   )
))
