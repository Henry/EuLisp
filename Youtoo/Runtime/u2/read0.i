;;; EuLisp system 'youtoo'
;;;   Interface file for module read0

(definterface read0
  (import (level-1)
   syntax (boot0)
   full-import (symbol stream3 random handler read table table1 vector convert1 format list socket stream2 lock stream1 stream float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos level-1 level-1)
   export (
    ((name . match-let) (pos . 2) (origin read0 . match-let))
   )
   local-literals (
    (match-let . 14)
    (anonymous . 12)
    (let . 11)
    (if . 9)
    (progn . 8)
    (cdr . 7)
    (car . 6)
    (setq . 5)
    (null? . 4)
   )
   literals (
   )
))
