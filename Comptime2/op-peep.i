;;; EuLisp system 'youtoo'
;;;   Interface file for module op-peep

(definterface op-peep
  (import (i-all)
   syntax (_macros _op-peep0)
   full-import (i-error i-notify i-param i-level1 boot1 boot read symbol random handler table table1 convert1 format list socket stream2 lock stream1 stream vector stream3 float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl telos level1 aux-table i-all)
   export (
    ((name . add-rule) (pos . 5) (origin op-peep . add-rule))
    ((name . peep-hole-optimize) (pos . 3) (origin op-peep . peep-hole-optimize))
   )
   local-literals (
    (top-level . 25)
    (apply-rule . 24)
    (peep-hole-optimize . 23)
    (match-pattern . 22)
    (add-rule . 21)
    (match-rule . 20)
    (anonymous . 11)
    ("[~a => ~a]" . 9)
   )
   literals (
   )
))
