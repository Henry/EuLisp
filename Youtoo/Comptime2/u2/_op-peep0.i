;;; EuLisp system 'youtoo'
;;;   Interface file for module _op-peep0

(definterface _op-peep0
  (import (level-1)
   syntax (syntax-1)
   full-import (symbol stream3 random handler read table table1 vector convert1 format list socket stream2 lock stream1 stream float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit telos level-1)
   export (
    ((name . guarded-rule) (pos . 2) (origin _op-peep0 . guarded-rule))
    ((name . simple-rule) (pos . 3) (origin _op-peep0 . simple-rule))
   )
   local-literals (
    (simple-rule . 17)
    (guarded-rule . 15)
    (add-rule . 13)
    (lambda . 12)
    (anonymous . 10)
    (*no-variable* . 8)
    (* . 7)
    (list . 5)
    (quote . 4)
   )
   literals (
   )
))
