;;; EuLisp system 'youtoo'
;;;   Interface file for module cg-stack

(definterface cg-stack
  (import (i-all sx-obj cg-state)
   syntax (_macros)
   full-import (i-error i-notify i-param i-level1 boot1 boot read symbol random handler table table1 convert1 format list socket stream2 lock stream1 stream vector stream3 float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl telos level1 aux-table i-all sx-obj2 sx-obj1 sx-obj cg-state)
   export (
    ((name . pop-display) (pos . 5) (origin cg-stack . pop-display))
    ((name . push-display) (pos . 8) (origin cg-stack . push-display))
    ((name . push-stack-var) (pos . 3) (origin cg-stack . push-stack-var))
    ((name . stack-var-index) (pos . 2) (origin cg-stack . stack-var-index))
    ((name . display-var-index) (pos . 7) (origin cg-stack . display-var-index))
    ((name . move-stack) (pos . 6) (origin cg-stack . move-stack))
   )
   local-literals (
    (stack-var-index . 29)
    (push-stack-var . 28)
    (update-stack-vars . 27)
    (pop-display . 26)
    (move-stack . 25)
    (display-var-index . 24)
    (push-display . 23)
    ("no display-index for ~a" . 19)
    ("stack underflow" . 17)
    (anonymous . 11)
    ("parameter not on stack" . 9)
   )
   literals (
   )
))
