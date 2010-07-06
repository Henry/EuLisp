;;; EuLisp system 'youtoo'
;;;   Interface file for module an-side

(definterface an-side
  (import (i-all sx-obj)
   syntax (_macros)
   full-import (i-error i-notify i-param i-level1 boot1 boot symbol stream3 random handler read table table1 vector convert1 format list socket stream2 lock stream1 stream double double1 float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl telos level1 aux-table i-all sx-obj2 sx-obj1 sx-obj)
   export (
    ((name . compute-captured-vars) (pos . 2) (origin an-side . compute-captured-vars))
   )
   local-literals (
    (top-level . 11)
    (|(method compute-captured-vars)| . 9)
    (compute-captured-vars . 8)
    (anonymous . 5)
    ("var ~a captured" . 3)
   )
   literals (
   )
))
