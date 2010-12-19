;;; EuLisp system 'youtoo'
;;;   Interface file for module ex-direct

(definterface ex-direct
  (import (i-all p-env sx-obj sx-node ex-import ex-syntax ex-expose)
   syntax (_syntax-1 _i-aux0)
   full-import (i-error i-notify i-param i-level-1 boot1 boot symbol stream3 random handler read table table1 vector convert1 format list socket stream2 lock stream1 stream float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl telos level-1 aux-table i-all sx-obj sx-obj1 sx-obj2 p-env i-ffi sx-node cg-interf i-modify ex-import ex-syntax ex-expose)
   export (
    ((name . expand-export) (pos . 4) (origin ex-direct . expand-export))
    ((name . expand-directive) (pos . 6) (origin ex-direct . expand-directive))
   )
   local-literals (
    (top-level . 39)
    (install-directive-expander . 38)
    (expand-export . 37)
    (directive-expander . 36)
    (expand-directive . 35)
    (|(method G005481)| . 33)
    (ct-error-value: . 31)
    ("bad directive syntax" . 30)
    ("compile time error condition: " . 29)
    ("no directive expander ~a available" . 26)
    ("no directive expander ~a available" . 25)
    ("exported lexical binding ~a not available" . 21)
    ("redefinition of expander ~a" . 19)
    (syntax . 17)
    (expose . 16)
    (export . 15)
    (import . 14)
    (anonymous . 11)
    (*actual-module* . 9)
   )
   literals (
   )
))
