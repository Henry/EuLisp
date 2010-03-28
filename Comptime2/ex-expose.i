;;; EuLisp system 'youtoo'
;;;   Interface file for module ex-expose

(definterface ex-expose
  (import (i-all p-env sx-obj sx-node cg-interf ex-import)
   syntax (_macros _i-aux0)
   full-import (i-error i-notify i-param i-level1 boot1 boot read symbol random handler table table1 convert1 format list socket stream2 lock stream1 stream vector stream3 float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl telos level1 aux-table i-all sx-obj sx-obj1 sx-obj2 p-env i-ffi sx-node i-modify cg-interf ex-import)
   export (
    ((name . expand-expose) (pos . 8) (origin ex-expose . expand-expose))
   )
   local-literals (
    (top-level . 67)
    (expose-expander . 66)
    (expose-binding . 65)
    (expose-module . 64)
    (make-prefix . 63)
    (install-expose-expander . 62)
    (expand-expose . 61)
    ("redefinition of expander ~a" . 58)
    ("  Expose module ~a ..." . 55)
    ("external binding ~a not available in module ~a" . 52)
    ("no expose expander ~a available" . 50)
    ("no expose expander ~a available" . 49)
    (prefix . 45)
    (rename . 44)
    (except . 43)
    (only . 42)
    (|(method G005047)| . 40)
    ("bad expose only syntax" . 38)
    ("compile time error condition: " . 37)
    (|(method G005071)| . 33)
    ("bad expose except syntax" . 31)
    ("compile time error condition: " . 30)
    (|(method G005098)| . 26)
    ("bad expose rename syntax" . 24)
    ("compile time error condition: " . 23)
    (*actual-module* . 18)
    (|(method G005131)| . 17)
    (anonymous . 16)
    (ct-error-value: . 14)
    ("bad expose prefix syntax" . 13)
    ("compile time error condition: " . 12)
   )
   literals (
   )
))
