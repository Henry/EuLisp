;;; EuLisp system 'youtoo'
;;;   Interface file for module p-read

(definterface p-read
  (import (i-all sx-obj)
   syntax (_macros _i-aux0)
   full-import (i-error i-notify i-param i-level-1 boot1 boot symbol stream3 random handler read table table1 vector convert1 format list socket stream2 lock stream1 stream float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl telos level-1 aux-table i-all sx-obj2 sx-obj1 sx-obj)
   export (
    ((name . read-source-file) (pos . 2) (origin p-read . read-source-file))
   )
   local-literals (
    (read-source-file . 12)
    ("  Reading sources from ~a~a~a.em ..." . 10)
    (anonymous . 9)
    (*clean-ups* . 8)
    (file-name: . 7)
    ("No such file or directory ~a in ~a" . 6)
    (".em" . 5)
    (read . 4)
   )
   literals (
   )
))
