;;; EuLisp system 'youtoo'
;;;   Interface file for module i-modify

(definterface i-modify
  (import (i-all)
   syntax (_macros _i-aux0)
   full-import (i-error i-notify i-param i-level1 boot1 boot symbol stream3 random handler read table table1 vector convert1 format list socket stream2 lock stream1 stream float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl telos level1 aux-table i-all)
   export (
    ((name . external-file-exist-p) (pos . 7) (origin i-modify . external-file-exist-p))
    ((name . library-newer-p) (pos . 3) (origin i-modify . library-newer-p))
    ((name . absolute-file-name) (pos . 5) (origin i-modify . absolute-file-name))
    ((name . file-newer-p) (pos . 2) (origin i-modify . file-newer-p))
    ((name . module-modified-p) (pos . 8) (origin i-modify . module-modified-p))
    ((name . C-module-modified-p) (pos . 6) (origin i-modify . C-module-modified-p))
    ((name . file-exist-p) (pos . 4) (origin i-modify . file-exist-p))
   )
   local-literals (
    (file-newer-p . 31)
    (library-newer-p . 30)
    (file-exist-p . 29)
    (absolute-file-name . 28)
    (C-module-modified-p . 27)
    (external-file-exist-p . 26)
    (module-modified-p . 25)
    (".i" . 23)
    (".em" . 22)
    (".o" . 19)
    (".o" . 18)
    (".c" . 17)
    ("file ~a does not exist" . 15)
    ("" . 14)
    ("no such file ~a" . 10)
   )
   literals (
   )
))
