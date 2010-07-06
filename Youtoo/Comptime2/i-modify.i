;;; EuLisp system 'youtoo'
;;;   Interface file for module i-modify

(definterface i-modify
  (import (i-all)
   syntax (_macros _i-aux0)
   full-import (i-error i-notify i-param i-level1 boot1 boot symbol stream3 random handler read table table1 vector convert1 format list socket stream2 lock stream1 stream double double1 float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl telos level1 aux-table i-all)
   export (
    ((name . external-file-exist?) (pos . 5) (origin i-modify . external-file-exist?))
    ((name . absolute-file-name) (pos . 3) (origin i-modify . absolute-file-name))
    ((name . module-modified?) (pos . 4) (origin i-modify . module-modified?))
    ((name . library-newer?) (pos . 2) (origin i-modify . library-newer?))
    ((name . file-exist?) (pos . 7) (origin i-modify . file-exist?))
    ((name . C-module-modified?) (pos . 6) (origin i-modify . C-module-modified?))
    ((name . file-newer?) (pos . 8) (origin i-modify . file-newer?))
   )
   local-literals (
    (library-newer? . 31)
    (absolute-file-name . 30)
    (module-modified? . 29)
    (external-file-exist? . 28)
    (C-module-modified? . 27)
    (file-exist? . 26)
    (file-newer? . 25)
    ("no such file ~a" . 23)
    (".o" . 20)
    (".o" . 19)
    (".c" . 18)
    (".i" . 15)
    (".em" . 14)
    ("file ~a does not exist" . 12)
    ("" . 11)
   )
   literals (
   )
))
