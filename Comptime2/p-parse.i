;;; EuLisp system 'youtoo'
;;;   Interface file for module p-parse

(definterface p-parse
  (import (i-all p-env ex-module ex-body sx-write sx-node)
   syntax (_macros)
   full-import (i-error i-notify i-param i-level1 boot1 boot read symbol random handler table table1 convert1 format list socket stream2 lock stream1 stream vector stream3 float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl telos level1 aux-table i-all sx-obj sx-obj1 sx-obj2 p-env ex-expr cg-dld ex-direct ex-expose ex-syntax ex-import i-modify cg-interf sx-node i-ffi ex-module ex-body sx-write)
   export (
    ((name . interactive-parse) (pos . 2) (origin p-parse . interactive-parse))
   )
   local-literals (
    (top-level . 21)
    (interactive-parse . 20)
    (" .XE/SE" . 18)
    (" .ME/SE" . 17)
    (|(method parse-module)| . 15)
    ("bad defmodule syntax in module ~a ..." . 13)
    (" .XE/SE" . 12)
    (" .EE" . 11)
    (anonymous . 10)
    (*clean-ups* . 9)
    (*actual-module* . 8)
    (defmodule . 7)
    (" .ME/SE" . 6)
    (parse . 5)
    ("  Parsing module ~a ..." . 4)
   )
   literals (
   )
))
