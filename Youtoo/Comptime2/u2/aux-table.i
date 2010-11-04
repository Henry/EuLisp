;;; EuLisp system 'youtoo'
;;;   Interface file for module aux-table

(definterface aux-table
  (import (level-1)
   syntax (_macros)
   full-import (symbol stream3 random handler read table table1 vector convert1 format list socket stream2 lock stream1 stream float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos level-1)
   export (
    ((name . access-table-keys) (pos . 3) (origin aux-table . access-table-keys))
    ((name . access-table-values) (pos . 8) (origin aux-table . access-table-values))
    ((name . access-table-clear) (pos . 9) (origin aux-table . access-table-clear))
    ((name . make-access-table) (pos . 4) (origin aux-table . make-access-table))
    ((name . access-table-do) (pos . 7) (origin aux-table . access-table-do))
    ((name . access-table-print) (pos . 2) (origin aux-table . access-table-print))
    ((name . access-table-size) (pos . 5) (origin aux-table . access-table-size))
    ((name . access-table-map) (pos . 6) (origin aux-table . access-table-map))
    ((name . c-string-as-eul-symbol) (pos . 10) (origin aux-table . c-string-as-eul-symbol) (class . ff) (arity . 1) (ff (8) 6 ("eul_c_str_as_eul_symbol" . "ff_stub_eul_c_str_as_eul_symbol27")))
   )
   local-literals (
    (access-table-print . 30)
    (access-table-keys . 29)
    (make-access-table . 28)
    (access-table-size . 27)
    (access-table-map . 26)
    (access-table-do . 25)
    (access-table-values . 24)
    (access-table-clear . 23)
    (anonymous . 12)
   )
   literals (
   )
))
