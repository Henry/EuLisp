;;; EuLisp system 'youtoo'
;;;   Interface file for module i-error

(definterface i-error
  (import (i-level-1 i-param i-notify)
   syntax (_syntax-1 _i-aux0)
   full-import (aux-table level-1 telos mop-defcl mop-meth mop-gf mop-inspect mop-init mop-class mop-key mop-prim mop-access mop-alloc bit condition event thread dynamic let-cc callback string convert copy integer number fpi collect compare character float stream stream1 lock stream2 socket list format convert1 vector table1 table read handler random stream3 symbol boot boot1 i-level-1 i-param i-notify)
   export (
    ((name . <ct-error>) (pos . 5) (origin i-error . <ct-error>) (class . constant))
    ((name . ct-exit) (pos . 2) (origin i-error . ct-exit))
    ((name . ct-error) (pos . 4) (origin i-error . ct-error))
   )
   local-literals (
    (top-level . 29)
    (ct-exit . 28)
    (ct-error-value . 27)
    ("*** TOTAL NUMBER OF ERRORS: ~a
" . 23)
    ("*** TOTAL NUMBER OF WARNINGS: ~a
" . 22)
    (|(method generic-print)| . 20)
    (|(method get-ct-error-condition-class)| . 19)
    (|(setter ct-error-value)| . 18)
    ((ct-error-value:) . 17)
    (direct-keywords: . 16)
    (direct-slots: . 15)
    (direct-superclasses: . 14)
    (ct-error . 13)
    (ct-error-value: . 12)
    (keyword: . 11)
    (value . 10)
    (name: . 9)
   )
   literals (
   )
))
