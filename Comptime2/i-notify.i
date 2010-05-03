;;; EuLisp system 'youtoo'
;;;   Interface file for module i-notify

(definterface i-notify
  (import (i-level1 i-param)
   syntax (_macros)
   full-import (aux-table level1 telos mop-defcl mop-meth mop-gf mop-inspect mop-init mop-class mop-key mop-prim mop-access mop-alloc bit condition event thread dynamic let-cc callback string convert copy integer number fpi collect compare character float stream stream1 lock stream2 socket list format convert1 vector table1 table read handler random stream3 symbol boot boot1 i-level1 i-param)
   export (
    ((name . ct-warning) (pos . 5) (origin i-notify . ct-warning))
    ((name . notify) (pos . 3) (origin i-notify . notify))
    ((name . notify0) (pos . 4) (origin i-notify . notify0))
    ((name . ct-serious-warning) (pos . 6) (origin i-notify . ct-serious-warning))
   )
   local-literals (
    (basic-warning . 33)
    (notify . 32)
    (notify0 . 31)
    (ct-warning . 30)
    (ct-serious-warning . 29)
    (basic-notify . 28)
    (ct-error-value: . 25)
    ("ERROR" . 24)
    ("WARNING" . 22)
    (*indent* . 19)
    ("*** ~a ~a: ~a" . 17)
    ("~a[~a]" . 16)
    (*encl-lambda* . 15)
    ("" . 14)
    ("[~a]" . 13)
    (*actual-module* . 12)
    (anonymous . 11)
    (*clean-ups* . 10)
    (*pprint* . 9)
   )
   literals (
   )
))
