;;; EuLisp system 'youtoo'
;;;   Interface file for module integer

(definterface integer
  (import (telos number)
   syntax (_telos0)
   full-import (mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos copy compare callback let-cc dynamic thread event condition number)
   export (
    ((name . oddp) (pos . 5) (origin integer . oddp))
    ((name . <integer>) (pos . 2) (origin integer . <integer>) (class . constant))
    ((name . evenp) (pos . 4) (origin integer . evenp))
    ((name . integerp) (pos . 3) (origin integer . integerp))
   )
   local-literals (
    (top-level . 21)
    (evenp . 20)
    (oddp . 19)
    (|(method integerp)| . 15)
    (integerp . 14)
    (abstractp: . 13)
    (direct-keywords: . 12)
    (direct-slots: . 11)
    (direct-superclasses: . 10)
    (integer . 9)
    (name: . 8)
   )
   literals (
   )
))
