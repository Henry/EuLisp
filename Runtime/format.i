;;; EuLisp system 'youtoo'
;;;   Interface file for module format

(definterface format
  (import (telos collect fpi list string stream)
   syntax (_telos0)
   full-import (mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos compare condition event thread dynamic let-cc callback collect integer number copy fpi convert list string socket stream2 lock stream1 stream)
   export (
    ((name . format) (pos . 17) (origin mop-gf . generic-format))
   )
   local-literals (
    (top-level . 20)
    (|(method format)| . 18)
    ("" . 16)
    (anonymous . 11)
    ("%o" . 9)
    ("bad format string ~s" . 8)
    ("%x" . 7)
    ("bad format string ~s" . 6)
    ("bad format string ~s" . 5)
    ("bad format string ~s" . 4)
    ("%s" . 3)
   )
   literals (
   )
))
