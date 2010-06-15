;;; EuLisp system 'youtoo'
;;;   Interface file for module format

(definterface format
  (import (telos collect fpi list string character stream)
   syntax (_telos0)
   full-import (mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos compare condition event thread dynamic let-cc callback collect integer number copy fpi convert list string character socket stream2 lock stream1 stream)
   export (
    ((name . fmt) (pos . 2) (origin format . fmt))
    ((name . format) (pos . 17) (origin mop-gf . generic-format))
   )
   local-literals (
    (top-level . 21)
    (fmt . 20)
    (|(method format)| . 17)
    ("" . 15)
    (anonymous . 10)
    ("%" . 8)
    ("bad format string ~s" . 7)
    ("bad format string ~s" . 6)
    ("bad format string ~s" . 5)
    ("%s" . 4)
   )
   literals (
   )
))
