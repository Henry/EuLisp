;;; EuLisp system 'youtoo'
;;;   Interface file for module convert1

(definterface convert1
  (import (telos condition convert collect fpi character list string vector table)
   syntax (_telos0)
   full-import (mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos let-cc dynamic thread event condition convert compare callback collect integer number copy fpi string character list vector table1 table)
   export (
   )
   local-literals (
    (top-level . 40)
    (|(method (converter <character>))| . 38)
    (|(method (converter <int>))| . 37)
    (|(method (converter <table>))| . 36)
    (|(method (converter <vector>))| . 35)
    (|(method (converter <string>))| . 34)
    (|(method (converter <list>))| . 33)
    (size: . 23)
    ("conversion to string with non character element " . 21)
    (anonymous . 11)
   )
   literals (
   )
))
