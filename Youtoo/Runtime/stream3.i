;;; EuLisp system 'youtoo'
;;;   Interface file for module stream3

(definterface stream3
  (import (telos integer collect list character string vector float stream1 stream)
   syntax (_telos0)
   full-import (mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos number condition event thread dynamic let-cc callback compare copy integer collect fpi convert list string character vector float stream1 socket stream2 lock stream)
   export (
   )
   local-literals (
    (top-level . 50)
    (|(method generic-prin)| . 48)
    (|(method generic-write)| . 47)
    (": " . 43)
    ("#<" . 42)
    ("()" . 40)
    ("%i" . 36)
    ("%f" . 34)
    ("#<~a: ~a>" . 32)
    ("#(" . 30)
    ("#(" . 29)
    ("#()" . 28)
    (" . " . 25)
    ("#<~a: ~a>" . 21)
    ("*unconnected*" . 20)
    ("*unconnected*" . 19)
    (r . 18)
    ("#<~a: ~a:~a>" . 16)
    ("#<~a: ~a:~a>" . 14)
    ("#(" . 12)
    ("#(" . 11)
    ("#()" . 10)
    (" . " . 7)
    (anonymous . 2)
   )
   literals (
   )
))
