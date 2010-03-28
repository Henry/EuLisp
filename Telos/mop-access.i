;;; EuLisp system 'youtoo'
;;;   Interface file for module mop-access

(definterface mop-access
  (import (boot mop-prim mop-key mop-class mop-inspect mop-gf mop-meth mop-defcl)
   syntax (_boot0 _mop-gf0 _mop-meth0)
   full-import (boot1 boot mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl)
   export (
    ((name . compute-primitive-reader-using-slot) (pos . 4) (origin mop-access . compute-primitive-reader-using-slot))
    ((name . find-slot-names) (pos . 5) (origin mop-access . find-slot-names))
    ((name . ensure-slot-writer) (pos . 6) (origin mop-access . ensure-slot-writer))
    ((name . pprint) (pos . 3) (origin mop-access . pprint))
    ((name . ensure-slot-reader) (pos . 2) (origin mop-access . ensure-slot-reader))
    ((name . compute-primitive-writer-using-slot) (pos . 7) (origin mop-access . compute-primitive-writer-using-slot))
    ((name . compute-primitive-reader-using-class) (pos . 8) (origin mop-access . compute-primitive-reader-using-class))
    ((name . compute-primitive-writer-using-class) (pos . 12) (origin mop-access . compute-primitive-writer-using-class))
    ((name . compute-and-ensure-slot-accessors) (pos . 9) (origin mop-access . compute-and-ensure-slot-accessors))
    ((name . compute-slot-reader) (pos . 11) (origin mop-access . compute-slot-reader))
    ((name . compute-slot-writer) (pos . 10) (origin mop-access . compute-slot-writer))
   )
   local-literals (
    (top-level . 63)
    (pprint . 62)
    (find-slot-names . 61)
    ("
" . 57)
    ("
Instance ~a of class #<~a>" . 56)
    ("
  ~a = ~a" . 54)
    (|(method compute-primitive-writer-using-class)| . 52)
    (|(method compute-primitive-writer-using-slot)| . 51)
    (|(method ensure-slot-writer)| . 50)
    (|(method compute-primitive-reader-using-class)| . 49)
    (|(method compute-primitive-reader-using-slot)| . 48)
    (|(method ensure-slot-reader)| . 47)
    (|(method compute-slot-writer)| . 46)
    (|(method compute-slot-reader)| . 45)
    (|(method compute-and-ensure-slot-accessors)| . 44)
    (compute-primitive-writer-using-class . 43)
    (compute-primitive-writer-using-slot . 42)
    (ensure-slot-writer . 41)
    (compute-primitive-reader-using-class . 40)
    (compute-primitive-reader-using-slot . 39)
    (ensure-slot-reader . 38)
    (compute-slot-writer . 37)
    (compute-slot-reader . 36)
    (compute-and-ensure-slot-accessors . 35)
    (anonymous . 14)
   )
   literals (
   )
))
