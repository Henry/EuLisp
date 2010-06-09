;;; EuLisp system 'youtoo'
;;;   Interface file for module mop-alloc

(definterface mop-alloc
  (import (boot mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl mop-access)
   syntax (_boot0 _mop-gf0 _mop-meth0)
   full-import (boot1 boot mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl mop-access)
   export (
    ((name . compute-slots) (pos . 9) (origin mop-alloc . compute-slots))
    ((name . compatible-superclasses?) (pos . 14) (origin mop-alloc . compatible-superclasses?))
    ((name . compute-specialized-slot-class) (pos . 2) (origin mop-alloc . compute-specialized-slot-class))
    ((name . compute-keywords) (pos . 6) (origin mop-alloc . compute-keywords))
    ((name . compatible-superclass?) (pos . 8) (origin mop-alloc . compatible-superclass?))
    ((name . compute-defined-slot) (pos . 7) (origin mop-alloc . compute-defined-slot))
    ((name . compute-inherited-keywords) (pos . 10) (origin mop-alloc . compute-inherited-keywords))
    ((name . compute-class-precedence-list) (pos . 13) (origin mop-alloc . compute-class-precedence-list))
    ((name . compute-defined-slot-class) (pos . 12) (origin mop-alloc . compute-defined-slot-class))
    ((name . compute-inherited-slots) (pos . 3) (origin mop-alloc . compute-inherited-slots))
    ((name . compute-specialized-slot) (pos . 15) (origin mop-alloc . compute-specialized-slot))
   )
   local-literals (
    (top-level . 82)
    (check-keywords . 81)
    (inherited-slot . 80)
    (redefined-slot . 79)
    ((reader: writer: keyword: default:) . 77)
    (default: . 75)
    (writer: . 74)
    (reader: . 73)
    ("unexpected keyword ~a in initialization of ~a" . 70)
    (|(method compute-defined-slot-class)| . 68)
    (|(method compute-defined-slot)| . 67)
    (|(method compute-specialized-slot-class)| . 66)
    (|(method compute-specialized-slot)| . 65)
    (|(method compute-slots)| . 64)
    (|(method compute-inherited-slots)| . 63)
    (|(method compute-keywords)| . 62)
    (|(method compute-inherited-keywords)| . 61)
    (|(method compute-class-precedence-list)| . 60)
    (|(method compatible-superclass?)| . 59)
    (|(method compatible-superclasses?)| . 58)
    (|(method initialize)| . 57)
    (|(method allocate)| . 56)
    (compute-defined-slot-class . 55)
    (compute-defined-slot . 54)
    (compute-specialized-slot-class . 53)
    (compute-specialized-slot . 52)
    (compute-slots . 51)
    (compute-inherited-slots . 50)
    (compute-keywords . 49)
    (compute-inherited-keywords . 48)
    (compute-class-precedence-list . 47)
    (compatible-superclass? . 46)
    (compatible-superclasses? . 45)
    ("can't allocate an instance of abstract-class ~a" . 43)
    ("missing keyword ~a to make ~a" . 40)
    ("~a can not be a subclass of ~a" . 38)
    (direct-keywords: . 37)
    (direct-slots: . 36)
    (anonymous . 27)
    ((keyword:) . 20)
    (keyword: . 19)
    (required: . 18)
    (name: . 17)
   )
   literals (
   )
))
