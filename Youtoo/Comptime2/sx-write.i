;;; EuLisp system 'youtoo'
;;;   Interface file for module sx-write

(definterface sx-write
  (import (i-all sx-obj)
   syntax (_macros)
   full-import (i-error i-notify i-param i-level1 boot1 boot symbol stream3 random handler read table table1 vector convert1 format list socket stream2 lock stream1 stream float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl telos level1 aux-table i-all sx-obj2 sx-obj1 sx-obj)
   export (
    ((name . pprint-module) (pos . 4) (origin sx-write . pprint-module))
   )
   local-literals (
    (top-level . 69)
    (binding-prin-string . 68)
    (pprint-module . 67)
    ("
  syntax-env:" . 65)
    ("
  external-env:" . 64)
    ("
  lexical-env:" . 63)
    ("
Pretty printed environment of module ~a:" . 62)
    (*clean-ups* . 61)
    ("
   (~a . ~a)" . 58)
    ("
   (~a . ~a)" . 56)
    ("
   (~a . ~a)" . 54)
    ("~a" . 52)
    ("#<macro-function>" . 51)
    (|(method new-generic-print)| . 49)
    (|(method generic-print)| . 48)
    (new-generic-print . 47)
    ("#<module: ~a>" . 45)
    ("#<binding: ~a:~a>" . 43)
    ("#<binding: ~a:~a:~a>" . 42)
    ("#<macro-function>" . 39)
    ("<unprintable syntax object>" . 37)
    ("~a" . 36)
    (binding . 35)
    ("~a" . 33)
    ("~a" . 31)
    ("~a" . 29)
    ("(setq ~a ~a)" . 27)
    ("~a" . 25)
    ("~a" . 23)
    ("(~a ~a ~a)" . 21)
    ("lambda" . 20)
    ("inlined-lambda" . 19)
    ("~a" . 18)
    (*pprint* . 17)
    ("(opencoded-lambda ~a ~a)" . 15)
    (") ~a)" . 13)
    (anonymous . 12)
    ("(let* (" . 11)
    ("(~a ~a)" . 9)
    ("~a" . 7)
    ("(if ~a ~a ~a)" . 5)
   )
   literals (
   )
))
