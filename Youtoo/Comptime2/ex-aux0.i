;;; EuLisp system 'youtoo'
;;;   Interface file for module ex-aux0

(definterface ex-aux0
  (import (level-1)
   syntax (macros)
   full-import (symbol stream3 random handler read table table1 vector convert1 format list socket stream2 lock stream1 stream float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos level-1)
   export (
    ((name . get-body) (pos . 6) (origin ex-aux0 . get-body))
    ((name . get-lambda-params) (pos . 7) (origin ex-aux0 . get-lambda-params))
    ((name . get-top-level-forms) (pos . 8) (origin ex-aux0 . get-top-level-forms))
    ((name . get-lambda-body) (pos . 5) (origin ex-aux0 . get-lambda-body))
    ((name . get-directives) (pos . 4) (origin ex-aux0 . get-directives))
    ((name . get-name) (pos . 3) (origin ex-aux0 . get-name))
    ((name . get-params) (pos . 2) (origin ex-aux0 . get-params))
    ((name . get-value) (pos . 9) (origin ex-aux0 . get-value))
   )
   local-literals (
    (get-params . 47)
    (get-name . 46)
    (get-directives . 45)
    (get-lambda-body . 44)
    (get-body . 43)
    (get-lambda-params . 42)
    (get-top-level-forms . 41)
    (get-value . 40)
    ("body ~a not a list" . 35)
    (cdr . 34)
    ("body ~a not a list" . 32)
    (or . 31)
    (null? . 30)
    (cddr . 29)
    (let . 26)
    (if . 25)
    (error . 24)
    (<condition> . 23)
    (fmt . 22)
    ("bad value ~a" . 21)
    (and . 20)
    (eq . 19)
    (quote . 18)
    (setter . 17)
    (car . 16)
    (cons? . 15)
    (symbol? . 14)
    (x . 13)
    (cadr . 12)
    (caddr . 10)
   )
   literals (
   )
))
