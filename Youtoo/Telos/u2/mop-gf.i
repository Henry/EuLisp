;;; EuLisp system 'youtoo'
;;;   Interface file for module mop-gf

(definterface mop-gf
  (import (boot mop-prim mop-class mop-inspect)
   syntax (_boot0 _mop-gf0)
   full-import ()
   export (
    ((name . error-no-applicable-methods) (pos . 15) (origin mop-gf . error-no-applicable-methods))
    ((name . initialize) (pos . 12) (origin mop-gf . initialize))
    ((name . discriminating-domain) (pos . 6) (origin mop-gf . discriminating-domain))
    ((name . sig=) (pos . 10) (origin mop-gf . sig=))
    ((name . allocate) (pos . 8) (origin mop-gf . allocate))
    ((name . gf-reset-cache) (pos . 3) (origin mop-gf . gf-reset-cache))
    ((name . finalize) (pos . 14) (origin mop-gf . finalize))
    ((name . make-generic-function) (pos . 16) (origin mop-gf . make-generic-function))
    ((name . make) (pos . 2) (origin mop-gf . make))
    ((name . the-method-lookup-function) (pos . 7) (origin mop-gf . the-method-lookup-function))
   )
   local-literals (
    (top-level . 72)
    (make . 71)
    (gf-reset-cache . 70)
    (compute-primitive-discriminating-function . 69)
    (primitive-make-generic-function . 68)
    (discriminating-domain . 67)
    (the-method-lookup-function . 66)
    (select-methods . 65)
    (sig= . 64)
    (sig-applicable? . 63)
    (sig<= . 62)
    (error-no-applicable-methods . 61)
    (make-generic-function . 60)
    (method-keywords: . 58)
    (method-class: . 57)
    (domain: . 56)
    (name: . 55)
    ("no applicable methods for ~a
    arguments: ~a
    classes: ~a" . 53)
    (primitive-method-lookup-function . 38)
    (primitive-discriminating-function . 35)
    (anonymous . 32)
    (warning . 28)
    (error . 27)
    (allocate . 26)
    (finalize . 25)
    (initialize . 24)
    ("***    See Backtrace? (y/n) " . 22)
    ("
" . 21)
    ("*** ERROR [level-1]: " . 20)
    ("
" . 18)
    ("*** WARNING [level-1]: " . 17)
   )
   literals (
   )
))
