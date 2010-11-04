;;; EuLisp system 'youtoo'
;;;   Interface file for module mop-key

(definterface mop-key
  (import (boot)
   syntax (_boot0)
   full-import ()
   export (
    ((name . find-key) (pos . 2) (origin mop-key . find-key))
    ((name . filter-keywords) (pos . 3) (origin mop-key . filter-keywords))
   )
   local-literals (
    (find-key . 11)
    (filter-keywords . 10)
    (anonymous . 8)
    ("missing required keyword ~a" . 5)
    (required: . 4)
   )
   literals (
   )
))
