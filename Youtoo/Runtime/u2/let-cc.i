;;; EuLisp system 'youtoo'
;;;   Interface file for module let-cc

(definterface let-cc
  (import (telos thread dynamic)
   syntax (_macros)
   full-import ()
   export (
    ((name . call/ep) (pos . 2) (origin let-cc . call/ep))
   )
   local-literals (
    (call/ep . 9)
    (k . 7)
    (*clean-ups* . 5)
    (anonymous . 4)
   )
   literals (
   )
))
