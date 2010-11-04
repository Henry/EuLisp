;;; EuLisp system 'youtoo'
;;;   Interface file for module convert

(definterface convert
  (import (telos condition)
   syntax (_telos0)
   full-import ()
   export (
    ((name . converter) (pos . 13) (origin mop-class . converter))
    ((name . convert) (pos . 2) (origin convert . convert))
   )
   local-literals (
    (convert . 4)
   )
   literals (
   )
))
