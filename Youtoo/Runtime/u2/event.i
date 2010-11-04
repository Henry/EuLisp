;;; EuLisp system 'youtoo'
;;;   Interface file for module event

(definterface event
  (import (telos)
   syntax (_telos0)
   full-import ()
   export (
    ((name . ticks-per-second) (pos . 3) (origin event . ticks-per-second) (class . constant))
    ((name . wait) (pos . 2) (origin event . wait))
   )
   local-literals (
    (top-level . 10)
    (|(method wait)| . 8)
    (wait . 7)
    ("wait not yet implemented" . 5)
   )
   literals (
   )
))
