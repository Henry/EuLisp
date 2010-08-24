;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp system 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library:
;;;  Authors: Andreas Kind
;;; Description:
;;;  Compilation
;;;-----------------------------------------------------------------------------
(defmodule test1
  (syntax (scheme0)
   import (scheme)
   export ())

(define (tak x y z)
        (if (not (< y x))
            z
          (tak (tak (- x 1) y z)
               (tak (- y 1) z x)
               (tak (- z 1) x y))))

(display (tak 18 12 6))

(newline)

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
