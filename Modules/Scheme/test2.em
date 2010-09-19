;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp system 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library:
;;;  Authors: Andreas Kind
;;; Description:
;;;  Compilation
;;;-----------------------------------------------------------------------------
(defmodule test2
  (syntax (scheme0)
   import (scheme)
   export ())

(define (fact x)
        (if (< x 2)
            1
          (* x (fact (- x 1)))))

(display (fact 1000))

(newline)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
