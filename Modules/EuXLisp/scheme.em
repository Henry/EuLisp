;;; scheme-like module

;;; call/cc may be weird
;;; syntax of various objects may be different (e.g., symbols)

;;; interactive scheme module

(defmodule scheme
    (import (schemer
              (rename
                ((error ERROR))
                (only
                  (!>
                    exit
                    expose
                    export
                    error
                    defcondition
                    <error>)
                  level0)))
     export (!> exit error))

  (expose schemer)

  (defcondition <scheme-error> <error>)

  (define (error a b)
          (ERROR
            a
            <scheme-error>
            value: b))

  )
