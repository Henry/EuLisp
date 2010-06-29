(defmodule test-match-macros
  (syntax (macros)
   import (level1))

  (defmacro print-test (body)
    `(format "~s~%    ;;=> ~s~%" ',body ,body))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
