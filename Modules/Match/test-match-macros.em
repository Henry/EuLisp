(defmodule test-match-macros
  (syntax (syntax-0)
   import (level-0))

(defmacro print-test (body)
  `(format "~s~%    ;;=> ~s~%" ',body ,body))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
