(defmodule nuseq
  (syntax (macros)
   import (level1)
   export (subseq))

  (defgeneric subseq (seq start last))

  (defmethod subseq ((string <string>) (start <integer>) (last <integer>))
    (substring string start last))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
