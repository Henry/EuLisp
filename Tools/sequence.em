(defmodule sequence
  (syntax (macros)
   import (level1)
   export (sub-sequence))

  (defgeneric sub-sequence ((seq <sequence>) (start <integer>) (last <integer>)))

  (defmethod sub-sequence ((string <string>) (start <integer>) (last <integer>))
    (substring string start last))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
