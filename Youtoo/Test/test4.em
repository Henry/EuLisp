;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: test
;;;  Authors: Andreas Kind
;;; Description: Test executable with linked level1 library
;;;  Compilation
;;    youtoo test4 -l level1
;;;-----------------------------------------------------------------------------
(defmodule test4
  (import (level1))
(print "Testing module level1 ... OK.")

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
