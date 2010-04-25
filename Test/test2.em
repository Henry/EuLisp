;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: test
;;;  Authors: Andreas Kind
;;; Description: Test executable with linked boot library
;;;  Compilation
;;    youtoo test2 -l boot
;;;-----------------------------------------------------------------------------
(defmodule test2
  (import (boot))
  (print "Testing module boot ... OK.")

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
