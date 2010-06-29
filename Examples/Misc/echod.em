;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: misc
;;;  Authors: Andreas Kind
;;; Description: echod deamon
;;;  Compilation
;;    youtoo echod -l level1
;;;-----------------------------------------------------------------------------
(defmodule echod
  (syntax (macros)
   import (level1))

  (let* ((port (if (< *argc* 2) 4711 (vector-ref *argv* 1)))
         (s (make <socket> port: port))
         c x)
    (unwind-protect
        (while (setq c (make <connection> socket: s))
          (while (null? (eq (setq x (read-line c () (eos-default-value)))
                           (eos-default-value)))
            (format "echoing ~a" x)
            (sprin c x))
          (disconnect c))
      (disconnect s)))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
