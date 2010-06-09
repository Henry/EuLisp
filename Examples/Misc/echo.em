;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: misc
;;;  Authors: Andreas Kind
;;; Description: checking out the echo daemon (echod)
;;;  Compilation
;;    youtoo echo -l level1
;;;-----------------------------------------------------------------------------
(defmodule echo
  (syntax (macros)
   import (level1))

  (let* ((port (if (< *argc* 2) 4711 (vector-ref *argv* 1)))
         (c (make <connection> port: port))
         x)
    (while (null? (eq (setq x (read-line stdin () (eos-default-value)))
                     (eos-default-value)))
      (prin x c)
      (print (read-line c)))
    (disconnect c))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
