;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: mpis
;;;  Authors: Andreas Kind
;;; Description: MPI example module
;;;  Compilation
;;    youtoo test1 -l level1 -l mpis -ld mpicc -static
;;;  Run
;;    mpirun -np 2 test1
;;;-----------------------------------------------------------------------------
(defmodule test1
  (syntax (macros)
   import (level1 mpis))

;;;-----------------------------------------------------------------------------
;;; Symbol echo
;;;-----------------------------------------------------------------------------
(defun echo ()
  (let ((s1 (make <mpi-stream>))
        (s2 (make <mpi-stream>)))
    (cond ((local-mpi-stream? s1)
           (write "Your input: \n")
           (swrite s2 (read-line))
           (write (read s2))
           (disconnect s1))
          ((local-mpi-stream? s2)
           (swrite s1 (read s1))
           (disconnect s1))
          (t
           (error <condition>
                  "unhandled local mpi stream")))))
(echo)

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
