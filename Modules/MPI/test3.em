;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: mpi
;;;  Authors: Andreas Kind
;;; Description: MPI example module
;;;  Compilation
;;    youtoo test3 -l level1 -l mpis -ld mpicc -static
;;;  Run
;;    edit ./mpi-configure then mpirun -p4pg mpi-configure test3
;;    or see Makefile
;;;-----------------------------------------------------------------------------
(defmodule test3
  (syntax (macros)
   import (level1 mpis))

;;;-----------------------------------------------------------------------------
;;; Example
;;;-----------------------------------------------------------------------------
(let* ((s1 (make <mpi-stream>))
       (s2 (make <mpi-stream>))
       (remote-s (if (local-mpi-stream? s1) s2 s1))
       (local-s (if (local-mpi-stream? s1) s1 s2))
       ;(x "Hello world!")
       (x 12.34))
  (sformat stderr "Sending ~a from ~a to ~a\n" x local-s remote-s)
  (swrite remote-s x)
  (disconnect local-s))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
