;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: mpis
;;;  Authors: Andreas Kind
;;; Description: MPI example module
;;;  Compilation
;;    youtoo test4 -l level1 -l mpis -ld mpicc -static
;;;  Run
;;    edit ./mpi-configure then mpirun -p4pg mpi-configure test3
;;    or see Makefile
;;;-----------------------------------------------------------------------------
(defmodule test4
  (syntax (macros)
   import (level1 mpis))

;;;-----------------------------------------------------------------------------
;;; Example
;;;-----------------------------------------------------------------------------
(let* ((s1 (make <mpi-stream>))
       (s2 (make <mpi-stream>))
       (remote-s (if (local-mpi-stream? s1) s2 s1))
       (local-s (if (local-mpi-stream? s1) s1 s2))
       (x (generic-read remote-s () ())))
  (format "Received ~a at ~a from ~a\n" x local-s remote-s)
  (disconnect local-s))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
