;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: mpi
;;;  Authors: Andreas Kind
;;;  Description: MPI example module
;;;  Compilation: youtoo mpi-example1 -l level1 -l mpis -ld mpicc -static
;;;  Run: edit ./mpifile then mpirun -p4pg mpifile mpi-example2
;;; -----------------------------------------------------------------------
(defmodule mpi-example1
  (syntax (macros)
   import (level1 mpis))
;;; --------------------------------------------------------------------
;;; Example
;;; --------------------------------------------------------------------
  
  (let* ((p1 (make <mpi-process>))
         (p2 (make <mpi-process>))
         (the-other-p (if (eq p1 *mpi-this-process*) p2 p1))
         (x 'hello))
    (format t "~a sending ~a\n" *mpi-this-process* x)
    (flush)
    (write x the-other-p)
    (disconnect *mpi-this-process*))
)  ;; end of module
