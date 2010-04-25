;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: mpis
;;;  Authors: Andreas Kind
;;; Description: MPI example module
;;;  Compilation
;;    youtoo test2 -l level1 -l mpis -ld mpicc -static
;;;  Run
;;    mpirun -np 2 mpi-example
;;;-----------------------------------------------------------------------------
(defmodule test2
  (syntax (macros)
   import (level1 mpis))

;;;-----------------------------------------------------------------------------
;;; Send user input
;;;-----------------------------------------------------------------------------
  (defun test ()
    ;; Queries the user for objects to send
    (let ((s1 (make <mpi-stream>))
          (s2 (make <mpi-stream>))
          x)
      (cond ((local-mpi-stream-p s1)
             (while (setq x (read lispin () ()))
               (format t "~a sending ~a\n" s1 x)
               (write x s2))
             (disconnect s1))
            ((local-mpi-stream-p s2)
             (while (setq x (read s1))
               (format t "~a received instance of ~a: ~a\n"
                       s2 (class-of x) x))
             (disconnect s2))
            (t
             (error "unhandled local mpi stream" <condition>)))))
  (test)

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
