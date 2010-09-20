;;; Copyright 1997 A. Kind & University of Bath
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;
;;  Youtoo is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
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
    (cond ((local-mpi-stream? s1)
           (while (setq x (read lispin () ()))
             (format "~a sending ~a\n" s1 x)
             (swrite s2 x))
           (disconnect s1))
          ((local-mpi-stream? s2)
           (while (setq x (read s1))
             (format "~a received instance of ~a: ~a\n"
                     s2 (class-of x) x))
           (disconnect s2))
          (t
           (error <condition>
                  "unhandled local mpi stream")))))
(test)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
