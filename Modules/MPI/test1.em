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
;;; Title: MPI example module
;;;  Library: mpis
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;  Compilation:
;;    youtoo test1 -l level1 -l mpis -ld mpicc -static
;;;  Run:
;;    mpirun -np 2 test1
;;;-----------------------------------------------------------------------------

(defmodule test1
  (syntax (macros)
   import (level1
           mpis))

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
)  ;; End of module test1
;;;-----------------------------------------------------------------------------
