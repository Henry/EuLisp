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
;;    youtoo test3 -l level-1 -l mpis -ld mpicc --static
;;;  Run:
;;    edit ./mpi-configure then mpirun -p4pg mpi-configure test3
;;    or see Makefile
;;;-----------------------------------------------------------------------------

(defmodule test3
  (syntax (macros)
   import (level-1
           mpis))

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
)  ;; End of module test3
;;;-----------------------------------------------------------------------------
