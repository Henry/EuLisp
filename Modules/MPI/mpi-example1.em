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
;;;  Library: mpi
;;;  Authors: Andreas Kind
;;; Description: MPI example module
;;;  Compilation
;;    youtoo mpi-example1 -l level1 -l mpis -ld mpicc -static
;;;  Run
;;    edit ./mpifile then mpirun -p4pg mpifile mpi-example2
;;;-----------------------------------------------------------------------------
(defmodule mpi-example1
  (syntax (macros)
   import (level1 mpis))

;;;-----------------------------------------------------------------------------
;;; Example
;;;-----------------------------------------------------------------------------
(let* ((p1 (make <mpi-process>))
       (p2 (make <mpi-process>))
       (the-other-p (if (eq p1 *mpi-this-process*) p2 p1))
       (x 'hello))
  (format "~a sending ~a\n" *mpi-this-process* x)
  (flush)
  (swrite the-other-p x)
  (disconnect *mpi-this-process*))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
