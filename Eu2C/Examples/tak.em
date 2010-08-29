;;; Copyright 1994-2010 Fraunhofer ISST
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'Eu2C'
;;;-----------------------------------------------------------------------------
;;
;;  Eu2C is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;;  Title: Benchmark 'tak'
;;;  Authors: Ingo Mohr
;;;  Description:
;;    This benchmark was taken from R.P.Gabriel: "Performance and Evaluation of
;;    Lisp-Systems" and adapted to EuLisp.
;;;  Notes:
;;    The benchmark is called with 26, 16 and 6 instead of 18, 12 and 6 as in
;;    Gabriel to get times greater than one second on current machines.
;;;  Compilation:
;;    Compile this module with basic system eulisp-level-0.
;;;  See also:
;;    takl, gtakl and gtakl2
;;;-----------------------------------------------------------------------------
(defmodule tak
  (import (eulisp-level-0)
   syntax (eulisp-level-0))

(defun tak (x y z)
  (if (null (< y x))
      z
    (tak (tak (- x 1) y z)
         (tak (- y 1) z x)
         (tak (- z 1) x y))))

;;;-----------------------------------------------------------------------------
;;; Run the test
;;;-----------------------------------------------------------------------------

(format t "~%(tak 26 16 6) => ~a~%" (tak 26 16 6))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
