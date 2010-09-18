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
;;; Title: Example 'command-line'
;;;  Description:
;;    Test the POSIX command-line handling
;;;  Compilation:
;;    Compile this module with extended system level-0x.
;;;-----------------------------------------------------------------------------
(defmodule command-line
  (import (level-0x)
   syntax (level-0x))

(do (lambda (arg)
      (format "~%~s" arg))
    (command-line-arguments))

(deflocal argv (command-line-arguments))
(deflocal argc (size argv))

(defun select ()
  (format "~%argument number:")
  (let ((n (read)))
    (cond ((null? (number? n)))
          ((< n argc)
           (format "~%~s" (element argv n))
           (select))
          (t (format "~%There are only the arguments 0..~a" (- argc 1))
             (select)))))

;;;-----------------------------------------------------------------------------
;;; Run the test
;;;-----------------------------------------------------------------------------

(select)

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
