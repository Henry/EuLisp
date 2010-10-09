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
;;; Title: error handling
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule i-error
  (syntax (_macros
           _i-aux0)
   import (i-level1
           i-param
           i-notify)
   export (<ct-error>
           ct-error
           ct-exit))

;;;-----------------------------------------------------------------------------
;;; Compile-time error conditions
;;;-----------------------------------------------------------------------------
(defcondition <ct-error> <condition>
  ((value keyword: ct-error-value: accessor: ct-error-value)))

(defmethod get-ct-error-condition-class (x) <ct-error>)

(defmethod generic-print ((c <ct-error>) (s <stream>))
  (ct-serious-warning (ct-error-value c) (condition-message c)))

(defun ct-error (value str . args)
  (error <ct-error>
         (apply fmt str args) ct-error-value: value))

;;;-----------------------------------------------------------------------------
;;; Exit from compilation
;;;-----------------------------------------------------------------------------
(defun ct-exit values
  (let ((value (if values (car values) ())))
    (if (= *number-of-warnings* 0) ()
      (sformat stderr "*** TOTAL NUMBER OF WARNINGS: ~a\n"
               *number-of-warnings*))
    (if (= *number-of-errors* 0) ()
      (progn
        (sformat stderr "*** TOTAL NUMBER OF ERRORS: ~a\n"
                 *number-of-errors*)
        (if values ()
          (setq values -1))))
    (exit value)))

;;;-----------------------------------------------------------------------------
)  ;; End of module i-error
;;;-----------------------------------------------------------------------------
