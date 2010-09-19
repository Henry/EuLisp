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
;;; Title: Example 'umdrehen'
;;;  Authors: E. Ulrich Kriegel
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    A variation of reverse.
;;;  Compilation:
;;    Compile this module with extended system level-0x.
;;;-----------------------------------------------------------------------------
(defmodule umdrehen
  (import (level-0x)
   syntax (level-0x timing))

(defun umdrehen (l)
  (if (null? l) ()
    (if (null? (cdr l))
        l
      (cons (car (umdrehen (cdr l)))
            (umdrehen (cons (car l)
                            (umdrehen (cdr (umdrehen (cdr l))))))))))

;;;-----------------------------------------------------------------------------
;;; Run the test
;;;-----------------------------------------------------------------------------

;; There is currently an issue with the GC running 64bit:
(deflocal liste '(1 2 3 4 5 6 7 8 9 10 11 12 13 14)) ;; Works 32bit not 64bit
;;(deflocal liste '(1 2 3 4 5 6 7 8 9)) ;; Works both 32bit and 64bit

(format "~%(umdrehen '~a)" liste)

(format "~%==> ~a~%" (time (umdrehen liste)
                             "\numdrehen: usertime %.2f sec (system %.2f)"))

;;;-----------------------------------------------------------------------------
)  ;; End of module umdrehen
;;;-----------------------------------------------------------------------------
