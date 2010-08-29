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
;;;  Title: EL-in-CL: module spint
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module spint
(import (eulisp-kernel
         (only (find-class
                class-name)
               common-lisp)
         #+ :cmu (only (fixnump) ext)
         number)
 syntax (eulisp-kernel)
 export (single-precision-integer-p)
 expose ((only (evenp oddp)
               common-lisp)))


(progn
  (make-eulisp-class spint fixnum)
  ;; the alias with a short namemust appear before the following assignment, to
  ;; rename the CL-class to single-precision-integer and not to spint
  (make-eulisp-class single-precision-integer fixnum)
  )

(defun single-precision-integer-p (x)
  (fixnump x))

#module-end
