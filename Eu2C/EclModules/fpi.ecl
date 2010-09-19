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
;;; Title: EL-in-CL: module fpint
;;;  Description:
;;;  Authors: Ingo Mohr
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

#module fpi
(import (eulisp-kernel
         (only (find-class
                class-name)
               common-lisp)
         (only (fixnump)
               ext))
 export (int?)
 expose (number)
 syntax (eulisp-kernel))

(progn
  (make-eulisp-class fpi fixnum)
  ;; the alias with a short namemust appear before the following assignment, to
  ;; rename the CL-class to int and not to fpi
  (make-eulisp-class int fixnum))

(defun int? (x)
  (fixnump x))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
