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
;;; Title: EL-in-CL: module list
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

#module list
(import (eulisp-kernel
         null
         (only (<symbol>)
               symbol)
         (only (<number>)
               number)
         (only (<character>)
               character)
         (only (cons
                car
                cdr
                copy-list)
               common-lisp)
         copy-generic
         ;;deep-copy
         ;;shallow-copy
         )
 syntax (eulisp-kernel)
 expose (null
         (only (cons
                car
                cdr
                list
                length ; not in EL
                equal
                copy-list ; not in EL
                ;;copy-alist
                ;;copy-tree
                )
               common-lisp)
         (only ($empty-list)
               el-modules))
 export (atom?
         cons?
         list?
         deep-copy
         shallow-copy))

(make-eulisp-class pair cons) ; not in EL
(make-eulisp-class cons cons)
(make-eulisp-class list list)

(defun atom? (a)
  (cl:atom a))

(defun cons? (a)
  (cl:consp a))

(defun list? (a)
  (cl:listp a))

(defmethod deep-copy ((tree cons))
  (cons (deep-copy (car tree))
        (deep-copy (cdr tree))))

(defmethod deep-copy ((tree <null>))
  ())

(defmethod deep-copy ((tree <symbol>))
  tree)

(defmethod deep-copy ((tree <number>))
  tree)

(defmethod deep-copy ((tree <character>))
  tree)

(defmethod shallow-copy ((tree <null>))
  ())

(defmethod shallow-copy ((tree <cons>))
  (copy-list tree))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
