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
;;;  Title: 
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Horst Friedrich
;;;-----------------------------------------------------------------------------

#module progn-context
(import
 (eulisp1
  SIMPLE-PROGRAMMING
  LZS
  MZS
  context
  analyse-h) ; make-vector and vector-ref
 ;; typeinfernce

 syntax
 (eulisp1)

 export
 (l2m-progn)
 )

;;------------------------------
;; constants
;;------------------------------

(defmethod l2m-progn (con progn-list)
  (cond ((cdr progn-list)
         (l2m-a (dynamic *void-context*) (car progn-list))
         (l2m-progn con  (cdr progn-list)))
        (t (l2m-a con (car progn-list))))
  )

#module-end
