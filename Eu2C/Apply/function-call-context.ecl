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
;;; Title: all analyses in context function-call
;;;  Description: fill the var-vector of a statement
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Horst Friedrich
;;;-----------------------------------------------------------------------------

#module function-call-context
(import
 (level-1
  SIMPLE-PROGRAMMING
  LZS
  MZS
  context
  analyse-h
  ;;  function-call
  vector) ; make-vector and vector-ref
 ;; typeinfernce

 syntax
 (level-1)

 export (l2m-call
         simple-constant?)
 )


;;;-----------------------------------------------------------------------------
;;; constants
;;;-----------------------------------------------------------------------------
(defmethod l2m-call (con  arg-list)
  (l2m-function-call con arg-list 1 (dynamic *arg-context*))
  ;; fill the var-vector
  )

(defun l2m-function-call (statement arg-list nr arg-context)
  (if (null? arg-list) ()
    (let ((var-or-constant (l2m-a arg-context (car arg-list))))
      (setf (vector-ref (?var-vec (?var-descr statement))
                        nr) var-or-constant)
      (if (simple-constant? var-or-constant)
          (setf (?constant-counter
                 (?var-descr statement))
                (+ 1 (?constant-counter
                      (?var-descr statement))))
        ())
      (l2m-function-call statement (cdr arg-list)
                         (+ nr 1) arg-context)
      )))


(defgeneric simple-constant? (lzs-object))

(defmethod simple-constant? ((obj <named-const>))
  (simple-constant? (?value obj)))

(defmethod simple-constant? ((obj <structured-literal>))
  t)

(defmethod simple-constant? ((obj <sym>))
  t)

(defmethod simple-constant? ((obj <symbol>))
  t)

(defmethod simple-constant? ((obj <fpi>))
  t)

(defmethod simple-constant? ((obj <double-float>))
  t)

(defmethod simple-constant? ((obj <character>))
  t)

(defmethod simple-constant? ((obj <null>))
  t)

(defmethod simple-constant? ((obj <object>))
  #f)

#module-end
