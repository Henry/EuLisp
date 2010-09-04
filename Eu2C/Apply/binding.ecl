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
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module binding
(import (level-0
         apply-standard
         accessors
         (only (make-instance call-next-method)
               common-lisp))
 syntax (level-0
         apply-standard)
 export (<binding>
         binding?
         make-binding
         finally-refered-object
         get-lzs-object)
 )

;;;-----------------------------------------------------------------------------
;;; bindings
;;;-----------------------------------------------------------------------------

(defstandardclass <binding> ()
  (identifier :reader :initarg)
  (code-identifier :accessor :initform ())
  (refers-to :reader :initarg)
  (refers-finally-to :accessor :initform ())
  :predicate)

(defun make-binding options
  (let ((binding (apply #'make-instance <binding> options)))
    (setf (?refers-finally-to binding)
          (finally-refered-object (?refers-to binding)))
    binding))

(defgeneric finally-refered-object (obj))
(defmethod finally-refered-object (obj) obj)
(defmethod finally-refered-object ((obj <binding>))
  (or (?refers-finally-to obj)
      (setf (?refers-finally-to obj)
            (finally-refered-object (?refers-to obj)))))

(defmethod ?exported ((ref <binding>))
  (?exported (?refers-finally-to ref)))

(defun get-lzs-object (object)
  ;; get-lzs-object returns the object finally refered to if object is a
  ;; renamed object or otherwise returns its argument
  (if (binding? object)
      (?refers-finally-to object)
    object))


#module-end
