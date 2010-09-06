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
;;;  Title: EL-in-CL: some CL-like extensions for classes
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module class-ext
(import ((only (subtypep
                mapcar
                make-instance)
               common-lisp)
         (only (make-eulisp-class-id)
               apply-standard)
         level-0)
 syntax (level-0
         apply-standard)
 export (subclass?
         define-singleton-apply-class)
 expose ((only (defstandardclass
                 make-eulisp-class-id
                 make-eulisp-class-id)
               apply-standard)))

(defun subclass? (class1 class2)
  (subtypep class1 class2))

;;;-----------------------------------------------------------------------------
;;; Examples for defstandardclass
;;;-----------------------------------------------------------------------------
;; (defstandardclass <foo> ()
;;                   bar
;;                   (baz :reader :initform 123)
;;                   (foobar :reader :standard-options)
;;                   (foobarbaz :keyword :accessor)
;;                   :predicate)

;; (defstandardclass <bar> ()
;;                   :default-slot-options (:reader :keyword)
;;                   :slots
;;                   foo
;;                   (bar :accessor :standard-options))


(defmacro define-singleton-apply-class (identifier supers . initial-values)
  (let ((class-id (make-eulisp-class-id identifier)))
    `(progn
       (defstandardclass ,class-id ,(mapcar #'make-eulisp-class-id supers))
       (defconstant ,identifier (make-instance ,class-id ,@initial-values))
       (export ,identifier ,class-id))))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
