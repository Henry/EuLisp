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
;;;  Title: Type Expressions for Type Inference
;;;  Description:
;;    Type expressions can be subdivided into atomic types, slot identifications, and type variables. Atomic types have a link to a lattice type or to a combinations of lattice types. A combination of lattice types can be created with lattice operations meet, join, and complement.
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Andreas Kind
;;;-----------------------------------------------------------------------------


#module ti-exprs
(import (lzs lzs-mop
             machine-description ti ti-codes ti-lattice
             (only (find) common-lisp))
 syntax (ti)
 export (<type-expr>
         <atomic-type> ?code ?name ?comp-name
         <type-var> ?id
         <slot-id> ?slot-name
         atomic-type? type-var? slot-id?
         contains-type-var? eq-type-var?
         substitute-type-var class-as-type-expr
         new-type-var new-type-var-id reset-actual-type-var-id
         lattice-type-to-atomic-type lattice-type-to-atomic-types
         general-type general-type-p
         <null>-type <null>-type?
         %object-type %object-type?
         %void-type %void-type?
         %false-type %false-type-p
         fpi-list-type fpi-list-type-p
         <function>-type <function>-type-p
         <fpi>-type <fpi>-type-p
         no-type? %class-type? %function-type %integer-type)
 )

;;;-----------------------------------------------------------------------------
;;; TYPE EXPRESSIONS
;;;-----------------------------------------------------------------------------

(defstandardclass <type-expr> ())

(defstandardclass <atomic-type> (<type-expr>)
  (code :accessor :initarg :initform ())
  (name :accessor :initarg :initform ())
  (comp-name :accessor :initarg :initform ()))

(defstandardclass <type-var> (<type-expr>)
  (id :accessor :initarg :initform ()))

(defstandardclass <slot-id> (<type-expr>)
  (slot-name :accessor :initarg :initform ()))

;;;-----------------------------------------------------------------------------
;;; TYPE EXPRESSION PREDICATES
;;;-----------------------------------------------------------------------------

(defgeneric atomic-type? (expr))

(defmethod atomic-type? ((expr <type-expr>))
  #f)

(defmethod atomic-type? ((expr <atomic-type>))
  #t)

;;;-----------------------------------------------------------------------------
(defgeneric type-var? (expr))

(defmethod type-var? ((expr <type-expr>))
  #f)

(defmethod type-var? ((expr <type-var>))
  #t)

;;;-----------------------------------------------------------------------------
(defgeneric slot-id? (expr))

(defmethod slot-id? ((expr <slot-id>))
  #t)

(defmethod slot-id? ((expr <type-expr>))
  #f)

;;;-----------------------------------------------------------------------------
;;; Answer whether an type expression contains a type varible.
(defgeneric contains-type-var? (obj var))

(defmethod contains-type-var? ((expr <type-expr>)
                                (var <type-var>))
  #f)

(defmethod contains-type-var? ((expr <type-var>)
                                (var <type-var>))
  (eq-type-var? expr var))

(defmethod contains-type-var? ((obj <pair>)
                                (var <type-var>))
  (member-with-args #'contains-type-var? obj var))

(defmethod contains-type-var? ((vec <vector>)
                                (var <type-var>))
  (find var vec :test #'eq-type-var?))

;;;-----------------------------------------------------------------------------
;;; Answer whether two type varibles are equal.
(defgeneric eq-type-var? (var1 var2))

(defmethod eq-type-var? (var1 var2)
  ())

(defmethod eq-type-var? ((var1 <type-var>)
                          (var2 <type-var>))
  (eq (?id var1) (?id var2)))

;;;-----------------------------------------------------------------------------
;;; TYPE VARIABLE IDENTIFICATION
;;;-----------------------------------------------------------------------------

(DEFLOCAL *actual-type-var-id* 0)

(defun reset-actual-type-var-id ()
  (setq *actual-type-var-id* 0))

;;; Answer a new type variable identifier.
(defun new-type-var-id ()
  (setq *actual-type-var-id* (+ *actual-type-var-id* 1))
  *actual-type-var-id*)

;;; Answer a new type variable.
(defun new-type-var ()
  (make <type-var> :id (new-type-var-id)))

;;;-----------------------------------------------------------------------------
;;; TYPE EXPRESSION/LATTICE TYPE CONNECTION
;;;-----------------------------------------------------------------------------

(defun lattice-type-to-atomic-type (lattice-type)
  (let ((expr (?atomic-expr lattice-type)))
    (if expr
        (if (?compound lattice-type)
            (make <atomic-type>
                  :code (?code lattice-type)
                  :name lattice-type
                  :comp-name t)
          expr)
      (let ((new-expr (make <atomic-type>
                            :code (?code lattice-type)
                            :name lattice-type
                            :comp-name (?compound lattice-type))))
        (setf (?atomic-expr lattice-type) new-expr)
        new-expr))))

(defun lattice-types-to-atomic-types (lattice-types)
  (mapcar #'lattice-type-to-atomic-type lattice-types))

;;; Answer a type expression for the top of the lattice.
(defun general-type ()
  (lattice-type-to-atomic-type *top*))

(defun %object-type ()
  (lattice-type-to-atomic-type *%object*))

(defun %void-type ()
  (lattice-type-to-atomic-type *%void*))

(defun %integer-type ()
  (lattice-type-to-atomic-type *%integer*))

(defun %false-type ()
  (lattice-type-to-atomic-type *%false*))

(defun %function-type ()
  (lattice-type-to-atomic-type *%function*))

(defun %class-type ()
  (lattice-type-to-atomic-type *%class*))

(defun fpi-list-type ()
  (lattice-type-to-atomic-type *fpi-list*))

(defun <null>-type ()
  (lattice-type-to-atomic-type *<null>*))

(defun <function>-type ()
  (lattice-type-to-atomic-type *<function>*))

(defun <fpi>-type ()
  (lattice-type-to-atomic-type *<fpi>*))

;;; Answer whether a type expression correspondes to the top of the lattice.
(defun general-type-p (type-expr)
  (eq-code? (?code type-expr) *top-code*))

;;; Answer whether a type expression correspondes to %object.
(defun %object-type? (type-expr)
  (eq-code? (?code type-expr) *%object-code*))

;;; Answer whether a type expression correspondes to %false.
(defun %false-type-p (type-expr)
  (eq-code? (?code type-expr) *%false-code*))

;;; Answer whether a type expression correspondes to %void.
(defun %void-type? (type-expr)
  (and (atomic-type? type-expr)
       (eq-code? (?code type-expr) *%void-code*)))

;;; Answer whether a type expression correspondes to %class.
(defun %class-type? (type-expr)
  (and (atomic-type? type-expr)
       (eq-code? (?code type-expr) *%class-code*)))

;;; Answer whether a type expression correspondes to fpi-list.
(defun fpi-list-type-p (type-expr)
  (eq-code? (?code type-expr) *fpi-list-code*))

;;; Answer whether a type expression correspondes to the bottom of the lattice.
(defun no-type? (type-expr)
  (eq-code? (?code type-expr) *bottom-code*))

;;; Answer whether a type expression correspondes to <null>.
(defun <null>-type? (type-expr)
  (eq-code? (?code type-expr) *<null>-code*))

;;; Answer whether a type expression correspondes to <null>.
(defun <function>-type-p (type-expr)
  (eq-code? (?code type-expr) *<function>-code*))

(defun <fpi>-type-p (type-expr)
  (eq-code? (?code type-expr) *<fpi>-code*))

;;;-----------------------------------------------------------------------------
;;; TYPE EXPRESSION/LZS-CLASS CONNECTION
;;;-----------------------------------------------------------------------------

;;; Answer a corresponding type expr to a application class (lzs-class).
(defgeneric class-as-type-expr (class))

(defmethod class-as-type-expr ((class <named-const>))
  (class-as-type-expr (?value class)))

(defmethod class-as-type-expr ((class <null>))
  (ti-format t "Warning: no class for class-as-type-expr")
  (ti-error)
  (general-type))

(defmethod class-as-type-expr ((class <class-def>))
  (let ((lattice-type (?lattice-type class)))
    (if (null? lattice-type)
        (ti-format t  "Warning: Can't find lattice type for class ~A" class)
      (lattice-type-to-atomic-type lattice-type))))

#module-end
