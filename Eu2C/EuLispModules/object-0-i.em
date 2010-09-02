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

(defmodule object-0-i
  (import (eulisp-kernel
           tail
           option-lists
           (only (<pointer-to-void>)
                 pointer-to-void)) ;;***HGW needed for defclass
   syntax (eulisp-kernel
           syntax-0
           tail)
   export (<object>
           <structure>
           make
           allocate
           initialize

           ;; the following must not be visible to the level-0 user
           <structure-class>
           <slot-description>
           slot-description-name
           slot-description-keyword
           slot-description-default-function

           ;; the following two are necessary only if initialize , deep-copy and
           ;; shallow-copy (see module 'copy') are working via slot accessors
           ;;slot-description-slot-reader
           ;;slot-description-slot-writer

           <instance-as-vector>
           get-nth-slot-value
           set-nth-slot-value))

;;;-----------------------------------------------------------------------------
;;; Slot descriptions
;;;-----------------------------------------------------------------------------
(%define-standard-class (<slot-description> <class>)
  <object>
  ((name reader slot-description-name
         type %string)
   (keyword reader slot-description-keyword)
   (default-function reader slot-description-default-function)
   ;; (reader reader slot-description-slot-reader)
   ;; (writer reader slot-description-slot-writer)
   )
  allocation single-card
  representation pointer-to-struct)

(%define-literal-expansion slot-description
  `(%literal ,<slot-description>
             name (%literal ,%string () ,name)
             keyword ,keyword
             default-function ,default-function
             ;; reader ,reader
             ;; writer ,writer
             ))

;;;-----------------------------------------------------------------------------
;;; defclass
;;;-----------------------------------------------------------------------------
(%define-metaclass (<structure-class> <class>)
  <class>
  (
   ;; no additional slots are neccesary
   ))

(%define-abstract-class (<structure> <abstract-class>)
  <object>
  ;; the superclass of all defclass-classes
  (
   ;; no predefined slots
   ))

(defmacro defclass (class-name superclass slot-descriptions . class-options)
  `(%define-standard-class
     (,class-name <structure-class>)
     (or ,superclass <object>)
     ,slot-descriptions
     representation pointer-to-struct
     allocation multiple-type-card
     ,@class-options))

;;;-----------------------------------------------------------------------------
;;; Instance creation
;;;-----------------------------------------------------------------------------
(defun make (class . initlist)
  (apply initialize
         (apply allocate class initlist)
         initlist))

(%define-function (allocate <object>) ((class <class>) . initlist)
  (%funcall (%class-allocator class)))

;;;-----------------------------------------------------------------------------
;;; Instance initialization
;;;-----------------------------------------------------------------------------
(defgeneric initialize (object . initlist))

(defmethod initialize (object . initlist)
  (standard-initialize object
                       (%class-slot-descriptions (%class-of object))
                       initlist))

;;;-----------------------------------------------------------------------------
;;; the following standard-initialize uses slot readers and slot writers
;;; for initialization

;; (defun standard-initialize (object slots initlist)
;;   (if (null? slots) object
;;     (let ((option (find-option (slot-description-keyword (car slots))
;;                                initlist
;;                                ())))
;;       ((slot-description-slot-writer (car slots))
;;        object
;;        (cond (option (car option))
;;              ((slot-description-default-function (car slots))
;;               ((slot-description-default-function (car slots))))
;;              (t (make-unbound-slot (%class-of object) (car slots)))))
;;       (standard-initialize object (cdr slots) initlist))))


;;;-----------------------------------------------------------------------------
;;; the following version of standard-initialize assumes that
;;; 1. the order and number of slots in the structures generated for and by C is
;;;    represented by the slot descriptions in the class
;;; 2. all objects are of the same size and are indirect instances of <object>

(defun standard-initialize (object slots initlist)
  (standard-init object slots initlist #%I0))

(%define-function (standard-init <object>)
  ((object <object>) (slots <list>) (initlist <list>)
   (slot-position %unsigned-word-integer))
  (if (null? slots) object
    (%let ((option <list> (find-option (slot-description-keyword (car slots))
                                       initlist
                                       ()))
           (default-function <object> (slot-description-default-function (car slots))))
          (set-nth-slot-value (%cast <instance-as-vector> object)
                              slot-position
                              (cond (option (car option))
                                    ((slot-description-default-function (car slots))
                                     ((slot-description-default-function (car slots))))
                                    (t (make-unbound-slot (%class-of object) (car slots)))))
          (standard-init object (cdr slots) initlist (%plus slot-position #%I1)))))

(%define-tail-class (<instance-as-vector> <tail-class>)
  ((length)
   (element reader get-nth-slot-value
            writer set-nth-slot-value
            type <object>))
  representation pointer-to-vector)

;;;-----------------------------------------------------------------------------
;;; Unbound slots
;;;-----------------------------------------------------------------------------
(%define-standard-class (<unbound-slot> <class>)
  <object>
  ((class reader unbound-slot-class
          type <class>
          keyword class)
   (slot reader unbound-slot-slot
         type <slot-description>
         keyword slot)
   )
  constructor (make-unbound-slot class slot)
  allocation single-card
  representation pointer-to-struct)

;;;-----------------------------------------------------------------------------
;;; type schemes for type inference
;;;-----------------------------------------------------------------------------
(%annotate-function
  make new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <object>))
    ((var var1) (atom? <class>))
    ((var var2) (atom? <list>)))))

;;(%annotate-function
;; standard-initialize new-signature
;;  (((var0 var1 var2 var3)
;;    ((var var0) (atom? <object>))
;;    ((var var1) (atom? <object>))
;;    ((var var2) (atom? <cons>))
;;    ((var var3) (atom? <list>)))
;;   ((var0 var1 var2 var3)
;;    ((var var0) (atom? <object>))
;;    ((var var1) (var var0))
;;    ((var var2) (atom? <null>))
;;    ((var var3) (atom? <list>)))))

;;;-----------------------------------------------------------------------------
) ;end of module object-0-i
;;;-----------------------------------------------------------------------------
