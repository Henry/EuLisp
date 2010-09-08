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

#module standard-mop
(import (level-0
         lzs-mop
         lzs
         el2lzs-literals
         (only (<%pointer-to-vector>)
               representation) ;whc-classes
         (only (<%string>)
               tail-module)
         (only (find
                format)
               common-lisp))
 syntax (level-0))

;;;-----------------------------------------------------------------------------
;;; ~class-of
;;;-----------------------------------------------------------------------------
(defmethod ~class-of (object)
  (literal-type object))

(defmethod ~class-of ((object <class-def>))
  (?class object))

;;;-----------------------------------------------------------------------------
;;; Class Introspection
;;;-----------------------------------------------------------------------------
(defmethod ~class-name ((class <class-def>))
  (?identifier class))

(defmethod ~class-precedence-list ((class <class-def>))
  (?class-precedence-list class))

(defmethod ~class-slots ((class <class-def>))
  (?effective-slots class))

(defmethod ~class-keywords ((class <class-def>))
  (?keywords class))

(defmethod ~class-representation ((class <class-def>))
  (?representation class))

(defmethod ~find-slot ((class <class-def>) slot-name)
  (find slot-name (~class-slots class)
        :key #'~slot-name))

(defmethod ~class-subclasses ((class <class-def>))
  (?subclasses class))

(defmethod ~converter ((class <class-def>))
  (labels ((get-converter (class-list)
                          (cond ((null? class-list) ())
                                ((?converter (car class-list)))
                                (t (get-converter (cdr class-list))))))
          (get-converter (~class-precedence-list class))))

;;;-----------------------------------------------------------------------------
;;; Slot Introspection
;;;-----------------------------------------------------------------------------
(defmethod ~slot-name ((slot <slot-desc>))
  (?identifier slot))

(defmethod ~slot-default-function ((slot <slot-desc>))
  (?default-function slot))

(defmethod ~slot-slot-reader ((slot <slot-desc>))
  (?reader slot))

(defmethod ~slot-slot-writer ((slot <slot-desc>))
  (?writer slot))

(defmethod ~slot-type ((slot <slot-desc>))
  (?type slot))

(defmethod ~slot-keyword ((slot <slot-desc>))
  (?keyword slot))

(defmethod ~slot-initvalue ((slot <slot-desc>))
  (?initvalue slot))

;;;-----------------------------------------------------------------------------
;;; Generic Function Introspection
;;;-----------------------------------------------------------------------------
(defmethod ~generic-function-domain ((gf <generic-fun>))
  (?domain gf))

(defmethod ~generic-function-method-class ((gf <generic-fun>))
  (?method-class gf))

(defmethod ~generic-function-methods ((gf <generic-fun>))
  (?method-list gf))

(defmethod ~generic-function-method-lookup-function ((gf <generic-fun>))
  (?method-lookup-fun gf))

(defmethod ~generic-function-discriminating-function ((gf <generic-fun>))
  (?discriminating-fun gf))

(defmethod ~generic-function-discrimination-depth ((gf <generic-fun>))
  (?discrimination-depth gf))

;;;-----------------------------------------------------------------------------
;;; Method Introspection
;;;-----------------------------------------------------------------------------
(defmethod ~method-domain ((method <method-def>))
  (?domain method))

(defmethod ~method-function ((method <method-def>))
  (?fun method))

(defmethod ~method-generic-function ((method <method-def>))
  (?generic-fun method))

;;;-----------------------------------------------------------------------------
;;; Introspection of Vector Classes
;;;-----------------------------------------------------------------------------
(defmethod ~vector-class-instance-size-literal (vector-class)
  (let ((initfun (~slot-default-function
                  (~find-slot vector-class ^length))))
    (if (null? initfun)
        ()
      (init-fun-value (?body initfun) vector-class))))


(defmethod ~vector-class-instance-size (vector-class)
  (let ((initfun (~slot-default-function
                  (~find-slot vector-class ^length))))
    (if (null? initfun)
        ()
      (car (?value-list (init-fun-value (?body initfun) vector-class))))))


(defgeneric init-fun-value (default class))

(defmethod init-fun-value ((default <named-const>) class)
  (if (eq (?value default) ^unknown)
      (error-bad-init-form class default)
    (init-fun-value (?value default) class)))

(defmethod init-fun-value ((default <literal-instance>) class)
  default)

(defmethod init-fun-value ((default <integer>) class)
  default)

(defmethod init-fun-value (default class)
  (error-bad-init-form class default))

(defun error-bad-init-form (class default)
  (format t "~% -------------------- error ------------------------")
  (format t "~% invalid initialization form ~A for the vector class ~A"
          default class)
  (format t "~% ---------------------------------------------------~%")
  ())

(defmethod ~vector-class-element-type (vector-class)
  (and
   ;;(check-for-vetcor-class vector-class (?representation vector-class))
   (~slot-type
    (~find-slot vector-class ^element))))

(defmethod ~vector-class-element-default-function (vector-class)
  (and
   ;;(check-for-vetcor-class vector-class (?representation vector-class))
   (~slot-default-function
    (~find-slot vector-class ^element))))

;; check-for-vector-class cannot use the representation for its test because the
;; vector class accessors are already needed during computation of representations
;;
;;(defgeneric check-for-vetcor-class (class representation))
;;(defmethod check-for-vetcor-class
;;           (class (representation <%pointer-to-vector>))
;;  t)
;;(defmethod check-for-vetcor-class
;;           ((class <%string>) representation)
;;  ;??? why this is necessary ???
;;  t)
;;(defmethod check-for-vetcor-class (class representation)
;;  (format t "~%Error: ~A (module ~A) is no vector class"
;;          (?identifier class)
;;          (?module-id class))
;;  ())

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
