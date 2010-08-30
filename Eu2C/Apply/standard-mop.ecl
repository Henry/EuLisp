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
;;;-----------------------------------------------------------------------------

#module standard-mop
(import
 (eulisp0
  lzs-mop
  lzs
  el2lzs-literals
  (only (<%pointer-to-vector>) representation) ;whc-classes
  (only (<%string>) tail-module)
  (only (find format) common-lisp))
 syntax
 (eulisp0)
 )

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

(defmethod ~class-slot-descriptions ((class <class-def>))
  (?effective-slots class))

(defmethod ~class-keywords ((class <class-def>))
  (?keywords class))

(defmethod ~class-representation ((class <class-def>))
  (?representation class))

(defmethod ~find-slot-description ((class <class-def>) slot-name)
  (find slot-name (~class-slot-descriptions class)
        :key #'~slot-description-name))

(defmethod ~class-subclasses ((class <class-def>))
  (?subclasses class))

(defmethod ~converter ((class <class-def>))
  (labels ((get-converter (class-list)
                          (cond ((null? class-list) nil)
                                ((?converter (car class-list)))
                                (t (get-converter (cdr class-list))))))
          (get-converter (~class-precedence-list class))))

;;;-----------------------------------------------------------------------------
;;; Slot Introspection
;;;-----------------------------------------------------------------------------

(defmethod ~slot-description-name ((slot <slot-desc>))
  (?identifier slot))

(defmethod ~slot-description-default-function ((slot <slot-desc>))
  (?default-function slot))

(defmethod ~slot-description-slot-reader ((slot <slot-desc>))
  (?reader slot))

(defmethod ~slot-description-slot-writer ((slot <slot-desc>))
  (?writer slot))

(defmethod ~slot-description-type ((slot <slot-desc>))
  (?type slot))

(defmethod ~slot-description-keyword ((slot <slot-desc>))
  (?keyword slot))

(defmethod ~slot-description-initvalue ((slot <slot-desc>))
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

(defmethod ~vector-class-instance-length-literal (vector-class)
  (let ((initfun (~slot-description-default-function
                  (~find-slot-description vector-class ^length))))
    (if (null? initfun)
        nil
      (init-fun-value (?body initfun) vector-class))))


(defmethod ~vector-class-instance-length (vector-class)
  (let ((initfun (~slot-description-default-function
                  (~find-slot-description vector-class ^length))))
    (if (null? initfun)
        nil
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
  nil)

(defmethod ~vector-class-element-type (vector-class)
  (and
   ;;(check-for-vetcor-class vector-class (?representation vector-class))
   (~slot-description-type
    (~find-slot-description vector-class ^element))))

(defmethod ~vector-class-element-default-function (vector-class)
  (and
   ;;(check-for-vetcor-class vector-class (?representation vector-class))
   (~slot-description-default-function
    (~find-slot-description vector-class ^element))))

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
;;  nil)

#module-end
