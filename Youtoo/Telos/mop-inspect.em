;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;; Description: inspection
;;;-----------------------------------------------------------------------------
(defmodule mop-inspect
  (import (boot mop-prim mop-class mop-init)
   syntax (_boot0)
   export (subclass? function? methodp generic-function? cpl-subclass?
                     class? class-of slotp primitive-metaclass?
                     primitive-find-slot-position primitive-slot-value))

;;;-----------------------------------------------------------------------------
;;; Class-of (identical to primitive-class-of)
;;;-----------------------------------------------------------------------------
(defun class-of (obj) ((opencoded-lambda (o) (primitive-class-of)) obj))
(declare-inline class-of)

;;;-----------------------------------------------------------------------------
;;; Subclass?
;;;-----------------------------------------------------------------------------
(defun subclass? (cl1 cl2)
  (if (eq cl1 cl2) t
    (let ((code1 (class-code cl1))
          (code2 (class-code cl2)))
      (if (and code1 code2)
          (subcode? code1 code2)
        (labels
         ((loop (l)
                (if (null? l) ()
                  (if (subclass? (car l) cl2)
                      cl1
                    (loop (cdr l))))))
         (loop (class-direct-superclasses cl1)))))))

(defun cpl-subclass? (cl1 cl2)
  (if (eq cl1 cl2) t
    (let ((code1 (class-code cl1))
          (code2 (class-code cl2)))
      (if (and code1 code2)
          (subcode? code1 code2)
        (member1-list cl2 (class-precedence-list cl1))))))


(defun subcode? (code1 code2)
  (if (< (car code1) (car code2)) ()
    (if (< (cdr code2) (cdr code1)) ()
      t)))
;;(declare-inline subcode?)

;;;-----------------------------------------------------------------------------
;;; Class predicates (<null>, <cons> have not yet its superclasses!)
;;;-----------------------------------------------------------------------------
(defun class? (a)
  (if (subclass? (class-of a) <class>) a ()))
;;(declare-inline class?)

(defun slotp (a)
  (if (subclass? (class-of a) <slot>) a ()))
;;(declare-inline slotp)

(defun generic-function? (a)
  (if (subclass? (class-of a) <generic-function>) a ()))
;;(declare-inline generic-function?)

(defun methodp (a)
  (if (subclass? (class-of a) <method>) a ()))
;;(declare-inline methodp)

(defun function? (a)
  (if (subclass? (class-of a) <function>) a ()))
;;(declare-inline function?)

;;;-----------------------------------------------------------------------------
;;; Primitive classes
;;;-----------------------------------------------------------------------------
(defconstant primitive-metaclasses
  (list <simple-class> <class> <function-class> <class>))

(defun primitive-metaclass? (obj)
  ;; cannot be inlined with binding not exported
  (member1-list obj primitive-metaclasses))

;;;-----------------------------------------------------------------------------
;;; Primitive slot value access
;;;-----------------------------------------------------------------------------
(defun primitive-find-slot-position (cl name slots index)
  (cond ((null? slots)
         (error () "slot ~a not found in class ~a" name cl))
        ((eq name (slot-name (car slots)))
         index)
        (t
         (primitive-find-slot-position cl name (cdr slots) (+ index 1)))))

(defun primitive-slot-value (obj name)
  (let* ((cl (class-of obj))
         (i (primitive-find-slot-position cl name (class-slots cl) 0)))
    (primitive-ref obj i)))

(defun (setter primitive-slot-value) (obj name val)
  (let* ((cl (class-of obj))
         (i (primitive-find-slot-position cl name (class-slots cl) 0)))
    ((setter primitive-ref) obj i val)))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
