;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;; Description: handling class definitions
;;;-----------------------------------------------------------------------------
(defmodule mop-defcl
  (syntax (_boot0 _mop-gf0 _mop-meth0)
   import (boot mop-prim mop-inspect mop-key mop-class mop-init
           mop-gf mop-meth)
   export (add-subclass remove-class slot-value slot-value-using-slot
           find-slot predefined-reader predefined-writer))

;;;-----------------------------------------------------------------------------
;;; Add subclass
;;;-----------------------------------------------------------------------------
  (defgeneric add-subclass ((super <class>) (sub <class>)))
  (defmethod add-subclass ((super <class>) (sub <class>))
    ;; Would be nice to have weak pointers here
    ((setter class-direct-subclasses) super
     (cons sub (class-direct-subclasses super))))

;;;-----------------------------------------------------------------------------
;;; Remove class
;;;-----------------------------------------------------------------------------
  (defgeneric remove-class ((cl <class>)))
  (defmethod remove-class ((cl <class>))
    ;; Dodgy if cl is a metaclass
    (let ((super (car (class-direct-superclasses cl))))
      ((setter class-direct-subclasses) super
       (list-remove cl (class-direct-subclasses super)))
      ;; Update the class codes
      (compute-class-codes)
      cl))

;;;-----------------------------------------------------------------------------
;;; Slot-value-using-slot
;;;-----------------------------------------------------------------------------
  (defgeneric slot-value-using-slot ((sd <slot>) obj))
  (defmethod slot-value-using-slot ((sd <slot>) obj)
    ((slot-reader sd) obj))

  (defgeneric (setter slot-value-using-slot) ((sd <slot>) obj val))
  (defmethod (setter slot-value-using-slot) ((sd <slot>) obj val)
    ((setter (slot-reader sd)) obj val))

  (defun find-slot (cl name)
    (labels
        ((loop (l)
               (if (null? l)
                   (error "slot ~a not found in class ~a" name cl)
                 (let* ((slot (car l))
                        (x (slot-name slot)))
                   (if (eq x name) slot
                     (loop (cdr l)))))))
      (loop (class-slots cl))))

  (defun slot-value (obj name)
    (if (primitive-metaclass? (class-of (class-of obj)))
        (primitive-slot-value obj name)
      (slot-value-using-slot
       (find-slot (class-of obj) name)
       obj)))

  (defun (setter slot-value) (obj name val)
    (if (primitive-metaclass? (class-of (class-of obj)))
        ((setter primitive-slot-value) obj name val)
      ((setter slot-value-using-slot)
       (find-slot (class-of obj) name) obj val)))

;;;-----------------------------------------------------------------------------
;;; Read/write slot values
;;;-----------------------------------------------------------------------------
  (defconstant *max-predefined-accessors* 12)

  (defun predefined-reader (i)
    (if (< i *max-predefined-accessors*)
        (primitive-ref *predefined-readers* i)
      (lambda (obj) (primitive-ref obj i))))

  (defun predefined-writer (i)
    (if (< i *max-predefined-accessors*)
        (primitive-ref *predefined-writers* i)
      (lambda (obj val) ((setter primitive-ref) obj i val))))

  (deflocal *predefined-readers*
    (primitive-allocate () *max-predefined-accessors*))

  (deflocal *predefined-writers*
    (primitive-allocate () *max-predefined-accessors*))

  ((setter primitive-ref) *predefined-readers* 0
   (lambda (obj) (primitive-ref obj 0)))

  ((setter primitive-ref) *predefined-readers* 1
   (lambda (obj) (primitive-ref obj 1)))

  ((setter primitive-ref) *predefined-readers* 2
   (lambda (obj) (primitive-ref obj 2)))

  ((setter primitive-ref) *predefined-readers* 3
   (lambda (obj) (primitive-ref obj 3)))

  ((setter primitive-ref) *predefined-readers* 4
   (lambda (obj) (primitive-ref obj 4)))

  ((setter primitive-ref) *predefined-readers* 5
   (lambda (obj) (primitive-ref obj 5)))

  ((setter primitive-ref) *predefined-readers* 6
   (lambda (obj) (primitive-ref obj 6)))

  ((setter primitive-ref) *predefined-readers* 7
   (lambda (obj) (primitive-ref obj 7)))

  ((setter primitive-ref) *predefined-readers* 8
   (lambda (obj) (primitive-ref obj 8)))

  ((setter primitive-ref) *predefined-readers* 9
   (lambda (obj) (primitive-ref obj 9)))

  ((setter primitive-ref) *predefined-readers* 10
   (lambda (obj) (primitive-ref obj 10)))

  ((setter primitive-ref) *predefined-readers* 11
   (lambda (obj) (primitive-ref obj 11)))

  ((setter primitive-ref) *predefined-writers* 0
   (lambda (obj val) ((setter primitive-ref) obj 0 val)))

  ((setter primitive-ref) *predefined-writers* 1
   (lambda (obj val) ((setter primitive-ref) obj 1 val)))

  ((setter primitive-ref) *predefined-writers* 2
   (lambda (obj val) ((setter primitive-ref) obj 2 val)))

  ((setter primitive-ref) *predefined-writers* 3
   (lambda (obj val) ((setter primitive-ref) obj 3 val)))

  ((setter primitive-ref) *predefined-writers* 4
   (lambda (obj val) ((setter primitive-ref) obj 4 val)))

  ((setter primitive-ref) *predefined-writers* 5
   (lambda (obj val) ((setter primitive-ref) obj 5 val)))

  ((setter primitive-ref) *predefined-writers* 6
   (lambda (obj val) ((setter primitive-ref) obj 6 val)))

  ((setter primitive-ref) *predefined-writers* 7
   (lambda (obj val) ((setter primitive-ref) obj 7 val)))

  ((setter primitive-ref) *predefined-writers* 8
   (lambda (obj val) ((setter primitive-ref) obj 8 val)))

  ((setter primitive-ref) *predefined-writers* 9
   (lambda (obj val) ((setter primitive-ref) obj 9 val)))

  ((setter primitive-ref) *predefined-writers* 10
   (lambda (obj val) ((setter primitive-ref) obj 10 val)))

  ((setter primitive-ref) *predefined-writers* 11
   (lambda (obj val) ((setter primitive-ref) obj 11 val)))

  (defun primitive-make-slot (name i default)
    (let ((sd (primitive-allocate <local-slot> lsd-size)))
      ((setter slot-name) sd name)
      ((setter slot-keyword) sd (make-keyword (symbol-name name)))
      ((setter slot-required?) sd (eq default required:))
      ((setter slot-default) sd default)
      ((setter slot-reader) sd (predefined-reader i))
      ((setter slot-writer) sd (predefined-writer i))
      sd))

  (defun make-slotds (names defaults . offset)
    (labels
     ((loop (ns ds i res)
            (if (null? ns) (reverse-list res)
              (let ((sd (primitive-make-slot (car ns) i (car ds))))
                (loop (cdr ns) (cdr ds) (+ i 1) (cons sd res))))))
     (loop names defaults (if offset (car offset) 0) ())))

  (let ((class-slotds (make-slotds class-slotz class-slot-defaults)))
    ((setter class-slots) <simple-class> class-slotds)
    ((setter class-slots) <class> class-slotds)
    ((setter class-slots) <function-class> class-slotds))

  (let* ((gf-slotds (make-slotds gf-slots gf-slot-defaults))
         (f-slotds (list (car gf-slotds) (car (cdr gf-slotds))
                         (car (cdr (cdr gf-slotds)))))
         (sf-slotds
          (make-slotds sf-direct-slots sf-direct-slot-defaults
           function-size)))
    ((setter class-slots) <function> f-slotds)
    ((setter class-slots) <simple-function> (append f-slotds sf-slotds))
    ((setter class-slots) <generic-function> gf-slotds)
    ((setter class-slots) <simple-generic-function> gf-slotds))

  ;; Simple function code accessors need to be reset because the object
  ;; size may be <5 so that primitive-ref signals an error when accessing
  ;; simple function code. The object size does NOT reflect the size of the
  ;; object, but the size of the code string (similar to strings).
  (let ((slot (find-slot <simple-function> 'code)))
    ((setter slot-reader) slot simple-function-code)
    ((setter slot-writer) slot (setter simple-function-code)))

  (let ((method-slotds (make-slotds method-slots method-slot-defaults)))
    ((setter class-slots) <method> method-slotds)
    ((setter class-slots) <simple-method> method-slotds))

  (let* ((lsd-slotds (make-slotds lsd-slots lsd-slot-defaults))
         (sd-slotds (list (car lsd-slotds) (car (cdr lsd-slotds)))))
    ((setter class-slots) <slot> sd-slotds)
    ((setter class-slots) <local-slot> lsd-slotds))

  (let ((name-slotds (make-slotds name-slots name-slot-defaults)))
    ((setter class-slots) <name> name-slotds)
    ((setter class-slots) <symbol> name-slotds)
    ((setter class-slots) <keyword> name-slotds))

  (let ((cons-slotds (make-slotds cons-slots cons-slot-defaults)))
    ((setter class-slots) <cons> cons-slotds))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
