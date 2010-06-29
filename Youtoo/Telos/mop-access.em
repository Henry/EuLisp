;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;; Description: accessors
;;;-----------------------------------------------------------------------------
(defmodule mop-access
  (syntax (_boot0 _mop-gf0 _mop-meth0)
   import (boot mop-prim mop-key mop-class mop-inspect mop-gf
           mop-meth mop-defcl)
   export (compute-primitive-reader-using-slot
           compute-primitive-writer-using-slot
           compute-primitive-reader-using-class
           compute-primitive-writer-using-class
           compute-and-ensure-slot-accessors
           compute-slot-reader compute-slot-writer
           ensure-slot-reader ensure-slot-writer
           find-slot-names spprint pprint))

;;;-----------------------------------------------------------------------------
;;; Compute-and-ensure-slot-accessors
;;;-----------------------------------------------------------------------------
  (defgeneric compute-and-ensure-slot-accessors
    ((cl <class>) effective-slotds inherited-slotds))

  (defmethod compute-and-ensure-slot-accessors
    ((cl <class>) effective-slotds inherited-slotds)
    ;; If inheriting a sd, assume its reader & writer are OK.
    (do1-list (lambda (sd)
                (if (member-list (slot-reader sd)
                                 (car inherited-slotds)
                                 (lambda (x y) (eq x (slot-reader y)))) ()
                  (let ((reader (compute-slot-reader cl sd effective-slotds))
                        (writer (compute-slot-writer cl sd effective-slotds)))
                    ((setter slot-reader) sd reader)
                    ((setter slot-writer) sd writer)))
                (ensure-slot-reader cl sd effective-slotds (slot-reader sd))
                (ensure-slot-writer cl sd effective-slotds (slot-writer sd)))
              effective-slotds)
    effective-slotds)

;;;-----------------------------------------------------------------------------
;;; Compute-slot-reader
;;;-----------------------------------------------------------------------------
  (defgeneric compute-slot-reader
    ((cl <class>) (slotd <slot>) effective-slotds))

  (defmethod compute-slot-reader
    ((cl <class>) (slotd <slot>) effective-slotds)
    (generic-lambda ((obj cl))))

  (defmethod compute-slot-reader
    ((cl <class>) (slotd <local-slot>) effective-slotds)
    ;; Accessors are simple functions.
    (let ((name (slot-name slotd)))
      (labels
          ((loop (n l)
                 (if (eq name (slot-name (car l)))
                     n
                   (loop (+ n 1) (cdr l)))))
        (let ((i (loop 0 effective-slotds)))
          (predefined-reader i)))))

;;;-----------------------------------------------------------------------------
;;; Compute-slot-writer
;;;-----------------------------------------------------------------------------
  (defgeneric compute-slot-writer
    ((cl <class>) (slotd <slot>) effective-slotds))

  (defmethod compute-slot-writer
    ((cl <class>) (slotd <slot>) effective-slotds)
    (generic-lambda ((obj cl) val)))

  (defmethod compute-slot-writer
    ((cl <class>) (slotd <local-slot>) effective-slotds)
    ;; Accessors are simple functions
    (let ((name (slot-name slotd)))
      (labels
          ((loop (n l)
                 (if (eq name (slot-name (car l)))
                     n
                   (loop (+ n 1) (cdr l)))))
        (let ((i (loop 0 effective-slotds)))
          (predefined-writer i)))))

;;;-----------------------------------------------------------------------------
;;; Ensure-slot-reader
;;;-----------------------------------------------------------------------------
  (defgeneric ensure-slot-reader
    ((cl <class>) (slotd <slot>) effective-slotds (reader <function>)))

  (defmethod ensure-slot-reader
    ((cl <class>) (slotd <slot>) effective-slotds (reader <generic-function>))
    ;; If there is a method, assume it's OK
    (if (generic-function-methods reader) ()
      (let ((primitive-reader
             (compute-primitive-reader-using-slot
              slotd cl effective-slotds)))
        (add-method reader
                    (method-lambda
                     class: (generic-function-method-class reader)
                     ((obj cl))
                     (primitive-reader obj)))))
    reader)

  (defmethod ensure-slot-reader
    ((cl <class>) (slotd <local-slot>) effective-slotds (reader <function>))
    reader)

  (defgeneric compute-primitive-reader-using-slot
    ((slotd <slot>) (cl <class>) effective-slotds))

  (defmethod compute-primitive-reader-using-slot
    ((slotd <slot>) (cl <class>) effective-slotds)
    (compute-primitive-reader-using-class cl slotd effective-slotds))

  (defgeneric compute-primitive-reader-using-class
    ((cl <class>) (slotd <slot>) effective-slotds))

  (defmethod compute-primitive-reader-using-class
    ((cl <class>) (slotd <slot>) effective-slotds)
    ;; Search on readers rather than names
    (let ((reader (slot-reader slotd)))
      (labels
          ((loop (n slots)
                 (if (eq reader (slot-reader (car slots)))
                     n
                   (loop (+ n 1) (cdr slots)))))
        (let ((i (loop 0 effective-slotds)))
          (predefined-reader i)))))

;;;-----------------------------------------------------------------------------
;;; Ensure-slot-writer
;;;-----------------------------------------------------------------------------
  (defgeneric ensure-slot-writer
    ((cl <class>) (slotd <slot>) effective-slotds (writer <function>)))

  (defmethod ensure-slot-writer
    ((cl <class>) (slotd <slot>) effective-slotds (writer <generic-function>))
    ;; If there is a method, assume it's OK
    (if (generic-function-methods writer) ()
      (let ((primitive-writer
             (compute-primitive-writer-using-slot
              slotd cl effective-slotds)))
        (add-method writer
                    (method-lambda
                     class: (generic-function-method-class writer)
                     ((obj cl) val)
                     (primitive-writer obj val)))))
    writer)

  (defmethod ensure-slot-writer
    ((cl <class>) (slotd <local-slot>) effective-slotds (writer <function>))
    writer)

  (defgeneric compute-primitive-writer-using-slot
    ((slotd <slot>) (cl <class>) effective-slotds))

  (defmethod compute-primitive-writer-using-slot
    ((slotd <slot>) (cl <class>) effective-slotds)
    (compute-primitive-writer-using-class cl slotd effective-slotds))

  (defgeneric compute-primitive-writer-using-class
    ((cl <class>) (slotd <slot>) effective-slotds))

  (defmethod compute-primitive-writer-using-class
    ((cl <class>) (slotd <slot>) effective-slotds)
    ;; Search on reader, rather than slot name.
    (let ((reader (slot-reader slotd)))
      (labels
          ((loop (n slots)
                 (if (eq reader (slot-reader (car slots)))
                     n
                   (loop (+ n 1) (cdr slots)))))
        (let ((i (loop 0 effective-slotds)))
          (predefined-writer i)))))

;;;-----------------------------------------------------------------------------
;;; Answer list of slot names of an instance
;;;-----------------------------------------------------------------------------
  (defun find-slot-names (obj)
    (let* ((class (class-of obj))
           (slots (class-slots class)))
      (map1-list (lambda (slot-descr)
             (slot-name slot-descr))
           slots)))

;;;-----------------------------------------------------------------------------
;;; Pretty print function
;;; Size of simple functions is not nslots but bytevector size in bytes!
;;;-----------------------------------------------------------------------------
  (defun spprint (s x)
    (if (object? x)
        (let* ((cl (class-of x)))
          (format1 s "\nInstance ~a of class #<~a>" x (class-name cl))
          (do1-list (lambda (slot)
                      (let ((name (slot-name slot))
                            (value ((slot-reader slot) x)))
                        (format1 s "\n  ~a = ~a" name value)))
                    (class-slots cl))
          (format1 s "\n"))
      (sprint s x)))

  (defun pprint (x)
    (spprint stdout x))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
