;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;; Description: handling object allocation
;;;-----------------------------------------------------------------------------
(defmodule mop-alloc
  (syntax (_boot0 _mop-gf0 _mop-meth0)
   import (boot mop-prim mop-key mop-class mop-init mop-inspect mop-gf
           mop-meth mop-defcl mop-access)
   export (compute-slots compute-inherited-slots
           compute-defined-slot compute-defined-slot-class
           compute-specialized-slot compute-specialized-slot-class
           compute-keywords compute-inherited-keywords
           compatible-superclasses? compatible-superclass?
           compute-class-precedence-list))

;;;-----------------------------------------------------------------------------
;;; Allocate
;;;-----------------------------------------------------------------------------
  (defmethod allocate ((cl <class>) inits)
    (if (class-abstract? cl)
        (error "can't allocate an instance of abstract-class ~a" cl)
      (primitive-allocate cl (class-instance-length cl))))

;;;-----------------------------------------------------------------------------
;;; Initialize <object>
;;;-----------------------------------------------------------------------------
  (defmethod initialize ((obj <object>) keywords)
    ;; Initialize the slot values
    (labels
     ((loop (slots i)
        (if (null slots) ()
          (let* ((sd (car slots))
                 (key (slot-keyword sd))
                 (init (slot-default sd)))
            ((setter primitive-ref) obj i
             (if (null key)
                 (if (functionp init)
                     (init)
                   init)
               (let ((x (find-key key keywords *absent*)))
                 (if (eq x *absent*)
                     (if (slot-required? sd)
                         (warning "missing keyword ~a to make ~a"
                          key (class-name (class-of obj)))
                       (if (functionp init)
                           (init)
                         init))
                   x))))
            (loop (cdr slots) (+ i 1))))))
     (let ((cl (class-of obj)))
       (check-keywords obj cl keywords)
       (loop (class-slots cl) 0)
       obj)))

;;;-----------------------------------------------------------------------------
;;; Check initialization keywords
;;;-----------------------------------------------------------------------------
  (defun check-keywords (obj cl init-keys)
    (let ((class-keys (class-keywords cl)))
      (labels
       ((loop (l)
              (if (null l) t
                (let ((x (car l)))
                  (if (member1-list x class-keys)
                      (loop (cdr (cdr l)))
                    (warning
                     "unexpected keyword ~a in initialization of ~a"
                     x obj))))))
       (loop init-keys))))

;;;-----------------------------------------------------------------------------
;;; Initialize <class>
;;;-----------------------------------------------------------------------------
  (defmethod initialize ((cl <class>) keywords)
    (call-next-method)
    ;; Add the class to the class hierarchy
    (let ((direct-supers (class-direct-superclasses cl))
          (direct-slotds (find-key direct-slots: keywords ()))
          (direct-keys (find-key direct-keywords: keywords ())))
      (if (compatible-superclasses? cl direct-supers) ()
          (error "~a can not be a subclass of ~a" cl direct-supers))
      ((setter class-precedence-list) cl
       (compute-class-precedence-list cl direct-supers))
      ((setter class-keywords) cl
       (compute-keywords cl direct-keys
                         (compute-inherited-keywords cl direct-supers)))
      (let* ((inherited-slotds (compute-inherited-slots cl direct-supers))
             (effective-slotds
              (compute-and-ensure-slot-accessors
               cl (compute-slots cl direct-slotds inherited-slotds)
               inherited-slotds)))
        ((setter class-slots) cl effective-slotds)
        ((setter class-instance-length) cl (list-size effective-slotds)))
      (do1-list (lambda (super) (add-subclass super cl)) direct-supers))
    (compute-class-codes)
    cl)

;;;-----------------------------------------------------------------------------
;;; Compatible superclasses
;;;-----------------------------------------------------------------------------
  (defgeneric compatible-superclasses? ((cl <class>) superclasses))
  (defmethod compatible-superclasses? ((cl <class>) superclasses)
    (compatible-superclass? cl (car superclasses)))

  (defgeneric compatible-superclass? ((cl <class>) (superclass <class>)))
  (defmethod compatible-superclass? ((cl <class>) (super <class>))
    (if (class-abstract? super) t
        (subclassp (class-of cl) (class-of super))))

;;;-----------------------------------------------------------------------------
;;; Compute class precedence list
;;;-----------------------------------------------------------------------------
  (defgeneric compute-class-precedence-list ((cl <class>) direct-supers))
  (defmethod compute-class-precedence-list ((cl <class>) direct-supers)
    (cons cl (class-precedence-list (car direct-supers))))

;;;-----------------------------------------------------------------------------
;;; Compute inherited keywords
;;;-----------------------------------------------------------------------------
  (defgeneric compute-inherited-keywords ((cl <class>) direct-supers))
  (defmethod compute-inherited-keywords ((cl <class>) direct-supers)
    (list (class-keywords (car direct-supers))))

;;;-----------------------------------------------------------------------------
;;; Compute keywords
;;;-----------------------------------------------------------------------------
  (defgeneric compute-keywords ((cl <class>) direct-keys inherited-inits))
  (defmethod compute-keywords ((cl <class>) direct-keys inherited-inits)
    (list-remove-duplicates (append direct-keys (car inherited-inits))))

;;;-----------------------------------------------------------------------------
;;; Compute inherited slots
;;;-----------------------------------------------------------------------------
  (defgeneric compute-inherited-slots ((cl <class>) direct-supers))
  (defmethod compute-inherited-slots ((cl <class>) direct-supers)
    (list (class-slots (car direct-supers))))

;;;-----------------------------------------------------------------------------
;;; Compute slots
;;;-----------------------------------------------------------------------------
  (defgeneric compute-slots ((cl <class>) slotd-specs inherited-slotds))
  (defmethod compute-slots ((cl <class>) slotd-specs inherited-slotds)
    (let ((old-sd-names (map1-list slot-name (car inherited-slotds)))
          (new-sd-plist (mapcan (lambda (spec)
                                  (list (find-key name: spec required:) spec))
                                slotd-specs)))
      (append
       (map1-list
        (lambda (sd)
          (compute-specialized-slot
           cl (list sd) (init-list-ref new-sd-plist (slot-name sd))))
        (car inherited-slotds))
       (mapcan
        (lambda (spec)
          (if (member1-list (find-key name: spec required:)
                            old-sd-names)
              ()
            (list (compute-defined-slot cl spec))))
        slotd-specs))))

;;;-----------------------------------------------------------------------------
;;; Compute specialized slot
;;;-----------------------------------------------------------------------------
  (defgeneric compute-specialized-slot ((cl <class>) sds spec))
  (defmethod compute-specialized-slot ((cl <class>) sds spec)
    (let* ((sd (car sds))
           (sdclass (compute-specialized-slot-class cl sds spec)))
      (if (null spec)
          (inherited-slot cl sd sdclass)
        (redefined-slot cl sd sdclass spec))))

  (defun inherited-slot (cl sd sdclass)
    ;; Inherited, but not redefined.
    (if (eq sdclass (class-of sd))
        sd
      (make sdclass             ; what of other keywords?
            name: (slot-name sd)
            reader: (slot-reader sd)
            writer: (slot-writer sd)
            keyword: (slot-keyword sd)
            default: (slot-default sd))))

  (defun redefined-slot (cl sd sdclass spec)
    ;; Inherited and redefined
    (let* ((reader (find-key reader: spec (slot-reader sd)))
           (writer (find-key writer: spec (slot-writer sd)))
           (init-fun (find-key default: spec (slot-default sd)))
           (name (find-key name: spec required:))
           (keyword (find-key keyword: spec
                              (let ((ia (slot-keyword sd)))
                                (if ia ia
                                  (if (member1-list name (class-keywords cl))
                                      name
                                    ()))))))
      (apply make sdclass
             reader: reader
             writer: writer
             keyword: keyword
             default: init-fun
             (filter-keywords spec '(reader: writer: keyword: default:)))))

;;;-----------------------------------------------------------------------------
;;; Compute specialized slot class
;;;-----------------------------------------------------------------------------
  (defgeneric compute-specialized-slot-class ((cl <class>) sds spec))
  (defmethod compute-specialized-slot-class ((cl <class>) sds spec)
    <local-slot>)

;;;-----------------------------------------------------------------------------
;;; Compute defined slot
;;;-----------------------------------------------------------------------------
  (defgeneric compute-defined-slot ((cl <class>) spec))
  (defmethod compute-defined-slot ((cl <class>) spec)
    (let* ((name (find-key name: spec required:))
           (key (find-key keyword: spec
                          (if (member1-list name (class-keywords cl))
                              name
                            ()))))
      (apply make
             (compute-defined-slot-class cl spec)
             keyword: key
             (filter-keywords spec '(keyword:)))))

;;;-----------------------------------------------------------------------------
;;; Compute defined slot class
;;;-----------------------------------------------------------------------------
  (defgeneric compute-defined-slot-class ((cl <class>) spec))
  (defmethod compute-defined-slot-class ((cl <class>) spec)
    <local-slot>)

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
