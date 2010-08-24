;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;; Description: generic copying
;;;-----------------------------------------------------------------------------
(defmodule copy
  (syntax (_telos0)
   import (telos thread condition)
   export (shallow-copy deep-copy))

;;;-----------------------------------------------------------------------------
;;; Shallow copy
;;;-----------------------------------------------------------------------------
(defgeneric shallow-copy (x))

(defmethod shallow-copy ((x <class>)) x)
(defmethod shallow-copy ((x <function>)) x)
;;(defmethod shallow-copy ((x <stream>)) x)
(defmethod shallow-copy ((x <thread>)) x)
(defmethod shallow-copy ((x <name>)) x)
(defmethod deep-copy ((x <name>)) x)
(defmethod shallow-copy ((x <object>))
  (let ((cl (class-of x)))
    (let ((x (allocate cl ())))
      (labels
       ((loop (sds)
              (if (null? sds) x
                (let ((sd (car sds)))
                  ((setter slot-value-using-slot) sd x
                   (slot-value-using-slot sd x))))))
       (loop (class-slots cl))))))

;;;-----------------------------------------------------------------------------
;;; Deep copy
;;;-----------------------------------------------------------------------------
(defgeneric deep-copy (x))

(defmethod deep-copy ((x <class>)) x)
(defmethod deep-copy ((x <function>)) x)
;;(defmethod deep-copy ((x <stream>)) x)
(defmethod deep-copy ((x <thread>)) x)

(defmethod deep-copy ((x <object>))
  (let ((cl (class-of x)))
    (let ((x (allocate cl ())))
      (labels
       ((loop (sds)
              (if (null? sds) x
                (let ((sd (car sds)))
                  ((setter slot-value-using-slot) sd x
                   (deep-copy (slot-value-using-slot sd x)))))))
       (loop (class-slots cl))))))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
