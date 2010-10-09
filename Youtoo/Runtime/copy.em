;;; Copyright 1997 A. Kind & University of Bath
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;
;;  Youtoo is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: generic copying
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule copy
  (syntax (_telos0)
   import (telos
           thread
           condition)
   export (shallow-copy
           deep-copy))

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
)  ;; End of module copy
;;;-----------------------------------------------------------------------------
