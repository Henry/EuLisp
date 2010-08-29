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
;;;  Title: standard module copy
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

(defmodule copy
  (import (tail
           object-0-i)
   syntax (tail
           object-0-i
           setf)
   export (deep-copy
           shallow-copy)
   )

;;;-----------------------------------------------------------------------------
;;; deep-copy
;;;-----------------------------------------------------------------------------
(defgeneric deep-copy (object))

(defmethod deep-copy (object)
  object)

(defmethod deep-copy ((object <structure>))
  (deep-copy-initialize
   (allocate (%class-of object))
   object
   (%select (%class-of object) <class> slot-descriptions)))

;; The following variant uses slot readers and slot writers for initialization
;; (defun deep-copy-initialize (new-object old-object slots)
;;   (if (null slots) new-object
;;     (progn
;;       ((slot-description-slot-writer (car slots))
;;        new-object
;;        (deep-copy
;;         ((slot-description-slot-reader (car slots)) old-object)))
;;       (deep-copy-initialize new-object old-object
;;                             (cdr slots)))))

(defun deep-copy-initialize (new-object old-object slots)
  (deep-copy-init (%cast <instance-as-vector> new-object)
                  (%cast <instance-as-vector> old-object)
                  slots #%I0))

(%define-function (deep-copy-init <object>)
  ((new-object <instance-as-vector>)
   (old-object <instance-as-vector>)
   (slots <list>) (slot-index %unsigned-word-integer))
  (if (null slots) (%cast <object> new-object)
    (progn
      (set-nth-slot-value
       new-object slot-index
       (deep-copy
        (get-nth-slot-value old-object slot-index)))
      (deep-copy-init new-object old-object
                      (cdr slots) (%plus #%I1 slot-index)))))

;;;-----------------------------------------------------------------------------
;;; shallow-copy
;;;-----------------------------------------------------------------------------
(defgeneric shallow-copy (object))

(defmethod shallow-copy (object)
  object)

(defmethod shallow-copy ((object <structure>))
  (shallow-copy-initialize
   (allocate (%class-of object))
   object
   (%select (%class-of object) <class> slot-descriptions)))

;; The following variant uses slot readers and slot writers for initialization
;; (defun shallow-copy-initialize (new-object old-object slots)
;;   (if (null slots) new-object
;;     (progn
;;       ((slot-description-slot-writer (car slots))
;;        new-object
;;        ((slot-description-slot-reader (car slots)) old-object))
;;       (shallow-copy-initialize new-object old-object
;;                                (cdr slots)))))

(defun shallow-copy-initialize (new-object old-object slots)
  (shallow-copy-init (%cast <instance-as-vector> new-object)
                     (%cast <instance-as-vector> old-object)
                     slots #%I0))

(%define-function (shallow-copy-init <object>)
  ((new-object <instance-as-vector>)
   (old-object <instance-as-vector>)
   (slots <list>) (slot-index %unsigned-word-integer))
  (if (null slots) (%cast <object> new-object)
    (progn
      (set-nth-slot-value new-object slot-index
                          (get-nth-slot-value old-object slot-index))
      (shallow-copy-init new-object old-object
                         (cdr slots) (%plus #%I1 slot-index)))))

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------
(%annotate-function deep-copy new-signature
  (((var0 var1)
    ((var var0) (atom <object>))
    ((var var1) (var var0)))))

(%annotate-function shallow-copy new-signature
  (((var0 var1)
    ((var var0) (atom <object>))
    ((var var1) (var var0)))))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
