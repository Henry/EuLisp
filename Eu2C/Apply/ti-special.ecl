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
;;; Title: Special Inference with some System Functions (%select, %extract ..)
;;;  Description:
;;    This file provides functions that compute formal type descriptors for some
;;    system functions, that are treated specially.
;;;  Authors: Andreas Kind
;;;-----------------------------------------------------------------------------

#module ti-special
(import (mzs
         lzs
         lzs-mop
         representation ; whc-classes
         tail-module
         standard-init
         ti
         ti-exprs
         ti-eqs
         ti-meet-join
         ti-write
         ti-signature
         ti-descrs
         ti-lattice
         (only (format)
               common-lisp))
 syntax (ti)
 export (formal-descrs-get-slot-value
         formal-descrs-set-slot-value
         convert-to-formal-descrs-%extract
         convert-to-formal-descrs-%setf-extract
         convert-to-formal-descrs-%funcall
         valid-for-then?
         valid-for-else?))

;;;-----------------------------------------------------------------------------
;;; CREATING FORMAL SIGNATURES FOR SOME TAIL FUNCTIONS
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;; Generate a formal descriptor for %select
;;;-----------------------------------------------------------------------------

(defun formal-descrs-get-slot-value (descrs slot)
  (mapcar (lambda (descr)
            (formal-descr-get-slot-value descr slot))
          descrs))

;;; Create a formal descr to get slot value.
(defun formal-descr-get-slot-value (descr slot)
  descr ;;***HGW not used
  (let* ((struct-class (?slot-of slot))
         (struct-type (class-as-type-expr struct-class))
         (slot-value-class (?type slot))
         (slot-value-type (class-as-type-expr slot-value-class)))
    ;;    (format t "~%Notice: %select with type ~A for slot ~A in ~A"
    ;;          (ti-print-string-no-cr slot-value-type)
    ;;          (?identifier slot)
    ;;          (ti-print-string-no-cr struct-type))
    (filled-formal-descr slot-value-type struct-type)))

;;(defun convert-to-formal-descrs-%select (descrs)
;;  (mapcar #'convert-to-formal-descr-%select descrs))

;;; Create a formal descr for %select using slot name.
;;(defun convert-to-formal-descr-%select (descr) ;<type-descr>
;;  (get-previous-subs descr)
;;  (let* ((struct-type (get-arg-type descr 1))
;;       (struct-class (type-expr-to-class struct-type ()))
;;       (slot-name-type (get-arg-type descr 2))
;;       (slot-name (convert-to-slot-name slot-name-type ()))
;;       (result-type (general-type)))
;;    (if (and slot-name
;;           (%pointer-to-struct-class? (?representation struct-class)))
;;      (setq result-type
;;        (or (find-slot-type-with-slot&struct slot-name struct-class)
;;            result-type))
;;      (if slot-name
;;        (let ((new-struct-class (car (find-classes-for-slot slot-name))))
;;          (setq struct-type (class-as-type-expr new-struct-class))
;;          (setq result-type
;;            (or (find-slot-type-with-slot&struct slot-name new-struct-class)
;;                result-type)))))
;;    (ti-format2 t "~%Notice: %select with type ~A for slot ~A in ~A"
;;             (ti-print-string-no-cr result-type)
;;             slot-name
;;             (ti-print-string-no-cr struct-type))
;;    (filled-formal-descr result-type struct-type slot-name-type)))

;;;-----------------------------------------------------------------------------
;;; Generate a formal descriptor for %setf-select
;;;-----------------------------------------------------------------------------

(defun formal-descrs-set-slot-value (descrs slot)
  (mapcar (lambda (descr)
            (formal-descr-set-slot-value descr slot))
          descrs))

;;; Create a formal descr to set slot value.
(defun formal-descr-set-slot-value (descr slot)
  (let* ((struct-class (?slot-of slot))
         (struct-type (class-as-type-expr struct-class))
         (slot-value-class (?type slot))
         (slot-value-type (class-as-type-expr slot-value-class))
         (slot-name (?identifier slot))
         (new-slot-value-type
          (meet-type-exprs slot-value-type (get-arg-type descr 2))))
    (if (null? new-slot-value-type)
        (setq new-slot-value-type slot-value-type))
    (if *use-compound-types*
        (check-write-access-stam? struct-type slot-name slot-value-type))
    ;;    (format t "~%Notice: %setf-select with type ~A for slot ~A in ~A"
    ;;          (ti-print-string-no-cr new-slot-value-type)
    ;;          slot-name
    ;;          (ti-print-string-no-cr struct-type))
    (filled-formal-descr new-slot-value-type struct-type new-slot-value-type)))

(defun check-write-access-stam? (struct-type slot-name slot-value-type)
  (if (fpi-list-type? struct-type)
      (progn
        (format t "~%struct: ~A, slot: ~A, value: ~A"
                (ti-print-string-no-cr struct-type)
                (ti-print-string-no-cr slot-name)
                (ti-print-string-no-cr slot-value-type))
        (if (or (and (eq slot-name ^car)
                     (<fpi>-type? slot-value-type))
                (and (eq slot-name ^cdr)
                     (or (fpi-list-type? slot-value-type)
                         (<null>-type? slot-value-type))))
            (format t "~%Notice: write access stamp NOT updated")
          (set-write-access-stam? (?lattice-type struct-type))))
    (set-write-access-stam? (?lattice-type struct-type))))

;;(defun convert-to-formal-descrs-%setf-select (descrs)
;;  (mapcar #'convert-to-formal-descr-%setf-select descrs))

;;; Create a formal descr for %setf-select using slot name.
;;(defun convert-to-formal-descr-%setf-select (descr) ;<type-descr>
;;  (get-previous-subs descr)
;;  (let* ((struct-type (get-arg-type descr 1))
;;       (struct-class (type-expr-to-class struct-type ()))
;;       (slot-name-type (get-arg-type descr 2))
;;       (slot-name (convert-to-slot-name slot-name-type ()))
;;       (new-slot-type (get-arg-type descr 3))
;;       result-type)
;;    (if (and slot-name
;;           (%pointer-to-struct-class? (?representation struct-class)))
;;      (setq new-slot-type
;;        (or (find-slot-type-with-slot&struct slot-name struct-class)
;;            new-slot-type))
;;      (if slot-name
;;        (let ((new-struct-class (car (find-classes-for-slot slot-name))))
;;          (setq struct-type (class-as-type-expr new-struct-class))
;;          (setq new-slot-type
;;            (or (find-slot-type-with-slot&struct slot-name new-struct-class)
;;                new-slot-type)))))
;;    (setq result-type new-slot-type)
;;    (if *use-compound-types*
;;      (check-write-access-stam?
;;       (compute-normalized-lattice-type (?name struct-type))
;;       slot-name
;;       (compute-normalized-lattice-type (?name new-slot-type))))
;;    (ti-format2 t "~%Notice: %setf-select with type ~A for slot ~A in ~A"
;;             (ti-print-string-no-cr new-slot-type)
;;             slot-name
;;             (ti-print-string-no-cr struct-type))
;;    (filled-formal-descr result-type struct-type slot-name-type new-slot-type)))

;;;-----------------------------------------------------------------------------
;;; Find The slot type giving slot name and structure
;;;-----------------------------------------------------------------------------

;;(defun find-slot-type-with-slot&struct (slot-name struct-class)
;;  (let ((slot-descr (~find-slot struct-class slot-name)))
;;    (if slot-descr
;;      (class-as-type-expr (~slot-type slot-descr))
;;      ())))

;;(defun find-slot-type-with-slot (slot-name)
;;  (let* ((struct-classes (find-classes-for-slot slot-name))
;;       (struct-types (mapcar #'class-as-type-expr struct-classes))
;;       (size (length struct-types)))
;;    (cond ((= size 0) ())
;;        ((= size 1)
;;         (let* ((slot-descr (~find-slot (car struct-classes)
;;                                                    slot-name))
;;                (slot-class (~slot-type slot-descr)))
;;           (if slot-class
;;               (filled-formal-descr (class-as-type-expr slot-class)
;;                                    (car struct-types)
;;                                    (general-type))
;;             ())))
;;        (t
;;         (ti-format2 t "~%Warning: is slot ~A defined twice?" slot-name)
;;         (filled-formal-descr (general-type)
;;                              (joined-type-exprs struct-types)
;;                              (general-type))))))

;;(defun find-classes-for-slot (slot-name)
;;  (let ((result-classes ()))
;;    (dolist (slot-descr *list-of-new-slots*)
;;      (if (eq slot-name (~slot-name slot-descr))
;;        (setq result-classes (cons (?slot-of slot-descr) result-classes))))
;;    result-classes))

;;;-----------------------------------------------------------------------------
;;; Generate a formal descriptor for %extract
;;;-----------------------------------------------------------------------------

(defun convert-to-formal-descrs-%extract (descrs)
  (mapcar #'convert-to-formal-descr-%extract descrs))

;;; Optimistic conversion: a class supposed to be available.
(defun convert-to-formal-descr-%extract (descr) ;<type-descr>
  (get-previous-subs descr)
  (let* ((vec-type (get-arg-type descr 1))
         (vec-class (type-expr-to-class vec-type ()))
         (index-type (%integer-type))
         (result-type (general-type)))
    (if (%pointer-to-vector-class? (?representation vec-class))
        (let ((elem-class (~vector-class-element-type vec-class)))
          (if elem-class
              (setq result-type (class-as-type-expr elem-class)))))
    (ti-format2 t "~%Notice: %extract with element type ~A in vector ~A"
                (ti-print-string-no-cr result-type)
                (ti-print-string-no-cr vec-type))
    (filled-formal-descr result-type vec-type index-type)))

;;;-----------------------------------------------------------------------------
;;; Generate a formal descriptor for %setf-extract
;;;-----------------------------------------------------------------------------

(defun convert-to-formal-descrs-%setf-extract (descrs)
  (mapcar #'convert-to-formal-descr-%setf-extract descrs))

;;; Optimistic conversion: a class supposed to be available.
(defun convert-to-formal-descr-%setf-extract (descr) ;<type-descr>
  (get-previous-subs descr)
  (let* ((vec-type (get-arg-type descr 1))
         (vec-class (type-expr-to-class vec-type ()))
         (index-type (%integer-type))
         (new-elem-type (get-arg-type descr 3))
         result-type)
    (if (%pointer-to-vector-class? (?representation vec-class))
        (let ((elem-class (~vector-class-element-type vec-class)))
          (if elem-class
              (setq new-elem-type (class-as-type-expr elem-class)))))
    (setq result-type new-elem-type)
    (ti-format2 t "~%Notice: %setf-extract with element type ~A in vector ~A"
                (ti-print-string-no-cr new-elem-type)
                (ti-print-string-no-cr vec-type))
    (filled-formal-descr result-type vec-type index-type new-elem-type)))

;;;-----------------------------------------------------------------------------
;;; Generate a formal descriptor for %class-of
;;;-----------------------------------------------------------------------------

;;(defun convert-to-formal-descrs-%class-of (descrs)
;;  (mapcar #'convert-to-formal-descr-%class-of descrs))
;;
;;(defgeneric convert-to-formal-descr-%class-of (descr))
;;
;;(defmethod convert-to-formal-descr-%class-of ((descr <type-descr>))
;;  (get-previous-subs descr)
;;  (let* ((obj-type (get-arg-type descr 1))
;;       (obj-class (type-expr-to-class obj-type ()))
;;       (obj-meta-class (~class-of obj-class))
;;       (result-type (class-as-type-expr obj-meta-class)))
;;    (ti-format2 t "~%Notice: %class-of with class ~A of object ~A"
;;             (ti-print-string-no-cr result-type)
;;             (ti-print-string-no-cr obj-type))
;;    (filled-formal-descr result-type obj-type)))

;;;-----------------------------------------------------------------------------
;;; Generate a formal descriptor for %instance-of?
;;;-----------------------------------------------------------------------------

;;(defun convert-to-formal-descrs-%instance-of? (descrs)
;;  (mapcar #'convert-to-formal-descr-%instance-of? descrs))
;;
;;(defgeneric convert-to-formal-descr-%instance-of? (descr))
;;
;;(defmethod convert-to-formal-descr-%instance-of? ((descr <type-descr>))
;;  (get-previous-subs descr)
;;  (let* ((obj-type (get-arg-type descr 1))
;;       (class-type (get-arg-type descr 2))
;;       (result-type (general-type)))
;;    (if (subtype-expr? obj-type class-type)
;;      (setq result-type (not-%false-type))
;;      (if (null? (meet-type-exprs? obj-type class-type))
;;        (setq result-type (%false-type))))
;;    (ti-format2 t "~%Notice: %instance-of? with object ~A, class ~A and result ~A"
;;             (ti-print-string-no-cr obj-type)
;;             (ti-print-string-no-cr class-type)
;;             (ti-print-string-no-cr result-type))
;;    (filled-formal-descr result-type obj-type class-type)))

;;;-----------------------------------------------------------------------------
;;; Generate a formal descriptor for %funcall
;;;-----------------------------------------------------------------------------

(defun convert-to-formal-descrs-%funcall (descrs)
  (let* ((arity (- (length (?type-vec (car descrs))) 1))
         (formal-descr (general-var-formal-descr arity)))
    (set-descr-type formal-descr 0 (%object-type))
    (set-descr-type formal-descr 1 (%function-type))
    (ti-format t "~%Notice: %funcall with formal descriptor ~A"
               (ti-print-string formal-descr))
    (list formal-descr)))

;;;-----------------------------------------------------------------------------

(defgeneric %pointer-to-vector-class? (x))

(defmethod %pointer-to-vector-class? (x)
  ())

(defmethod %pointer-to-vector-class? ((x <%pointer-to-vector>))
  x)

;;(defgeneric %pointer-to-struct-class? (x))
;;
;;(defmethod %pointer-to-struct-class? (x)
;;  ())
;;
;;(defmethod %pointer-to-struct-class? ((x <%pointer-to-struct>))
;;  x)

;;;-----------------------------------------------------------------------------
;;; DESCRIPTOR SELECTION FOR CONTROL STRUCTURES
;;;-----------------------------------------------------------------------------

;;; Answer whether descr is valid for then-case.
(defun valid-for-then? (fun            ;<fun>
                         descr)         ;<type-descr>
  (ti-format2 t "~%valid-for-then? ~A ~A"
              (?identifier fun)
              (ti-print-string descr))
  (let ((result (get-arg-type descr 0)))
    (let ((answer
           (if (general-type? result)
               (if (eq fun %eq)
                   (meet-type-exprs? (get-arg-type descr 1)
                                      (get-arg-type descr 2))
                 t)
             (null? (%false-type? result)))))
      (ti-format2 t " ~A" answer)
      answer)))

;;; Answer whether descr is valid for else-case.
(defun valid-for-else? (fun            ;<fun>
                         descr)         ;<type-descr>
  (ti-format2 t "~%valid-for-else? ~A ~A"
              (?identifier fun)
              (ti-print-string descr))
  (let ((result (get-arg-type descr 0)))
    (let ((answer
           (if (general-type? result)
               (if (eq fun %neq)
                   (meet-type-exprs? (get-arg-type descr 1)
                                      (get-arg-type descr 2))
                 t)
             (%false-type? result))))
      (ti-format2 t " ~A" answer)
      answer)))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
