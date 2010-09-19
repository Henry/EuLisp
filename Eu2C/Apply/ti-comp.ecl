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
;;; Title: Inference with Compound Lattice Types (fpi-list, sy-list, cons-list ..)
;;;  Description:
;;    We distinguish monomorphic lists (mono-lists) and polymorphic lists
;;    (poly-lists). Monomorphic lists contain elements with the same type,
;;    for example <symbol>, <fpi> or <cons>.
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

#module ti-comp
(import (mzs
         ti
         ti-lattice
         ti-exprs
         ti-eqs
         ti-meet-join
         ti-write
         (only (format)
               common-lisp))
 syntax (ti)
 export (check-compound-types-before
         check-compound-types-after
         convert-all-compound-types
         reset-write-access-stam?s))

;;;-----------------------------------------------------------------------------
;;; Check compound types and convert them to uncompound ones
;;; probably.
;;;-----------------------------------------------------------------------------
;; Copy all compound lattice types of a descriptor and set their write-access-
;; stamp in case it is () to the latest write-access-stamp.
;; See also comment for check-compound-types-before.
(defgeneric check-compound-types-after (descr))

(defmethod check-compound-types-after ((descr <type-descr>))
  (let ((no-type-clash t))
    (dolist (equ (?equations (?type-vars descr)))
            (let ((expr (?right-expr equ)))
              (if (and no-type-clash (atomic-type? expr))
                  (if (?comp-name expr)
                      (let ((new-name (check-compound-types-after
                                       (?name expr))))
                        (if new-name
                            (let ((new-atom (compute-to-atom new-name)))
                              (if new-atom
                                  (set-right-expr equ new-atom)
                                (setq no-type-clash ())))))))))
    (if (null? no-type-clash)
        (ti-format t "~%Notice: type descriptor removed (after): ~A"
                   (ti-print-string descr))
      no-type-clash)))

(defmethod check-compound-types-after ((name <pair>))
  (let ((op-symbol (car name)))
    (cond ((eq op-symbol ^not)
           (let* ((subname (car (cdr name)))
                  (new-subname (check-compound-types-after subname)))
             (if new-subname
                 (list ^not new-subname))))
          ((eq op-symbol ^or)
           (let* ((subname1 (car (cdr name)))
                  (subname2 (car (cdr (cdr name))))
                  (new-subname1 (check-compound-types-after subname1))
                  (new-subname2 (check-compound-types-after subname2)))
             (if (or new-subname1 new-subname2)
                 (list ^or
                       (or new-subname1 subname1)
                       (or new-subname2 subname2)))))
          ((eq op-symbol ^and)
           (let* ((subname1 (car (cdr name)))
                  (subname2 (car (cdr (cdr name))))
                  (new-subname1 (check-compound-types-after subname1))
                  (new-subname2 (check-compound-types-after subname2)))
             (if (or new-subname1 new-subname2)
                 (list ^and
                       (or new-subname1 subname1)
                       (or new-subname2 subname2))))))))

(defmethod check-compound-types-after ((lattice-type <lattice-type>))
  (if (?compound lattice-type)
      (let ((new-lattice-type (copy-lattice-type lattice-type)))
        (if (null? (?write-access-stamp new-lattice-type))
            (setf (?write-access-stamp new-lattice-type)
                  (new-write-access-stamp)))
        (format t "~%after: outcomming stamp: ~A min-stamp: ~A new stamp: ~A latest-stamp: ~A - ~A"
                (?write-access-stamp lattice-type)
                (?write-access-stamp
                 (get-strategic-lattice-type (?name lattice-type)))
                (?write-access-stamp new-lattice-type)
                (?latest-write-access-stamp *the-lattice*)
                (?name lattice-type))
        new-lattice-type)
    ()))

;; Convert all compound lattice types of a descritor that may be affected
;; be accessing operations on the itself or a super lattice type to the next
;; super non-compound lattice type. The conversion is done when the write-
;; access-stamp of the actual lattice type is less than the write-access-
;; stamp of the origin lattice type in the type lattice (*the-lattice*).
;; All compound lattice type have thus to be copied when after an inference
;; step (see check-compound-types-after).
(defgeneric check-compound-types-before (descr))

(defmethod check-compound-types-before ((descr <type-descr>))
  (let ((eqs (append (?equations (?type-vars descr))
                     (?equations (?type-vars (?t-descr-before descr)))))
        (no-type-clash t))
    (dolist (equ eqs)
            (let ((expr (?right-expr equ)))
              (if (and no-type-clash (atomic-type? expr))
                  (if (?comp-name expr)
                      (let ((new-name (check-compound-types-before (?name expr))))
                        (if new-name
                            (let ((new-atom (compute-to-atom new-name)))
                              (if new-atom
                                  (set-right-expr equ (compute-to-atom new-name))
                                (setq no-type-clash ())))))))))
    (if (null? no-type-clash)
        (ti-format t "~%Notice: type descriptor removed (before): ~A"
                   (ti-print-string descr))
      no-type-clash)))

(defmethod check-compound-types-before ((name <pair>))
  (let ((op-symbol (car name)))
    (cond ((eq op-symbol ^not)
           (let* ((subname (car (cdr name)))
                  (new-subname (check-compound-types-before subname)))
             (if new-subname
                 (list ^not new-subname))))
          ((eq op-symbol ^or)
           (let* ((subname1 (car (cdr name)))
                  (subname2 (car (cdr (cdr name))))
                  (new-subname1 (check-compound-types-before subname1))
                  (new-subname2 (check-compound-types-before subname2)))
             (if (or new-subname1 new-subname2)
                 (list ^or
                       (or new-subname1 subname1)
                       (or new-subname2 subname2)))))
          ((eq op-symbol ^and)
           (let* ((subname1 (car (cdr name)))
                  (subname2 (car (cdr (cdr name))))
                  (new-subname1 (check-compound-types-before subname1))
                  (new-subname2 (check-compound-types-before subname2)))
             (if (or new-subname1 new-subname2)
                 (list ^and
                       (or new-subname1 subname1)
                       (or new-subname2 subname2))))))))

(defmethod check-compound-types-before ((lattice-type <lattice-type>))
  (if (?compound lattice-type)
      (let ((min-stamp (?write-access-stamp ; get the original lattice type
                        (get-strategic-lattice-type (?name lattice-type)))))
        (format t "~%before: incomming stamp: ~A min-stamp: ~A latest-stamp: ~A - ~A"
                (?write-access-stamp lattice-type)
                min-stamp
                (?latest-write-access-stamp *the-lattice*)
                (?name lattice-type))
        (if (< (?write-access-stamp lattice-type) min-stamp)
            (convert-to-super-non-compound-type lattice-type)
          ()))
    ()))

;; Convert all comound lattice types of a descriptor on next super non-
;; compound lattice types. This function is used to ignore all compound
;; lattice types.
(defgeneric convert-all-compound-types (descr))

(defmethod convert-all-compound-types ((descr <type-descr>))
  (let ((no-type-clash t))
    (dolist (equ (?equations (?type-vars descr)))
            (let ((expr (?right-expr equ)))
              (if (and no-type-clash (atomic-type? expr))
                  (let ((new-name (convert-all-compound-types (?name expr))))
                    (if new-name
                        (let ((new-name (compute-to-atom new-name)))
                          (if new-name
                              (set-right-expr equ new-name)
                            (setq no-type-clash new-name))))))))
    (if (null? no-type-clash)
        (ti-format t "~%Notice: type descriptor removed (convert): ~A"
                   (ti-print-string descr))
      no-type-clash)))

(defmethod convert-all-compound-types ((name <pair>))
  (let ((op-symbol (car name)))
    (cond ((eq op-symbol ^not)
           (let* ((subname (car (cdr name)))
                  (new-subname (convert-all-compound-types subname)))
             (if new-subname
                 (list ^not new-subname))))
          ((eq op-symbol ^or)
           (let* ((subname1 (car (cdr name)))
                  (subname2 (car (cdr (cdr name))))
                  (new-subname1 (convert-all-compound-types subname1))
                  (new-subname2 (convert-all-compound-types subname2)))
             (if (or new-subname1 new-subname2)
                 (list ^or
                       (or new-subname1 subname1)
                       (or new-subname2 subname2)))))
          ((eq op-symbol ^and)
           (let* ((subname1 (car (cdr name)))
                  (subname2 (car (cdr (cdr name))))
                  (new-subname1 (convert-all-compound-types subname1))
                  (new-subname2 (convert-all-compound-types subname2)))
             (if (or new-subname1 new-subname2)
                 (list ^and
                       (or new-subname1 subname1)
                       (or new-subname2 subname2))))))))

(defmethod convert-all-compound-types ((lattice-type <lattice-type>))
  (if (?compound lattice-type)
      (convert-to-super-non-compound-type lattice-type)
    ()))

;; Set write-access-stamps of all lattice type of a descriptor to nil.
(defgeneric reset-write-access-stam?s (obj))

(defmethod reset-write-access-stam?s ((descr <type-descr>))
  (dolist (equ (?equations (?type-vars descr)))
          (let ((expr (?right-expr equ)))
            (if (atomic-type? expr)
                (reset-write-access-stam?s (?name expr))))))

(defmethod reset-write-access-stam?s ((name <pair>))
  (let ((op-symbol (car name)))
    (cond ((eq op-symbol ^not)
           (reset-write-access-stam?s (car (cdr name))))
          ((or (eq op-symbol ^or) (eq op-symbol ^and))
           (reset-write-access-stam?s (car (cdr name)))
           (reset-write-access-stam?s (car (cdr (cdr name))))))))

(defmethod reset-write-access-stam?s ((lattice-type <lattice-type>))
  (if (?compound lattice-type)
      (setf (?write-access-stamp lattice-type) ())))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
