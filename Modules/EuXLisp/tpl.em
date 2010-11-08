;;; Copyright 1995 Russell Bradford
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'EuXLisp'
;;;-----------------------------------------------------------------------------
;;
;;  EuXLisp is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  EuXLisp is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: A tiny paralation lisp
;;;  Authors: Russell Bradford
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    Probably originally written by Simon Merrall.
;;    EuSchemed by RJB, Sept 95.
;;;-----------------------------------------------------------------------------

(defmodule tpl
  (import (level-0
           tpl-base)
   export (<field>
           paralation
           index
           <mapping>
           elwise
           make-paralation
           match
           move
           inverse
           field-ref
           field-length
           vref
           shape
           shape-class
           attributes
           field-get
           field-put
           fieldp))

(defun make-paralation (size)
  (let ((new-paralation (make-paralation-internal
                         size: size
                         index: (make-index size))))
    ((setter paralation) (index-internal new-paralation) new-paralation)
    (copy-field (index-internal new-paralation))))

(defun make-index (size)
  (let* ((v (make-vector size 0))
         (new-field (make-field value: v)))
    (letfuns
     ((init-field-value (i)
                        (if (< i 0) nil
                          (progn
                            ((setter vector-ref) v i i)
                            (init-field-value (- i 1))))))
     (init-field-value (- size 1)))
    new-field))

(defun match (from to)
  (make-mapping
   from-key: (copy-field from)
   to-key: (copy-field to)))

(defun move (field map combiner default)
  (let ((table (make-table eql)))
    (keys-to-table (from-key map) field table)
    (table-to-field (to-key map) table combiner default)))

(defun keys-to-table (key data table)
  (elwise (key data)
          ((setter table-ref) table key (cons data (table-ref table key)))))

(defun table-to-field (key table combiner default)
  (elwise (key)
          (let ((val (table-ref table key)))
            (cond ((atom? val) default)
                  ((null? (cdr val)) (car val))
                  (t (apply combiner val))))))

(defun index (field)
  (copy-field (index-internal (paralation field))))

(defun inverse (map)
  (make-mapping
   from-key: (to-key map)
   to-key: (from-key map)))

(defun field-ref (field i)
  (vector-ref (value field) i))

(defun (setter field-ref) (field i val)
  ((setter vector-ref) (value field) i val))

(defun field-length (field)
  (p-size (paralation field)))

(defun vref (field fn)
  (field-ref
   (move field (match (elwise (field) 0) (make-paralation 1)) fn nil) 0))

(defun shape (field) (shape-internal (paralation field)))

(defun (setter shape) (field value)
  ((setter shape-internal) (paralation field) value)
  field)

(defun shape-class (field)
  (class-of (field-ref (shape field) 0)))

(defun attributes (field) (attr (paralation field)))

(defun (setter attributes) (field value)
  ((setter attr) (paralation field) value)
  field)

(defun field-get (accesor field default)
  (let* ((g-field (shape field))
         (get-key (elwise (g-field)
                          (accesor g-field))))
    (move field (match (index-internal (paralation field)) get-key)
          cons default)))

(defun field-put (accessor field default)
  (let* ((g-field (shape field))
         (get-key (elwise (g-field)
                          (accessor g-field))))
    (move field (match get-key (index-internal (paralation field)))
          cons default)))

(defun fieldp (object)
  (subclass? (class-of object) field))

;; (defun inverse (map)
;;   (let ((p (index (from-key map))))
;;     (match (move p map list nil) p)))

;;;-----------------------------------------------------------------------------
)  ;; End of module tpl
;;;-----------------------------------------------------------------------------
