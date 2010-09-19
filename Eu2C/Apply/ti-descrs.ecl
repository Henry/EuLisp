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
;;; Title: Default Type Descriptors
;;;  Description:
;;    Type schemes (signatures) describe the range and domain of a
;;    function. Type schemes are generic, i.e. they may have more than one
;;    line (descriptor). This file provides functions to create default type
;;    descriptors.
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

#module ti-descrs
(import (lzs
         lzs-mop
         mzs
         ti
         ti-exprs
         ti-eqs
         (only (make-array
                vector
                append)
               common-lisp))
 syntax (ti)
 export (filled-formal-descr
         filled-recursive-descr
         filled-actual-descr
         general-var-formal-descr
         general-var-recursive-descr
         %object-var-formal-descr
         general-var-actual-descr
         range&domain-descr
         range&domain-as-signature-p
         empty-formal-descr
         empty-recursive-descr
         empty-actual-descr))

;;;-----------------------------------------------------------------------------
;;; DEFAULT TYPE DESCRIPTORS
;;;-----------------------------------------------------------------------------

;; Answer a formal type descriptor filled with the given expressions.
(defun filled-formal-descr exprs
  (fill-descr (make <formal-type-descr>
                    :type-vars (make <type-var-substitutions>)
                    :type-vec (make-array (length exprs))
                    :stat ()
                    :t-descr-before ()
                    :type-spec 0)
              exprs 0))

;; Answer a recursive type descriptor filled with the given expressions.
(defun filled-recursive-descr exprs
  (fill-descr (make <recursive-type-descr>
                    :type-vars (make <type-var-substitutions>)
                    :type-vec (make-array (length exprs))
                    :stat ()
                    :t-descr-before ()
                    :type-spec 0)
              exprs 0))

;; Answer an actual type descriptor filled with the given expressions.
(defun filled-actual-descr exprs
  (fill-descr (make <act-type-descr>
                    :type-vars (make <type-var-substitutions>)
                    :type-vec (make-array (length exprs))
                    :stat ()
                    :t-descr-before ()
                    :type-spec 0)
              exprs 0))

;;;-----------------------------------------------------------------------------
;;; Fill type vector and substitutions of a type descriptor.
;;;-----------------------------------------------------------------------------
(defun fill-descr (descr ;;<type-descr>
                   exprs
                   index);;<spint>
  (let ((vec (?type-vec descr))
        (subs (?type-vars descr)))
    (if (< index (length vec))
        (let* ((new-var (new-type-var))
               (new-equ (new-type-equation new-var (car exprs))))
          (setf (vector-ref vec index) new-var)
          (push-type-equation subs new-equ)
          (fill-descr descr (cdr exprs) (+ index 1)))
      descr)))

;; Fill a type descriptor up to arity with general types.
(defun fill-descr-to (descr             ;<type-descr>
                      arity)            ;<single-precision-integer>
  (let ((vec (?type-vec descr))
        (subs (?type-vars descr)))
    (if (>= arity 0)
        (let* ((new-var (new-type-var))
               (new-equ (new-type-equation new-var (general-type))))
          (setf (vector-ref vec arity) new-var)
          (push-type-equation subs new-equ)
          (fill-descr-to descr (- arity 1)))
      descr)))

;; Fill a type descriptor up to arity with %object types.
(defun fill-descr-to-with-%object (descr  ;<type-descr>
                                   arity) ;<single-precision-integer>
  (let ((vec (?type-vec descr))
        (subs (?type-vars descr)))
    (if (>= arity 0)
        (let* ((new-var (new-type-var))
               (new-equ (new-type-equation new-var (%object-type))))
          (setf (vector-ref vec arity) new-var)
          (push-type-equation subs new-equ)
          (fill-descr-to-with-%object descr (- arity 1)))
      descr)))

;; Answer a formal type descriptor that is filled with general types.
(defun general-var-formal-descr (arity)
  (let ((descr (make <formal-type-descr>
                     :type-vars (make <type-var-substitutions>)
                     :type-vec (make-array (+ arity 1))
                     :stat ()
                     :t-descr-before ()
                     :type-spec 0)))
    (fill-descr-to descr arity)))

;; Answer a formal type descriptor that is filled with general types.
(defun %object-var-formal-descr (arity)
  (let ((descr (make <formal-type-descr>
                     :type-vars (make <type-var-substitutions>)
                     :type-vec (make-array (+ arity 1))
                     :stat ()
                     :t-descr-before ()
                     :type-spec 0)))
    (fill-descr-to-with-%object descr arity)))

;; Answer a recursive type descriptor that is filled with general types.
(defun general-var-recursive-descr (arity)
  (let ((descr (make <recursive-type-descr>
                     :type-vars (make <type-var-substitutions>)
                     :type-vec (make-array (+ arity 1))
                     :stat ()
                     :t-descr-before ()
                     :type-spec 0)))
    (fill-descr-to descr arity)))

;; Answer an actual type descriptor that is filled with general types.
(defun general-var-actual-descr (arity)
  (let ((descr (make <act-type-descr>
                     :type-vars (make <type-var-substitutions>)
                     :type-vec (make-array (+ arity 1))
                     :stat ()
                     :t-descr-before ()
                     :type-spec 0)))
    (fill-descr-to descr arity)))

;; Answer an empty formal type descriptor.
(defun empty-formal-descr (arity)
  (make <formal-type-descr>
        :type-vars (make <type-var-substitutions>)
        :type-vec (make-array (+ arity 1))
        :stat ()
        :t-descr-before ()
        :type-spec 0))

;; Answer an empty recursive type descriptor.
(defun empty-recursive-descr (arity)
  (make <recursive-type-descr>
        :type-vars (make <type-var-substitutions>)
        :type-vec (make-array (+ arity 1))
        :stat ()
        :t-descr-before ()
        :type-spec 0))

;; Answer an empty actual type descriptor.
(defun empty-actual-descr (arity)
  (make <act-type-descr>
        :type-vars (make <type-var-substitutions>)
        :type-vec (make-array (+ arity 1))
        :stat ()
        :t-descr-before ()
        :type-spec 0))

;; Answer a formal descriptor of the range and domain vector.
(defun range&domain-descr (fun)
  (let ((vec (?range-and-domain fun))
        (exprs ()))
    (dovector (class i vec)
              (cl:declare (cl:ignore i))
              (setq exprs (append exprs (list (class-as-type-expr class)))))
    (apply #'filled-formal-descr exprs)))

;; Answer whether range-and-domain can be directly used for the signature.
(defun range&domain-as-signature-p (fun)
  (and (?range-and-domain fun)
       (null? (?signature fun))
       (null? (defined-generic-fun? fun))
       (null? (eq %object (vector-ref (?range-and-domain fun) 0)))))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
