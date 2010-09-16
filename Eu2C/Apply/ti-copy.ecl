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
;;; Title: Copying Type Inference Objects
;;;  Description:
;;    This module provides a generic function to copy all kinds of objects
;;    that are concerned during the type inference process.
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Andreas Kind
;;;-----------------------------------------------------------------------------


#module ti-copy
(import (mzs
         ti
         ti-lattice
         ti-write
         ti-exprs
         ti-eqs
         ti-meet-join
         (only (make-array copy-alist dotimes mapcar)
               common-lisp))
 syntax (ti)
 export (ti-copy-descr ti-copy&rename-descr
                       ti-copy-stack ti-copy&rename-stack
                       ti-copy-subs ti-copy&rename-subs
                       ti-copy&rename-equ ti-copy&rename-var
                       ti-copy-vec ti-copy&rename-vec
                       copy-descr-up-to application-subs))

;;;-----------------------------------------------------------------------------
;;; COPYING TYPE EQUATIONS
;;;-----------------------------------------------------------------------------

(defun ti-copy-stack (stack)
  (make <type-equation-stack>
        :equations (copy-alist (?equations stack))))

(defun ti-copy&rename-stack (stack new-vars . same-vars)
  (make <type-equation-stack>
        :equations
        (mapcar (lambda (equ)
                  (ti-copy&rename-equ equ new-vars (cons? same-vars)))
                (?equations stack))))

(defun ti-copy-subs (subs)
  (make <type-var-substitutions>
        :equations (copy-alist (?equations subs))))

(defun ti-copy&rename-subs (subs new-vars . same-vars)
  (make <type-var-substitutions>
        :equations (mapcar (lambda (equ)
                             (ti-copy&rename-equ equ new-vars (cons? same-vars)))
                           (?equations subs))))

(defun ti-copy&rename-equ (equ new-vars . same-vars)
  (let ((with-same-vars (cons? same-vars))
        (left-expr (?left-expr equ))
        (right-expr (?right-expr equ)))
    (cons (if (type-var? left-expr)
              (ti-copy&rename-var left-expr new-vars with-same-vars)
            left-expr)
          (if (type-var? right-expr)
              (ti-copy&rename-var (cdr equ) new-vars with-same-vars)
            right-expr))))

(defun ti-copy-vec (vec)
  (let* ((size (length vec))
         (new-vec (make-array size)))
    (dovector (var i vec)
              (setf (vector-ref new-vec i) var))
    new-vec))

(defun ti-copy&rename-vec (vec new-vars . same-vars)
  (let* ((size (length vec))
         (new-vec (make-array size)))
    (dotimes (i size)
             (setf (vector-ref new-vec i)
                   (ti-copy&rename-var (vector-ref vec i) new-vars (cons? same-vars))))
    new-vec))

(defun ti-copy&rename-var (var new-vars . same-vars)
  (let ((equ (get-substitution new-vars var)))
    (if equ
        (make <type-var> :id (?id (?right-expr equ)))
      (if same-vars
          (make <type-var> :id (?id var))
        (let ((new-var (new-type-var)))
          (add-substitution new-vars var new-var)
          new-var)))))

;;;-----------------------------------------------------------------------------
;;; COPYING DESCRIPTORS
;;;-----------------------------------------------------------------------------

(defgeneric ti-copy-descr (descr))

;;(defmethod ti-copy-descr (descr)
;;  (ti-error)
;;  descr)

(defmethod ti-copy-descr ((descr <formal-type-descr>))
  (make <formal-type-descr>
        :type-vars (ti-copy-subs (?type-vars descr))
        :type-vec (ti-copy-vec (?type-vec descr))
        :stat (?stat descr)
        :t-descr-before (?t-descr-before descr)
        :type-spec (?type-spec descr)))

(defmethod ti-copy-descr ((descr <recursive-type-descr>))
  (make <recursive-type-descr>
        :type-vars (ti-copy-subs (?type-vars descr))
        :type-vec (ti-copy-vec (?type-vec descr))
        :stat (?stat descr)
        :t-descr-before (?t-descr-before descr)
        :type-spec (?type-spec descr)))

(defmethod ti-copy-descr ((descr <act-type-descr>))
  (make <act-type-descr>
        :type-vars (ti-copy-subs (?type-vars descr))
        :type-vec (ti-copy-vec (?type-vec descr))
        :stat (?stat descr)
        :t-descr-before (?t-descr-before descr)
        :type-spec (?type-spec descr)))

(defgeneric ti-copy&rename-descr (descr new-vars . same-vars))

(defmethod ti-copy&rename-descr ((descr <formal-type-descr>)
                                 new-vars . same-vars)
  (let ((with-same-vars (cons? same-vars)))
    (make <formal-type-descr>
          :type-vars (ti-copy&rename-subs (?type-vars descr) new-vars with-same-vars)
          :type-vec (ti-copy&rename-vec (?type-vec descr) new-vars with-same-vars)
          :stat (?stat descr)
          :t-descr-before (?t-descr-before descr)
          :type-spec (?type-spec descr))))

(defmethod ti-copy&rename-descr ((descr <recursive-type-descr>)
                                 new-vars . same-vars)
  (let ((with-same-vars (cons? same-vars)))
    (make <recursive-type-descr>
          :type-vars (ti-copy&rename-subs (?type-vars descr) new-vars with-same-vars)
          :type-vec (ti-copy&rename-vec (?type-vec descr) new-vars with-same-vars)
          :stat (?stat descr)
          :t-descr-before (?t-descr-before descr)
          :type-spec (?type-spec descr))))

(defmethod ti-copy&rename-descr ((descr <act-type-descr>)
                                 new-vars . same-vars)
  (let ((with-same-vars (cons? same-vars)))
    (make <act-type-descr>
          :type-vars (ti-copy&rename-subs (?type-vars descr) new-vars with-same-vars)
          :type-vec (ti-copy&rename-vec (?type-vec descr) new-vars with-same-vars)
          :stat (?stat descr)
          :t-descr-before (?t-descr-before descr)
          :type-spec (?type-spec descr))))

;;; Copy a type descriptor up to a given index.
(defgeneric copy-descr-up-to (descr max-index))

(defmethod copy-descr-up-to ((descr <formal-type-descr>)
                             (max-index <int>))
  (let* ((vec (?type-vec descr))
         (size (length vec))
         (new-vec (make-array size)))
    (dotimes (i (length vec))
             (setf (vector-ref new-vec i) (vector-ref vec i)))
    (make <formal-type-descr>
          :type-vars (ti-copy-subs (?type-vars descr))
          :type-vec new-vec
          :stat (?stat descr)
          :t-descr-before (?t-descr-before descr)
          :type-spec 0)))

(defmethod copy-descr-up-to ((descr <act-type-descr>)
                             (max-index <int>))
  (let* ((vec (?type-vec descr))
         (size (length vec))
         (new-vec (make-array size)))
    (dotimes (i (length vec))
             (setf (vector-ref new-vec i) (vector-ref vec i)))
    (make <act-type-descr>
          :type-vars (ti-copy-subs (?type-vars descr))
          :type-vec new-vec
          :stat (?stat descr)
          :t-descr-before (?t-descr-before descr)
          :type-spec 0)))

;;;-----------------------------------------------------------------------------
;;; Answer subs when formal-descr is applied to actual-descr.
(defun application-subs (formal-descr   ;<type-descr>
                         actual-descr   ;<type-descr>
                         . same-vars)
  (let ((copy-subs (make <type-var-substitutions>)))
    (vec-application-subs (?type-vec formal-descr)
                          (?type-vec actual-descr)
                          0
                          copy-subs)
    (ti-copy&rename-subs (?type-vars formal-descr) copy-subs (cons? same-vars))))

(defun vec-application-subs (vec-left vec-right index subs)
  (if (< index (length vec-left))
      (let ((left-var (vector-ref vec-left index))
            (right-var (vector-ref vec-right index)))
        (if (null? (eq-type-var? left-var right-var))
            (add-substitution subs left-var right-var))
        (vec-application-subs vec-left vec-right (+ index 1) subs))))

#module-end
