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
;;;  Title: An interpreter for the LZS
;;;  Description:
;;;  Documentation:
;;    lzs-syntax contains the macro def-lzs-object for lzs
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Winfried Heicking, Ingo Mohr
;;;-----------------------------------------------------------------------------

#module lzs-syntax
(import (eulisp0
         apply-standard
         accessors)
 syntax (eulisp0
         apply-standard)
 import ((only (append
                caar
                intern
                format
                last)
               common-lisp))
 syntax ((only (pushnew)
               common-lisp))
 export (def-lzs-object  ; macro
          make-structure-and-annotation-slots
          make-predicate-name
          <lzs-object>
          lzs-object-p)
 expose (accessors))

;;;-----------------------------------------------------------------------------
;;; defstandardclass of <lzs-object>
;;  here because of name conflict with symbol <lzs-object>
;;  in function make-supers
;;;-----------------------------------------------------------------------------
(deflocal *structure-slots* ())
(deflocal *annotation-slots* ())

(defstandardclass <lzs-object> ()  ; the top node
  (source :initform () :accessor :writer :initarg)   ; some reference to the source
  :predicate)

;;; only to create accessor table for the module 'accessors'
(defun print-slots (slots)
  (format t "~%; --- slots ---~%~{~(~A~%~)~}"
          (cl:sort slots
                   #'cl:string<
                   :key #'cl:string)))

(defconstant $default-annotations  ())

;;;-----------------------------------------------------------------------------
;;; def-lzs-object
;;;-----------------------------------------------------------------------------
(defmacro def-lzs-object (name supers . slots)
  `(progn
     (defstandardclass
       ,(make-eulisp-class-id name)
       ,(make-supers supers)
       ,@(make-lzs-slots supers slots)
       :predicate)
     ,@(make-mixin-predicates name supers)
     (export
      ,(make-eulisp-class-id name)
      ,(make-predicate-name name))))

(defun make-lzs-slots (supers slots)
  (make-structure-and-annotation-slots
   (add-annotations (add-mixin-slots supers slots)
                    $default-annotations)))

(defun add-structure-slots (new-slots slots)
  (append new-slots slots))

(defun add-annotations (new-slots slots)
  (append slots new-slots))

(defun add-mixin-slots (supers slots)
  (cond ((null? supers) slots)
        ((eq (car supers) ':named)
         (add-mixin-slots (cdr supers)
                          (add-annotations
                           '((identifier :initform ())
                             (module :initform ())
                             (code-identifier :initform ()))
                           slots)))
        ((eq (car supers) ':global)
         (add-mixin-slots (cdr supers)
                          ;;***HGW (cons ':named (cdr supers))
                          ;;***HGW :named is now used explicitly with :global
                          (add-structure-slots
                           '((exported :initform ()))
                           slots)))
        ((eq (car supers) ':imported)
         (add-mixin-slots (cdr supers)
                          (add-annotations
                           '((definition :initform ())
                             (language :initform ()))
                           slots)))
        (t
         (add-mixin-slots (cdr supers) slots))
        ))

(defun make-structure-and-annotation-slots (slots)
  (cond ((null? slots) ())
        ((eq (car slots) ':annotations)
         (make-annotations (cdr slots)))
        ((cons? (car slots))
         (pushnew (caar slots) *structure-slots*)
         (cons
          (append (car slots)
                  '(:accessor :writer :initarg))
          (make-structure-and-annotation-slots (cdr slots))))
        (t
         (pushnew (car slots) *structure-slots*)
         (cons
          (list (car slots) ':accessor ':writer ':initarg)
          (make-structure-and-annotation-slots (cdr slots))))))

(defun make-annotations (slots)
  (cond ((null? slots) ())
        ((cons? (car slots))
         (pushnew (caar slots) *annotation-slots*)
         (cons
          (append (car slots)
                  '(:accessor :writer :initarg))
          (make-annotations (cdr slots))))
        (t
         (pushnew (car slots) *annotation-slots*)
         (cons
          (list (car slots) ':accessor ':writer ':initarg)
          (make-annotations (cdr slots))))))

(defun get-superclass-name (supers)
  (make-eulisp-class-id (car (last supers))))

(defun make-supers (supers)
  (if (null? supers)
      '(<lzs-object>)
    (list (get-superclass-name supers))))

(defun make-predicate-name (class-name)
  (intern (format () "~A-P" class-name)))

(defun make-mixin-predicates (class-name supers)
  (cond ((null? supers) ())
        ((member (car supers) '(:named :global :imported))
         (cons
          `(defmethod ,(make-predicate-name (car supers))
             ((object ,(make-eulisp-class-id class-name)))
             t)
          (make-mixin-predicates class-name (cdr supers))))
        (t
         (make-mixin-predicates class-name (cdr supers)))))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
