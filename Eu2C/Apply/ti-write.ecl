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
;;;  Title: Formatting Type Inference Objects
;;;  Description:
;;    This modules provides generic functions to write and print all kinds
;;    of objects that are concerned during the type inference process.
;;;  Documentation:
;;;  Notes:
;;    Changes by IM:
;;    * At some places the identifier for output is not directly taken from
;;    ?identifier but is computed by 'export-identifier' because of the need for name
;;    equivalence in .def-files.
;;    * ~A was replaced at some places by ~S to get print-read-equivalence.
;;;  Requires:
;;;  Problems:
;;;  Authors: Andreas Kind
;;;-----------------------------------------------------------------------------

#module ti-write
(import (ti
         ti-lattice
         ti-exprs
         ti-meet-join
         ti-eqs
         mzs
         lzs
         (only (name-of funtype-of) name-of-fun)
         (only (<binding>) el2lzs)
         (only (export-identifier) code-identifier)
         (only (stream
                make-string-output-stream
                remove-if-not
                terpri
                get-output-stream-string
                maphash
                format)
               common-lisp))
 syntax (ti
         ti-exprs)
 export (ti-write-list
         ti-write-vector
         ti-def-write-list
         ti-def-write-vector
         ti-write
         ti-print-string
         ti-print-string-no-cr
         ti-print
         ti-print-fun
         ti-def-write
         ti-def-print-string
         ti-def-print
         def-write-super-strategic-lattice-types
         def-write-remaining-strategic-lattice-types
         reset-def-strategic-lattice-types
         ti-short-write-string))

;;;-----------------------------------------------------------------------------
;;; WRITING LISTS AND VECTORS
;;;-----------------------------------------------------------------------------

(defun ti-write-list (stream list)
  (cond (list
         (ti-write stream (car list))
         (cond ((> (length list) 1)
                (format stream " ")
                (ti-write-list stream (cdr list)))))))


(defun ti-write-eqs (stream list)
  (cond (list
         (ti-write-eq stream (car list))
         (cond ((> (length list) 1)
                (format stream "~%          ")
                (ti-write-eqs stream (cdr list)))))))

(defun ti-write-vector (stream vec index)
  (cond ((> (length vec) index)
         (ti-write stream (vector-ref vec index))
         (cond ((> (length vec) 1)
                (format stream " ")
                (ti-write-vector stream vec (+ 1 index)))))))

(defun ti-def-write-list (stream list)
  (cond (list
         (ti-def-write stream (car list))
         (cond ((> (length list) 1)
                (format stream " ")
                (ti-def-write-list stream (cdr list)))))))

(defun ti-def-write-eqs (stream list)
  (cond (list
         (ti-def-write-eq stream (car list))
         (cond ((> (length list) 1)
                (format stream "~%    ")
                (ti-def-write-eqs stream (cdr list)))))))

(defun ti-def-write-vector (stream vec index)
  (cond ((> (length vec) index)
         (ti-def-write stream (vector-ref vec index))
         (cond ((> (length vec) 1)
                (format stream " ")
                (ti-def-write-vector stream vec (+ 1 index)))))))

;;;-----------------------------------------------------------------------------
;;; PRINT STRINGS AND PRINTING FOR ANY KIND OF OBJECTS
;;;-----------------------------------------------------------------------------

(defun ti-print (obj)
  (ti-format t "~%~A" (ti-print-string obj)))

(defun ti-print-string (obj)
  (let ((stream (make-string-output-stream)))
    (format stream "~%")
    (ti-write stream obj)
    (get-output-stream-string stream)))

(defun ti-print-string-no-cr (obj)
  (let ((stream (make-string-output-stream)))
    (ti-write stream obj)
    (get-output-stream-string stream)))

;;;-----------------------------------------------------------------------------
;;; DEFINITION PRINT STRINGS AND DEFINITION PRINTING FOR ANY KIND OF OBJECTS
;;;-----------------------------------------------------------------------------

(defun ti-def-print (obj)
  (format t "~%~A" (ti-def-print-string obj)))

(defun ti-def-print-string (obj)
  (let ((stream (make-string-output-stream)))
    (format stream "~%")
    (ti-def-write stream obj)
    (get-output-stream-string stream)))

;;;-----------------------------------------------------------------------------
;;; Write a definition string to given character stream.
(defgeneric ti-def-write (stream expr))

;;;-----------------------------------------------------------------------------
;;; WRITE A PRETTY PRINT STRING TO GIVEN CHARACTER STREAM
;;;-----------------------------------------------------------------------------

(defgeneric ti-write (stream expr))

(defmethod ti-write (stream expr)
  (format stream "~A" expr))

(defmethod ti-write (stream (expr <symbol>))
  (format stream "~A" expr))

(defmethod ti-write (stream (expr <pair>))
  (ti-write-list stream expr))

(defmethod ti-write (stream (expr <vector>))
  (ti-write-vector stream expr 0))

(defun ti-write-list-enclosed (stream list start)
  (if list
      (let ((first (car list)))
        (if start (format stream "("))
        (if (consp first)
            (ti-write-list-enclosed stream first t)
          (ti-write stream first))
        (if (cdr list)
            (format stream " "))
        (ti-write-list-enclosed stream (cdr list) nil))
    (format stream ")")))

;; Attention: %object is displayed as <object>; %class is displayed as <class>.
(defmethod ti-write (stream (expr <atomic-type>))
  (let ((name (?name expr)))
    (if (consp name)
        (ti-write-list-enclosed stream (?name expr) t)
      (cond ((and *%object* (%object-type-p expr))
             (format stream "<OBJECT>"))
            ((and *%class* (%class-type-p expr))
             (format stream "<CLASS>"))
            (t (ti-write stream name))))))

(defmethod ti-write (stream (expr <type-var>))
  (let ((id (?id expr)))
    (if (symbolp id)
        (format stream "~A" id)
      (format stream "var~A" (?id expr)))))

(defmethod ti-write (stream (id <slot-id>))
  (format stream "(slot ~A)" (?slot-name id)))

;;;-----------------------------------------------------------------------------
;;; WRITE A DEFINITION STRING TO GIVEN CHARACTER STREAM
;;;-----------------------------------------------------------------------------

(defmethod ti-def-write (stream obj))

(defmethod ti-def-write (stream (expr <pair>))
  (ti-def-write-list stream expr))

(defmethod ti-def-write (stream (expr <vector>))
  (ti-def-write-vector stream expr 0))

(defmethod ti-def-write (stream (expr <atomic-type>))
  (format stream "(atom? ")
  (let ((name (?name expr)))
    (if (consp name)
        (ti-write-list-enclosed stream (?name expr) t)
      (ti-write stream name)))
  (format stream ")"))

(defmethod ti-def-write (stream (expr <type-var>))
  (let ((id (?id expr)))
    (if (symbolp id)
        (format stream "(var ~A)" id)
      (format stream "(var var~A)" (?id expr)))))

(defmethod ti-def-write (stream (lattice-type <lattice-type>))
  (format stream "~2%(%define-lattice-type ~S (" (?name lattice-type))
  (mapc (lambda (super-lattice-type)
          (format stream "~S " (?name super-lattice-type)))
        (?supertypes lattice-type))
  (format stream ") (")
  ;;  (mapc (lambda (sub-lattice-type)
  ;;        (format stream "~a " (?name sub-lattice-type)))
  ;;      (?subtypes lattice-type))
  (format stream "bottom")
  (format stream ") ")
  (if (?compound lattice-type)
      (format stream "t")
    (format stream "() "))
  (ti-def-write-literals stream lattice-type)
  (format stream ")"))

(defgeneric ti-def-write-literals (stream lattice-type))

(defmethod ti-def-write-literals (stream (lattice-type <lattice-type>))
  ;; nothing to do
  nil)

(defmethod ti-def-write-literals (stream (lattice-type <lattice-type-with-literals>))
  ;; the literals are in unexpanded form!
  (mapc (lambda (literal)
          (format stream "~S " literal))
        (?literals lattice-type)))

(deflocal *def-strategic-lattice-types* ())

(defun reset-def-strategic-lattice-types ()
  (setq *def-strategic-lattice-types* ()))

(defun def-write-super-strategic-lattice-types (stream class)
  (let ((strategic-supers
         (remove-if-not #'def-write-super-strategic-lattice-type-p
                        (?supertypes (?lattice-type class)))))
    (mapc (lambda (lattice-type)
            (setq *def-strategic-lattice-types*
                  (cons lattice-type *def-strategic-lattice-types*))
            (ti-def-write stream lattice-type))
          strategic-supers)
    (mapcar #'?name strategic-supers)))

(defun def-write-remaining-strategic-lattice-types (stream)
  (mapc (lambda (entry)
          (let ((lattice-type (cdr entry)))
            (cond ((def-write-super-strategic-lattice-type-p lattice-type)
                   (setq *def-strategic-lattice-types*
                         (cons lattice-type *def-strategic-lattice-types*))
                   (ti-def-write stream lattice-type)))))
        *strategic-lattice-types*))

(defun def-write-super-strategic-lattice-type-p (lattice-type)
  (and (?subtypes lattice-type)
       (eq-lattice-type *bottom* (car (?subtypes lattice-type)))
       (null? (member lattice-type  *def-strategic-lattice-types*))))

;;;-----------------------------------------------------------------------------
;;; WRITING TYPE EQUATIONS AND SUBSTITUTIONS
;;;-----------------------------------------------------------------------------

(defun ti-write-eq (stream eq)
  (format stream "(")
  (ti-write stream (?left-expr eq))
  (format stream " = ")
  (ti-write stream (?right-expr eq))
  (format stream ")"))

(defun ti-def-write-eq (stream eq)
  (format stream "(")
  (ti-def-write stream (?left-expr eq))
  (format stream " ")
  (ti-def-write stream (?right-expr eq))
  (format stream ")"))

(defmethod ti-write (stream (eqs <type-equation-stack>))
  (ti-write-eqs stream (?equations eqs)))

(defmethod ti-def-write (stream (eqs <type-equation-stack>))
  (ti-def-write-eqs stream (?equations eqs)))

;;;-----------------------------------------------------------------------------
;;; WRITING TYPE DESCRIPTORS
;;;-----------------------------------------------------------------------------

(defmethod ti-write (stream (descr <type-descr>))
  (format stream "~%  :descr <")
  (ti-write stream (?type-vec descr))
  (format stream ">~%          ")
  (ti-write stream (?type-vars descr)))

(defmethod ti-def-write (stream (descr <type-descr>))
  (format stream "~%  ((")  ; *IM* 01.03.94
  (ti-write stream (?type-vec descr))
  (format stream ")~%    ")   ; *IM* 01.03.94
  (ti-def-write stream (?type-vars descr))
  (format stream ")")); *IM* 08.03.94

(defmethod ti-write (stream (fun <fun>))
  (let ((sig (?signature fun)))
    (cond (sig
           (format stream "(~A:~A::~A"
                   (funtype-of fun)
                   (?module-id fun)
                   (name-of fun))
           (ti-write stream sig)
           (format stream ")"))
          (t
           (ti-format t
                      "~%Warning: no type scheme for function ~A"
                      (?identifier fun))))))

(defmethod ti-def-write (stream (fun <fun>))
  (let ((sig (?signature fun)))
    (cond (sig
           (format stream "~2%(%annotate-function ~S new-signature ("
                   (export-identifier fun))
           (ti-def-write stream sig)
           (format stream "))"))
          (t
           (format stream "~2%\;\;\;Warning: no type scheme for function ~A"
                   (?identifier fun))))))

(defmethod ti-def-write (stream (fun <named-const>)))

;;;-----------------------------------------------------------------------------
;;; WRITING LATTICES
;;;-----------------------------------------------------------------------------

(defun ti-write-lattice (stream lattice-type indent)
  (cond ((?subtypes lattice-type)
         (format stream "~%~70B~A~A [~A]"
                 (?code lattice-type)
                 indent
                 (?name lattice-type)
                 (?write-access-stamp lattice-type))
         (dolist (x (?subtypes lattice-type))
                 (ti-write-lattice stream x (format nil " ~A" indent))))))

(defun ti-write-lattice-type (stream lattice-type)
  (let ((class (?class lattice-type)))
    (format stream "~%~%| ~A" (?name lattice-type))
    (format stream "~%| strategic: ~A" (?strategic lattice-type))
    (format stream "~%| compound: ~A" (?compound lattice-type))
    (format stream "~%| write-access-stamp: ~A"
            (?write-access-stamp lattice-type))
    (format stream "~%| class: ~A" (if class (?identifier class) nil))
    (format stream "~%| supertypes: ")
    (dolist (x (?supertypes lattice-type))
            (format stream "~A " (?name x)))
    (format stream "~%| subtypes: ")
    (dolist (x (?subtypes lattice-type))
            (format stream "~A " (?name x)))))

(defmethod ti-write (stream (lattice <lattice>))
  (ti-write-lattice stream (?top lattice) " ")
  (maphash (lambda (key x)
             (ti-write-lattice-type stream x))
           (?table lattice)))

(defmethod ti-write (stream (lattice-type <lattice-type>))
  (cond ((and *%object* (eq-lattice-type lattice-type *%object*))
         (format stream "<OBJECT>"))
        ((and *%class* (eq-lattice-type lattice-type *%class*))
         (format stream "<CLASS>"))
        (t (format stream "~A" (?name lattice-type)))))

(defun ti-print-fun (fun)
  (ti-print fun)
  (dolist (descr (?type-descr-s fun))
          (format t "~%T-DESCR-BEFORE") (ti-print-t-descrs-before descr)))

(defun ti-print-t-descrs-before (descr)
  (cond (descr
         (ti-print descr)
         (ti-print-t-descrs-before (?t-descr-before descr)))))


#module-end
