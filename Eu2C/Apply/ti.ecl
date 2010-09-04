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
;;;  Title: Auxiliary Functions and Parameters for Type Inference
;;;  Description:
;;    Auxiliary functions for formatting, error signaling, collections, and
;;    statistics.
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Andreas Kind
;;;-----------------------------------------------------------------------------


#module ti
(import (level-1
         apply-standard
         debugging
         (only (format
                cerror
                float
                member-if
                dotimes
                clrhash
                gethash
                make-hash-table) common-lisp))
 syntax (level-1 debugging)
 export (member-with-args
         dovector
         ti-format
         ti-format2
         set-ti-verbose0
         set-ti-verbose1
         set-ti-verbose2
         ti-error
         ti-statistics
         initialize-ti-statistics
         ti-print-statistics
         *max-call-descrs*
         *max-signature-descrs*
         *use-compound-types*
         *ti-short-print*
         *use-global-optimization*
         *fun-call-key*
         *inferred-signature-key*
         *inferred-classes-key*
         *joined-call-descrs-key*
         *joined-signature-descrs-key*
         *inferred-abstract-classes-key*
         *%setf-select-key*)
 expose (level-1
         accessors
         apply-standard
         simple-programming
         debugging
         (only (append
                dolist
                mapc
                mapcar
                maphash) common-lisp)))

;;;-----------------------------------------------------------------------------
;;; TYPE INFERENCE PARAMETERS
;;;-----------------------------------------------------------------------------

;; Determines whether a short output for error notification satisfies.
(deflocal *ti-short-print* t)

;; Maximal number of descriptors used for a function call; if acceeded
;; descritors are joind.
(deflocal *max-call-descrs* 6)
(setq  *max-call-descrs* 4)

;; Maximal number of descriptors used in function signatures; if acceeded
;; descritors are joind.
(deflocal *max-signature-descrs* 4)

;; Flag that determines whether all compound types are converted to
;; non-compound super types.
(deflocal *use-compound-types* ())
;;(setq *use-compound-types* t)

;; Format level; level 0: quiet mode; level 1: major warnings; level 2: all
;; outputs are displayed.
(setq *ti-verbose* 0)

;;;-----------------------------------------------------------------------------
;;; BASIC COLLECTION FUNCTIONS
;;;-----------------------------------------------------------------------------

(defun member-with-args (pred list . args)
  (member-if (lambda (x) (apply pred x args)) list))

;;; Same as dolist for vector objects.
(defmacro dovector ((elem index vector-form) . body)
  `(dotimes (i (length ,vector-form) 1)
            (let ((,elem (vector-ref ,vector-form i))
                  (,index i))
              ,@body)))

;;;-----------------------------------------------------------------------------
;;; TYPE INFERENCE FORMAT/ERROR
;;;-----------------------------------------------------------------------------

;;; Toggable format function for type-inference.
(defmacro ti-format args
  (let ((ok (if (number? *ti-verbose*)
                (> *ti-verbose* 0)
              *ti-verbose*)))
    (if ok
        `(format ,@args))))

;;; Second toggable format function for type-inference.
(defmacro ti-format2 args
  (if (and (number? *ti-verbose*)
           (> *ti-verbose* 1))
      `(format ,@args)))

(defun set-ti-verbose0 ()
  (setq *ti-verbose* 0))

(defun set-ti-verbose1 ()
  (setq *ti-verbose* 1))

(defun set-ti-verbose2 ()
  (setq *ti-verbose* 2))

;;(defmacro ti-error args
;;  (let ((ok (if (number? *ti-break*)
;;              (> *ti-break* 0)
;;            *ti-break*)))
;;    (if ok
;;      `(cerror "Try to continue" "Break from type inference!"))))

(defun ti-error args
  (if *ti-break*
      (cerror "Go ahead .." "Break from type inference!")))

;;;-----------------------------------------------------------------------------
;;; Type inference statistics.
;;;-----------------------------------------------------------------------------

(deflocal *the-statistics* (make-hash-table))

(deflocal *fun-call-key* 'fun-call)
(deflocal *inferred-signature-key* 'inferred-signature)
(deflocal *inferred-classes-key* 'inferred-classes)
(deflocal *joined-call-descrs-key* 'joined-call-descrs)
(deflocal *joined-signature-descrs-key* 'joined-signature-descrs)
(deflocal *inferred-abstract-classes-key* 'inferred-abstact-classes)

;; Interesting for treating compound lattice types thus %setf-select may
;; destroy information about compound types.
(deflocal *%setf-select-key* '%setf-select)

(defun initialize-ti-statistics ()
  (clrhash *the-statistics*))

(defun ti-statistics (key)
  (if (null? key) (ti-error))
  (let ((value (get-ti-statistics key)))
    (if value
        (setf (gethash key *the-statistics*) (+ value 1))
      (setf (gethash key *the-statistics*) 0))))

(defun get-ti-statistics (key)
  (gethash key *the-statistics*))

(defun ti-print-statistics ()
  (let ((classes (or (get-ti-statistics *inferred-classes-key*) 0))
        (abstract-classes (or (get-ti-statistics *inferred-abstract-classes-key*) 0))
        (calls (or (get-ti-statistics *fun-call-key*) 0))
        (joined-calls (or (get-ti-statistics *joined-call-descrs-key*) 0))
        (signatures (or (get-ti-statistics *inferred-signature-key*) 0))
        (joined-signatures (or (get-ti-statistics *joined-signature-descrs-key*) 0))
        (select (or (get-ti-statistics *%setf-select-key*) 0)))
    (format t "~%Total number of analysed function calls: ~A" calls)
    (format t "~%Total number of joined function call descriptors: ~A (~,2F %)"
            joined-calls
            (if (zero? calls) 0
              (* (/ (float joined-calls) (float calls)) 100)))
    (ti-format t "~%Total number of analysed %setf-select calls: ~A" select)
    (format t "~2%Total number of inferred function type schemes: ~A" signatures)
    (format t "~%Total number of joined type scheme descriptors: ~A (~,2F %)"
            joined-signatures
            (if (zero? signatures) 0
              (* (/ (float joined-signatures) (float signatures)) 100)))
    (format t "~2%Total number of inferred classes: ~A" classes)
    (format t "~%Total number of inferred abstract classes: ~A (~,2F %)"
            abstract-classes
            (if (zero? classes) 0
              (* (/ (float abstract-classes) (float classes)) 100)))))

#module-end
