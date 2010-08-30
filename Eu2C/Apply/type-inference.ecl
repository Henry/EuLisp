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
;;;  Title: Main Type Inference File
;;;  Description:
;;    Provides type inference functions, that are used in other
;;    parts of the APPLY compiler.
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;    Other modules that are used for type inference directly or
;;    indirectly by this module are:
;;    ti-signature  Handling Type Schemes (Signatures)
;;    ti-descr      Default Type Descriptors
;;    ti-special    Special Inference with some System (TAIL) Functions
;;    ti-comp       Inference with Compound Lattice Types
;;    ti-const      Type Inference of Constants
;;    ti-init       Initialization of the Typ Inference System
;;    ti-meet-join  Operations on Type Expressions for Type Inference
;;    ti-unify      A Modified Unification Algorithm for Type Inference
;;    ti-eqs        Type Equations for Type Inference
;;    ti-exprs      Type Expressions for Type Inference
;;    ti-copy       Copying Type Inference Objects
;;    ti-write      Formatting Type Inference Objects
;;    ti-lattice    Lattice and Lattice Types Used for Type Inference
;;    ti-codes      Encoding Lattice Types
;;    ti            Auxiliary Functions and Parameters for Type Inference
;;;  Problems:
;;;  Authors: Andreas Kind
;;;-----------------------------------------------------------------------------

#module type-inference
(import (ti
         lzs
         lzs-mop
         mzs
         lzs-modules
         tail-module
         apply-funs
         analyse-h
         name-of-fun
         ti
         ti-lattice
         ti-exprs
         ti-eqs
         ti-write
         ti-copy
         ti-comp
         ti-meet-join
         ti-signature
         ti-const
         ti-descrs
         ti-special
         (only (mapc
                mapcar
                dotimes
                format
                terpri
                delete-if-not)
               common-lisp))
 syntax (ti)
 export (inference
         inference-get-slot-value inference-set-slot-value
         balance balance-applications constant-type
         filled-formal-descr filled-recursive-descr filled-actual-descr
         general-var-formal-descr general-var-recursive-descr
         general-var-actual-descr
         empty-formal-descr empty-recursive-descr empty-actual-descr
         get-descr-type set-descr-type get-previous-subs
         set-signature set-signature-from-classes set-predicate-signature
         reduce-descr set-joined-result-types
         convert-to-sys-type-vec class-as-type-expr
         check-result-subtypes check-subtype-exprs
         initialize-lattice general-type %object-type
         copy-descr-up-to ti-copy-subs
         ti-format ti-short-write ti-def-write))

;;;-----------------------------------------------------------------------------
;;; SPECIALIZING ACTUAL TYPE DESCRIPTORS
;;;-----------------------------------------------------------------------------

;; Inference step, i.e. a function is called with an actual type scheme.
(defun inference (fun incomming-descrs)
  (ti-statistics *fun-call-key*)
  (ti-format t "~%Notice: check-compound-types ~A" (name-of fun))
  (let (new-incomming-descrs
        result-descrs)  ; treat compound type?
    (setq new-incomming-descrs
          (if *use-compound-types*
              (delete-if-not #'check-compound-types-before incomming-descrs)
            incomming-descrs))
    (setq *actual-method-subset* ())
    (setq result-descrs (compute-inference fun new-incomming-descrs))
    (if *use-compound-types*
        (setq result-descrs
              (delete-if-not #'check-compound-types-after result-descrs)))
    ;;    (if cl::*p*
    ;;      (format t "~%== NEW DESCRS:~A" (ti-print-string result-descrs)))
    result-descrs))

(defgeneric compute-inference (fun descrs))

(defmethod compute-inference ((fun <fun>) descrs)
  (let ((formal-descrs ())
        (standard-inference nil))
    (cond ((eq fun %extract)
           (setq formal-descrs (convert-to-formal-descrs-%extract descrs))
           (setf (?signature fun) formal-descrs))
          ((eq fun %setf-extract)
           (setq formal-descrs (convert-to-formal-descrs-%setf-extract descrs))
           (setf (?signature fun) formal-descrs))
          ((eq fun %funcall)
           (setq formal-descrs (convert-to-formal-descrs-%funcall descrs))
           (setf (?signature fun) formal-descrs))
          (t
           (setq formal-descrs (?signature fun))
           (setq standard-inference t)))
    ;;    (if cl::*p*
    ;;      (progn
    ;;        (format t "~%== FUNCTION DESCRS:~A" (ti-print-string fun))
    ;;        (format t "~%== ACTUAL DESCRS:~A"
    ;;                (ti-print-string
    ;;                 (mapcar (lambda (descr)
    ;;                           (get-previous-subs (ti-copy-descr descr)))
    ;;                         descrs)))))
    (if formal-descrs    ;; uncomplete signature?
        (if standard-inference
            (unify-descrs fun descrs formal-descrs)
          (unify-descrs fun descrs formal-descrs t))
      (let ((range-and-domain-descr (?type-descr fun)))
        (if range-and-domain-descr
            (unify-descrs fun descrs (list range-and-domain-descr))
          (mapcar #'get-previous-subs descrs))))))

(defmethod compute-inference ((fun <defined-generic-fun>) descrs)
  (ti-format2 t "~%== GENERIC FUNCTION DESCRS:~A" (ti-print-string fun))
  (if (null? (?signature fun))
      (ti-format2 t "~%~A" (?identifier fun)))
  (ti-format2 t "~%== ACTUAL DESCRS:~A"
              (ti-print-string
               (mapcar (lambda (descr)
                         (get-previous-subs (ti-copy-descr descr)))
                       descrs)))
  (let* ((formal-descrs (?signature fun))
         (result-descrs
          (if formal-descrs
              (unify-descrs fun descrs formal-descrs)
            (let ((range-and-domain-descr (?type-descr fun)))
              (if range-and-domain-descr
                  (unify-descrs fun descrs (list range-and-domain-descr))
                (mapcar #'get-previous-subs descrs))))))
    (setq *actual-method-subset*
          (select-methods fun result-descrs))
    ;;    (cond (*ti-verbose*
    ;;         (terpri) (terpri)
    ;;         (ti-short-write t (mapcar #'ti-copy-descr result-descrs))
    ;;         (terpri)
    ;;         (ti-short-write-methods t (?method-list fun))
    ;;         (format t "~%Notice: reduced possible methods ~A -> ~A"
    ;;                    (length (?method-list fun))
    ;;                    (length *actual-method-subset*))
    ;;         (ti-short-write-methods t *actual-method-subset*)
    ;;         (terpri) (terpri)))
    (ti-format2 t "~%== NEW DESCRS:~A" (ti-print-string result-descrs))
    (if (null? *actual-method-subset*)
        (ti-error))      ;; no applicable method!
    result-descrs))

(defun inference-get-slot-value (descrs slot-description)
  (let ((formal-descrs (formal-descrs-get-slot-value descrs slot-description))
        (fun %select))
    (setf (?signature fun) formal-descrs)
    (unify-descrs fun descrs formal-descrs t)))

(defun inference-set-slot-value (descrs slot-description)
  (ti-statistics *%setf-select-key*)
  (let ((formal-descrs (formal-descrs-set-slot-value descrs slot-description))
        (fun %setf-select))
    (setf (?signature fun) formal-descrs)
    (unify-descrs fun descrs formal-descrs t)))

;;; Condense descriptor list to a single descriptor.
(defun balance (descrs)
  (join-descrs-min (ti-copy-descr (car descrs)) (cdr descrs)))

;;; Condense application descriptor list to a single descriptor.
(defun balance-applications (fun applications)
  (let* ((new-descr (balance (mapcar #'?type-descr applications)))
         (old-descr (balance (?type-descr-s fun)))
         (new-result-type (get-result-type new-descr))
         (old-result-type (get-result-type old-descr))
         (optimized ()))
    ;;    (format t "g")      ;; mark global optimization
    (cond ((%void-type-p new-result-type)
           (set-result-type-min new-descr old-result-type))
          ((true-subtype-expr-p old-result-type new-result-type)
           (set-result-type-min new-descr old-result-type)))
    ;;      (cond (*ti-verbose*
    ;;       (dotimes (i (length (?type-vec new-descr)))
    ;;         (setq optimized
    ;;           (or (null? (and (subtype-expr-p
    ;;                           (get-arg-type old-descr i)
    ;;                           (get-arg-type new-descr i))
    ;;                          (subtype-expr-p
    ;;                           (get-arg-type new-descr i)
    ;;                           (get-arg-type old-descr i))))
    ;;               optimized)))
    ;;       (if optimized
    ;;           (progn
    ;;             (format t "~2%Global optimization of ~A function ~A::~A~%"
    ;;                     (funtype-of fun)
    ;;                     (?module-id fun)
    ;;                     (name-of fun))
    ;;             (ti-short-write t new-descr)
    ;;             (format t "~%    instead of~%")
    ;;             (ti-short-write t old-descr)))))
    new-descr))

#module-end
