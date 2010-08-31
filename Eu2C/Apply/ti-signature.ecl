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
;;;  Title: Handling Type Schemes (Signatures)
;;;  Description:
;;    Type schemes (signatures) describe the range and domain of a
;;    function. Type schemes are generic, i.e. they may have more than one
;;    line (descriptor). The type schemes contain type expressions (see
;;                                                                  ti-exprs.em). Generic type schemes for standard functions are
;;    predefined; for defined functions they are inferred by a modified
;;    unification algorithm (see ti-unify.em).
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Andreas Kind
;;;-----------------------------------------------------------------------------


#module ti-signature
(import (lzs lzs-mop mzs lzs-modules tail-module machine-description
             ti ti-codes  ti-lattice ti-exprs ti-eqs ti-write ti-copy
             ti-meet-join ti-unify ti-descrs ti-comp debugging
             messages ; *IM* 01.03.94
             (only (class-sealed-p) predicates)
             (only (name-of funtype-of) name-of-fun)
             (only (append dolist mapc mapcar vector remove-if-not
                           remove-if abs delete-if-not
                           format dotimes member-if member-if-not terpri
                           remove-duplicates sort) common-lisp))
 syntax (ti)
 export (new-signature extend-signature renew-signature comp-signature
                       reset-funs-with-defined-signatures funs-with-defined-signatures
                       convert-to-sys-type-vec type-expr-to-class
                       unify-descrs join-descrs join-descrs-min select-methods
                       reduce-descr convert-to-atomic-descr
                       set-descr-type get-descr-type get-arg-type get-previous-subs
                       get-result-type set-result-type-min specialize-descrs
                       set-signature set-signature-from-classes
                       check-result-subtypes check-subtype-exprs
                       set-predicate-signature set-joined-result-types
                       ti-short-write ti-short-write-methods))

;;;-----------------------------------------------------------------------------
;;; READING TYPE SIGNATURES
;;;-----------------------------------------------------------------------------

;; Functions with defined type schemes.
(deflocal *funs-with-defined-signatures* '())

(defun funs-with-defined-signatures ()
  *funs-with-defined-signatures*)

(defun reset-funs-with-defined-signatures ()
  (ti-format t "~%Reset *funs-with-defined-signatures*.")
  (setq *funs-with-defined-signatures* '()))

(defun new-fun-with-defined-signatures (fun)
  (setq *funs-with-defined-signatures*
        (cons fun *funs-with-defined-signatures*)))

(defun new-signature (fun key descrs-def)
  (if (eq key ^new-signature)           ; new signature
      (let ((new-descrs (def-descrs descrs-def)))
        (new-fun-with-defined-signatures fun)
        (if (null? *use-compound-types*)
            (setq new-descrs
                  (delete-if-not #'convert-all-compound-types new-descrs)))
        (if (?signature fun)
            (ti-format t "~%Warning: type scheme of ~A function ~A::~A already defined"
                       (funtype-of fun)
                       (?module-id fun)
                       (name-of fun)))
        (set-defined-signature fun new-descrs)
        (ti-format t "~%Notice: new type scheme of ~A function ~A::~A"
                   (funtype-of fun)
                   (?module-id fun)
                   (name-of fun)))
    (write-message ^warning "wrong key to add type scheme ~A" key))); *IM* 01.03.94

(defun renew-signature (fun key descrs-def)
  (if (eq key ^renew-signature)         ; renew signature
      (let ((new-descrs (def-descrs descrs-def)))
        (new-fun-with-defined-signatures fun)
        (set-defined-signature fun new-descrs)
        (ti-format t "~%Notice: renew type scheme of ~A function ~A::~A."
                   (funtype-of fun)
                   (?module-id fun)
                   (name-of fun)))
    (write-message ^warning "wrong key to renew type scheme ~A" key))); *IM* 01.03.94

(defun comp-signature (fun key descrs-def)
  (if (eq key ^comp-signature)          ; renew signature
      (if (null? *use-compound-types*)
          (write-message ^info "compound type scheme of ~A function ~A::~A ignored" ; *IM* 01.03.94
                         (funtype-of fun)
                         (?module-id fun)
                         (name-of fun))
        (let ((new-descrs (def-descrs descrs-def)))
          (new-fun-with-defined-signatures fun)
          (set-defined-signature fun new-descrs)
          (write-message ^info "compound type scheme of ~A function ~A::~A" ; *IM* 01.03.94
                         (funtype-of fun)
                         (?module-id fun)
                         (name-of fun))))
    (write-message ^warning "wrong key to renew type scheme ~A" key))) ; *IM* 01.03.94

(defun extend-signature (fun key descrs-def)
  (if (eq key ^extend-signature)        ; extend signature
      (let ((new-descrs (def-descrs descrs-def))
            (signature (?signature fun)))
        (if (null? signature)
            (new-fun-with-defined-signatures fun))
        (if (null? *use-compound-types*)
            (setq new-descrs
                  (delete-if-not #'convert-all-compound-types new-descrs)))
        (set-defined-signature fun (append signature new-descrs))
        (ti-format t "~%Notice: extend type scheme of ~A function ~A::~A"
                   (funtype-of fun)
                   (?module-id fun)
                   (name-of fun)))
    (write-message ^warning "wrong key to extend type scheme ~A" key))) ; *IM* 01.03.94

(defun def-descrs (descrs-def)
  (mapcar #'def-descr descrs-def))

(defun def-descr (descr-def)
  (let ((vec (apply #'vector (mapcar (lambda (id)
                                       (make <type-var> :id id))
                                     (car descr-def))))
        (subs (mapcar #'eval-to-equation (cdr descr-def))))
    (make <formal-type-descr>
          :type-vec vec
          :type-vars (make <type-var-substitutions> :equations subs)
          :stat ()
          :t-descr-before ()
          :type-spec 0)))

(defun set-defined-signature (fun descrs)
  (let* ((descr-arity (- (length (?type-vec (car descrs))) 1))
         (fun-arity (abs (or (?arg-num fun) descr-arity))))
    (if (= fun-arity descr-arity)
        (setf (?signature fun) descrs)
      (write-message ^warning "wrong arity of defined type scheme (~A::~A)" ; *IM* 01.03.94
                     (?module-id fun)
                     (name-of fun)))))

;;;-----------------------------------------------------------------------------
;;; UNIFICATION OF TYPE DESCRIPTORS
;;;-----------------------------------------------------------------------------

(defun unify-descr&descr (actual-descr formal-descr)
  (let* ((formal-subs (application-subs-check-%void formal-descr actual-descr))
         (subs (ti-copy-subs (previous-subs actual-descr)))
         (stack (append-substitutions formal-subs (?type-vars actual-descr))))
    (ti-format2 t "~%-- Start unification ...")
    (ti-format2 t "~%substitutions before ~A" (ti-print-string subs))
    (ti-format2 t "~%stack~A" (ti-print-string stack))
    (cond ((unify (?equations stack) subs)
           (setf (?type-vars actual-descr) subs)
           (ti-format2 t "~%substitutions after ~A" (ti-print-string subs))
           ;;          (set-previous-subs actual-descr)
           (ti-format2 t "~%-- ... unification succeeded.")
           actual-descr)
          (t
           (ti-format2 t "~%-- ... unification failed.")
           ()))))

(defun unify-descr&descrs (actual-descr formal-descrs)
  (let ((result-descrs ()))
    (dolist (descr formal-descrs)
            (let ((new-descr (unify-descr&descr (ti-copy-descr actual-descr) descr)))
              (if new-descr
                  (setq result-descrs (cons new-descr result-descrs)))))
    result-descrs))

(defun unify-descrs&descrs (actual-descrs formal-descrs)
  (let ((result-descrs ()))
    (dolist (descr actual-descrs)
            (let ((new-descrs (unify-descr&descrs descr formal-descrs)))
              (if new-descrs
                  (setq result-descrs (append result-descrs new-descrs)))))
    result-descrs))

(defun unify-descrs-1-to-1 (actual-descrs formal-descrs)
  (let ((result-descrs ()))
    (mapc (lambda (actual-descr formal-descr)
            (let ((new-descr (unify-descr&descr actual-descr formal-descr)))
              (if new-descr
                  (setq result-descrs (cons new-descr result-descrs)))))
          actual-descrs formal-descrs)
    result-descrs))

(defun unify-descrs (fun actual-descrs formal-descrs . special)
  (let ((reduced-descrs actual-descrs))
    (if (> (length actual-descrs) *max-call-descrs*)
        (let ((first-descr (ti-copy-descr (car actual-descrs))))
          (join-descrs first-descr (cdr reduced-descrs))
          (ti-statistics *joined-call-descrs-key*)
          (setq reduced-descrs (list first-descr))))
    (let ((new-descrs (if special
                          (unify-descrs-1-to-1 reduced-descrs formal-descrs)
                        (unify-descrs&descrs reduced-descrs formal-descrs))))
      (cond ((null? new-descrs)
             (let* ((return-descrs
                     (mapcar #'get-previous-subs reduced-descrs))
                    (error-descrs
                     (mapcar (lambda (descr)
                               (reduce-descr (ti-copy-descr descr)))
                             return-descrs)))
               (notify-type-clash1 fun error-descrs)
               return-descrs))
            (t new-descrs)))))

(defun application-subs-check-%void (formal-descr actual-descr)
  (let ((subs (application-subs formal-descr actual-descr)))
    (if (%void-type-p (get-result-type actual-descr))
        (let ((equ (get-last-substitution
                    subs
                    (vector-ref (?type-vec actual-descr) 0))))
          (ti-format t "~%Notice: formal result type converted to %void")
          (set-right-expr equ (%void-type))))
    subs))

;;; Answer a list of methods that can be bound to the generic function call
;;; with actual-descrs. Algorithm: Are there any methods with true
;;; super domain types, then take them. Otherwise, take all methods with
;;; non-conflicting (meet) domain types.
(defun select-methods (fun actual-descrs)
  (let ((method-subset
         (remove-if (lambda (method)
                      (let* ((method-fun (?fun method))
                             (type-descr (range&domain-descr method-fun)))
                        (if (null? (?type-descr method-fun)) ; to remove!
                            (setf (?type-descr method-fun) type-descr))
                        (member-if-not (lambda (descr)
                                         (sub-descr-p descr type-descr))
                                       actual-descrs)))
                    (?method-list fun))))
    (if method-subset    ;; any methods called for sure?
        (reduce-method-subset method-subset (?method-list fun))
      (remove-if-not (lambda (method)
                       (unify-descrs&descrs
                        actual-descrs (list (?type-descr (?fun method)))))
                     (?method-list fun)))))

;;; Remove all methods with true super domain types compared to any other
;;; method in the list.
(defun reduce-method-subset (methods origin-methods)
  (let ((new-subset (remove-duplicates (sort methods #'sub-method-p)
                                       :from-end t :test #'sub-method-p)))
    (remove-if-not (lambda (origin-method)
                     (member-if (lambda (method)
                                  (sub-method-p origin-method method))
                                new-subset))
                   origin-methods)))

(defun sub-method-p (method1 method2)
  (sub-descr-p (?type-descr (?fun method1))
               (?type-descr (?fun method2))))

;; Answer whether agrument types of an type descriptor are subtype
;; expressions of an other; NOTE: result type is not considered!
(defun sub-descr-p (descr1 descr2)
  (let ((arity (- (length (?type-vec descr1)) 1))
        (ok t))
    (dotimes (i arity)
             (let ((j (+ i 1)))
               (cond ((null? (subtype-expr-p (get-arg-type descr1 j)
                                            (get-arg-type descr2 j)))
                      (setq i arity)
                      (setq ok ())))))
    ok))

;;;-----------------------------------------------------------------------------
;;; CONVERSIONS
;;;-----------------------------------------------------------------------------

;; Replace the type variables of the type vector of an descriptor with classes.
(defun convert-to-sys-type-vec (descr)
  (let ((vec (?type-vec descr))
        (subs (?type-vars descr)))
    (dovector (var i vec)
              (setf (vector-ref vec i)
                    (check-if-abstract-class (type-expr-to-class var subs))))
    (setf (?type-vars descr) ())
    vec))

;; Convert a type expression to a LZS class using type variable substitutions.
(defgeneric type-expr-to-class (expr subs))

(defmethod type-expr-to-class (expr subs)
  (ti-format t "~%Warning: vector elements are already converted to classes")
  expr)

(defmethod type-expr-to-class ((expr <type-var>)
                               (subs <type-var-substitutions>))
  (type-expr-to-class (convert-to-atomic-type expr subs) ()))

(defmethod type-expr-to-class ((expr <slot-id>)
                               subs)
  (?slot-name expr))

(defmethod type-expr-to-class ((expr <atomic-type>)
                               subs)
  (let ((lattice-type (compute-normalized-lattice-type (?name expr))))
    (cond (lattice-type
           (let ((class (get-lattice-type-class lattice-type)))
             (if class
                 (get-most-specialized-class class)
               %object)))
          (t (ti-format t "~%Warning: can't find class to atom ~A"
                        (ti-print-string-no-cr expr))
             %object))))

(defgeneric get-most-specialized-class (class))

(defmethod get-most-specialized-class ((class <class-def>))
  class)

(defmethod get-most-specialized-class ((class <abstract-class-def>))
  (let ((subs (~class-subclasses class)))
    (if (and (= (length subs) 1)        ; one subclass?
             (class-sealed-p class))    ; the one and only?
        (get-most-specialized-class (car subs))
      class)))

;; Call different statistics-function for abstract and non-abstract classes.
(defgeneric check-if-abstract-class (class))

;; Just return the slot name.
(defmethod check-if-abstract-class ((obj <symbol>))
  obj)

(defmethod check-if-abstract-class ((class <class-def>))
  (ti-statistics *inferred-classes-key*)
  class)

(defmethod check-if-abstract-class ((class <abstract-class-def>))
  (call-next-method)
  (ti-statistics *inferred-abstract-classes-key*)
  class)

;; Substitute all type vars in the arg/result type vec by atomic types.
(defun convert-to-atomic-descr (descr)
  (let* ((vec (?type-vec descr))
         (subs (?type-vars descr)))
    (dovector (var i vec)
              (setf (vector-ref vec i)
                    (convert-to-atomic-type var subs)))
    (setf (?type-vars descr) ()))      ; substitutions no longer used
  descr)

;; Remove all indirect references via type variables.
(defun convert-to-atomic-subs (descr)
  (let ((subs (?type-vars descr))
        (new-subs (make <type-var-substitutions>)))
    (dolist (equ (?equations subs))
            (let* ((var (?left-expr equ))
                   (expr (convert-to-atomic-type var subs (previous-subs descr))))
              (add-substitution new-subs var expr)))
    (setf (?type-vars descr) new-subs)
    descr))

;; Remove all indirect references via type variables and remove all variables
;; that do not occur in the type vec.
(defun convert-to-atomic-subs-min (descr)
  (let ((subs (?type-vars descr))
        (new-subs (make <type-var-substitutions>))
        (vec (?type-vec descr)))
    (dovector (vec-var i vec)
              (let ((expr (convert-to-atomic-type vec-var subs (previous-subs descr))))
                (add-substitution new-subs vec-var expr)))
    (setf (?type-vars descr) new-subs)
    descr))

;; Generic function is defined in ti-eqs.
(defmethod convert-general-to-%object-type (descrs)
  (if descrs
      (let ((subs (?type-vars (car descrs))))
        (convert-general-to-%object-type subs)
        (convert-general-to-%object-type (cdr descrs)))))

;;;-----------------------------------------------------------------------------
;;; CHECKING RESULT TYPE
;;;-----------------------------------------------------------------------------

;;; Check if result type of descr is supertype of all result types of descrs.
(defun check-result-subtypes (descrs descr)
  (dolist (descr1 descrs)
          (if (null? (meet-result-types-p descr1 descr))
              (notify-type-clash2 (get-result-type descr)
                                  (get-result-type descr1)))))

;;; Answer whether result type of descr1 is subtype of result type of descr2.
(defun meet-result-types-p (descr1 descr2)
  (let ((result1 (get-result-type descr1))
        (result2 (get-result-type descr2)))
    (or (subtype-expr-p result2 (%void-type)) ; void fits to all types
        (or (subtype-expr-p result1 result2)
            (cond ((meet-type-exprs-p result1 result2)
                   (let ((fun (analysed-fun)))
                     #+ :cmu fun
                     (ti-format
                      t
                      "~%Warning: type check necessary for ~A function ~A:~A)"
                      (funtype-of fun) (?module-id fun) (name-of fun))
                     (ti-format t "~%Result type ~A more general than ~A."
                                (ti-print-string-no-cr result1)
                                (ti-print-string-no-cr result2)))
                   t)
                  (t ()))))))

;;; Check if first type expression is subtype of second type expression.
(defun check-subtype-exprs (expr1 expr2)
  (if (subtype-expr-p expr1 expr2)
      expr1
    (notify-type-clash3 expr1 expr2)))

;;;-----------------------------------------------------------------------------
;;; JOIN RESULT TYPES
;;;-----------------------------------------------------------------------------

;; Answer the joined types of the result types of a list of type descrs.
(defun join-result-types (descrs)
  (if descrs
      (let ((result-type (get-result-type (car descrs))))
        (dolist (descr (cdr descrs))
                (setq result-type
                      (join-type-exprs result-type (get-result-type descr))))
        result-type)
    (progn
      (ti-format t "~%Warning: trying to join an empty list of descripors")
      (general-type))))

;;; Set the result types of a type descriptor to a union of all result types.
(defun set-joined-result-types (descrs)
  (let ((new-result-type (join-result-types descrs)))
    (dolist (descr descrs)
            (set-result-type descr new-result-type)))
  descrs)

;;;-----------------------------------------------------------------------------
;;; ACCESSING TYPE DESCRIPTORS
;;;-----------------------------------------------------------------------------

(defun get-descr-type (descr index)
  (vector-ref (?type-vec descr) index))

(defgeneric set-descr-type (descr index new-expr))

(defmethod set-descr-type ((descr <type-descr>)
                           (index <integer>); *IM* 01.03.94
                           (new-expr <type-expr>))
  (let ((new-var (new-type-var)))
    (setf (vector-ref (?type-vec descr) index) new-var)
    (add-substitution (?type-vars descr) new-var new-expr)))

(defmethod set-descr-type ((descr <type-descr>)
                           (index <integer>); *IM* 01.03.94
                           (new-expr <type-var>))
  (let ((vec (?type-vec descr)))
    (if (contains-type-var-p vec new-expr)
        (call-next-method)
      (setf (vector-ref vec index) new-expr))))

;; Set the result type of a type descriptor.
(defun set-result-type (descr new-expr)
  (let ((new-result-var (new-type-var)))
    (setf (vector-ref (?type-vec descr) 0) new-result-var)
    (add-substitution (?type-vars descr) new-result-var new-expr))
  descr)

;; Set the result type of a type descriptor and remove unused type vars.
(defun set-result-type-min (descr new-expr)
  (set-result-type descr new-expr)
  (reduce-descr descr)
  descr)

;; Get the atomic result type of a type descriptor.
(defun get-arg-type (descr index)
  (let ((prev-descr (?t-descr-before descr))
        (expr (vector-ref (?type-vec descr) index))
        (subs (?type-vars descr)))
    (if (and prev-descr (atom? prev-descr))
        (convert-to-atomic-type expr subs (?type-vars prev-descr))
      (convert-to-atomic-type expr subs))))

;; Get the atomic result type of a type descriptor.
(defun get-result-type (descr)
  (get-arg-type descr 0))

;; Answer the substitutions of the previous type descriptor.
(defun previous-subs (descr)
  (let ((prev-descr (?t-descr-before descr)))
    (if (or (null? prev-descr)           ; no prev descr?
            (cons? prev-descr))         ; more than one prev descr?
        (make <type-var-substitutions>) ; prev subs already used
      (?type-vars prev-descr))))

;;; Set the type var substitutions of the type descriptor before in descr.
(defun get-previous-subs (descr)
  (let ((prev-descr (?t-descr-before descr)))
    (if prev-descr
        (if (atom? prev-descr)
            (let ((prev-subs (ti-copy-subs (?type-vars prev-descr))))
              (append-substitutions (?type-vars descr) prev-subs))
          (ti-format t "~%Notice: more than one previous descriptor"))
      (ti-format2 t "~%Notice: no previous descriptor available (get)")))
  descr)

;;; Set one of the new type var substitutions of the type descriptor before.
(defun set-previous-subs (descr)
  (let ((prev-descr (?t-descr-before descr)))
    (if prev-descr
        (setf (?new-type-vars prev-descr)
              (cons (?type-vars descr) (?new-type-vars prev-descr)))
      (ti-format t "~%Warning: no previous descriptor available (set)")))
  descr)

;;; Set signature of a function and reduce/join it if necessary.
(defun set-signature (fun descrs)
  (ti-statistics *inferred-signature-key*)
  (specialize-recursive-descrs descrs)
  (let ((copied-descrs (mapcar #'ti-copy-descr descrs)))
    (if (or (> (length copied-descrs) *max-signature-descrs*)
            ;;          (> 1 (abs (?arg-num fun)))     ; this or the next line!
            (> 2 (length (?type-vec (car descrs)))))
        (let ((first-descr (car copied-descrs)))
          (join-descrs-min first-descr (cdr copied-descrs))
          (ti-statistics *joined-signature-descrs-key*)
          (setf (?t-descr-before first-descr) ())
          (setq copied-descrs (list first-descr))))
    (let ((range&domain (?range-and-domain fun))
          (max-domain-type ()))
      (cond ((null? range&domain)
             ;;           (convert-general-to-%object-type copied-descrs) ; EuLisp function
             (setq max-domain-type (%object-type)))
            (t             ;; Tail function
             (setq max-domain-type
                   (class-as-type-expr (vector-ref range&domain 0)))))
      (dolist (descr copied-descrs)
              (specialize-result-type descr max-domain-type)))
    (if *use-compound-types*
        (mapc #'reset-write-access-stamps copied-descrs)
      (setq copied-descrs
            (delete-if-not #'convert-all-compound-types copied-descrs)))
    (setf (?signature fun) copied-descrs)
    ;;    (if (null? (?range-and-domain fun))
    ;;      (format t "~%~%-- NEW SIGNATURE~A" (ti-print-string fun)))
    ))

;;; Set signature of a function from a list of classes.
(defun set-signature-from-classes (fun classes)
  (new-fun-with-defined-signatures fun)
  (setf (?signature fun)
        (list (apply #'filled-formal-descr (mapcar #'class-as-type-expr classes))))
  (ti-format t "~%~%-- NEW SIGNATURE~A" (ti-print-string fun)))

;;; Set signature of a function from a list of classes.
(defun set-predicate-signature (fun class)
  (new-fun-with-defined-signatures fun)
  ;; The code of the default atomic expr of lattice type %object has to
  ;; be updated because set-predicate-signature may be called before all
  ;; modules are loaded (i.e. before all defined classes are included
  ;; into the lattice).
  (setf (?code  (%object-type)) (?code *%object*))
  (let ((class-type (class-as-type-expr class))
        (not-class-type
         (compute-to-atom (list ^and *%object*
                                (list ^not (?lattice-type class)))))
        (<null>-type (<null>-type))
        (not-<null>-type
         (compute-to-atom (list ^and *%object* (list ^not *<null>*)))))
    (setf (?signature fun)
          (list (filled-formal-descr <null>-type not-class-type)
                (filled-formal-descr not-<null>-type class-type))))
  (ti-format t "~%~%-- NEW (predicate) SIGNATURE~A" (ti-print-string fun)))

;;;-----------------------------------------------------------------------------
;; Remove all unused type var substitutions of a given descr.
(defun reduce-descr (descr)
  (let ((new-subs (make <type-var-substitutions>)))
    (ti-format2 t "~%before reduce ~A " (ti-print-string descr))
    (reduce-substitutions (?type-vars descr) new-subs (?type-vec descr))
    (setf (?type-vars descr) new-subs)
    (ti-format2 t "~%after reduce ~A " (ti-print-string descr)))
  descr)

;;; Specialize recursive result types with joined non-recursive result types.
(defun specialize-recursive-descrs (descrs)
  (let* ((selection (select-recursive-descrs descrs))
         (rec-descrs (car selection))
         (non-rec-descrs (cdr selection)))
    (if (and rec-descrs non-rec-descrs)
        (let ((new-result-type (join-result-types (cdr selection))))
          (dolist (descr rec-descrs)
                  (set-result-type-min descr new-result-type)))))
  descrs)

;; Answer a pair with recursive descrs as car and non-recursive descrs as cdr.
(defun select-recursive-descrs (descrs)
  (let ((rec-descrs ())
        (non-rec-descrs ()))
    (dolist (descr descrs)
            (if (recursive-descr-p descr)
                (setq rec-descrs (cons descr rec-descrs))
              (setq non-rec-descrs (cons descr non-rec-descrs))))
    (cons rec-descrs non-rec-descrs)))

;; Answer whether a type descriptor is recursive or not.
(defgeneric recursive-descr-p (descr))

(defmethod recursive-descr-p ((descr <type-descr>))
  ())

(defmethod recursive-descr-p ((descr <recursive-type-descr>))
  t)

;; Join a type descriptor with a list of type descriptors.
(defun join-descrs (descr descrs)
  (get-previous-subs descr)
  (ti-format2 t "~%before join ~A " (ti-print-string (list descr descrs)))
  (convert-to-atomic-subs descr)
  (ti-format2 t "~%before join+ ~A " (ti-print-string (list descr descrs)))
  (dolist (descr2 descrs)
          (get-previous-subs descr2)
          (convert-to-atomic-subs descr2)
          (join-two-descrs descr descr2))
  (setf (?t-descr-before descr)
        (cons (?t-descr-before descr) (mapcar #'?t-descr-before descrs)))
  (ti-format2 t "~%after join ~A " (ti-print-string descr))
  descr)

;; Join two type descriptors with atomic substitutions.
(defun join-two-descrs (descr1 descr2)
  (setf (?type-vars descr1)
        (join-substitutions (application-subs descr2 descr1 t)
                            (?type-vars descr1)))
  descr1)

;; Join a type descriptor with a list of type descriptors; the type
;; substitutions contain only those variables that occur in the type vec.
(defun join-descrs-min (descr descrs)
  (get-previous-subs descr)
  (ti-format2 t "~%before join ~A " (ti-print-string (list descr descrs)))
  (convert-to-atomic-subs-min descr)
  (dolist (descr2 descrs)
          (get-previous-subs descr2)
          (convert-to-atomic-subs-min descr2)
          (join-two-descrs-min descr descr2 0))
  (ti-format2 t "~%after join ~A " (ti-print-string descr))
  (setf (?t-descr-before descr) ())
  descr)

;; Join two type descriptors with atomic substitutions; the type substitutions
;; contain only those variables that occur in the type vec.
(defun join-two-descrs-min (descr1 descr2 index)
  (if (< index (length (?type-vec descr1)))
      (let* ((var1 (vector-ref (?type-vec descr1) index))
             (var2 (vector-ref (?type-vec descr2) index))
             (equ1 (get-substitution (?type-vars descr1) var1))
             (equ2 (get-substitution (?type-vars descr2) var2)))
        (if (and equ1 equ2)
            (let ((expr1 (?right-expr equ1))
                  (expr2 (?right-expr equ2)))
              (if (null? (%void-type-p expr1))
                  (set-right-expr equ1 (join-type-exprs expr1 expr2)))
              (join-two-descrs-min descr1 descr2 (+ index 1))))))
  descr1)

;; Check whether the result type of a descriptor corresponds to the defined
;; result type (type-expr). If the defined result type in %void update the
;; descriptor.
(defun specialize-result-type (descr type-expr)
  (let* ((result-equ (get-last-substitution (?type-vars descr)
                                            (vector-ref (?type-vec descr) 0)))
         (result-type (?right-expr result-equ)))
    (if (null? (subtype-expr-p result-type type-expr))
        (if (%void-type-p type-expr)
            (set-result-type-min descr type-expr)
          (let ((domain-type
                 (meet-type-exprs result-type type-expr)))
            (cond (domain-type
                   (set-right-expr result-equ domain-type)
                   (format t "!")
                   (ti-format t "~%Warning: result type check necessary?"))
                  (t
                   (notify-type-clash2 type-expr (get-result-type descr))
                   (set-right-expr result-equ type-expr))))))))

(defun specialize-descrs (descrs min-descr)
  (let (
        ;;      (origin-min-descr (ti-copy-descr min-descr))
        ;;      (origin-descrs (mapcar #'ti-copy-descr descrs))
        )
    (convert-to-atomic-descr min-descr)
    (cond ((specialize-descrs2 descrs min-descr)
           ;;         (format t "~%~%specialize-descr~%")
           ;;         (ti-short-write t origin-descrs)
           ;;         (format t "~%with~%")
           ;;         (ti-short-write t origin-min-descr)
           ;;         (format t "~%to~%")
           ;;         (ti-short-write t descrs)
           )))
  descrs)

;;; The specialized flag is only used for printout in specialize-descr.
(defun specialize-descrs2 (descrs min-descr)
  (let ((specialized ()))
    (dolist (descr descrs)
            (if (specialize-descr descr min-descr)
                (setq specialized t)))
    specialized))

;;; The specialized flag is only used for printout in specialize-descr.
(defun specialize-descr (descr min-descr)
  (let ((vec (?type-vec descr))
        (subs (?type-vars descr))
        (vec-min (?type-vec min-descr))
        (specialized ()))
    (dotimes (i (length vec))
             (let ((expr-min (vector-ref vec-min i))
                   (equ (get-last-substitution subs (vector-ref vec i))))
               (cond ((null? (subtype-expr-p (?right-expr equ) expr-min))
                      (set-right-expr equ expr-min)
                      (setq specialized t)))))
    specialized))

;;;-----------------------------------------------------------------------------
;;; NOTIFYING TYPE CLASHES
;;;-----------------------------------------------------------------------------

(defun notify-type-clash1 (fun error-descrs)
  (if (< (?pass fun) 3) ; not 2nd analysis for global optimization
      (let ((signature (or (?signature fun) (?type-descr fun))))
        (format t "~% ------------------ warning ------------------------")
        (format t "~%Type clash analysing ~A function ~A::~A~%"
                (funtype-of (analysed-fun))
                (?module-id (analysed-fun))
                (name-of (analysed-fun)))
        (format t "The ~A function ~A::~A with range and domain types~%   "
                (funtype-of fun)
                (?module-id fun)
                (name-of fun))
        (ti-short-write t signature)
        (format t "~%is called with ~%   ")
        (ti-short-write t error-descrs)
        (format t "~%continuing with~%   ")
        (ti-short-write t signature)
        (format t "~% ---------------------------------------------------")
        (format t "~%There might be some constraints on argument and ")
        (format t "result types via type variables")
        (format t "~%that are not displayed here; see file eu2c.config.")
        (format t "~% ---------------------------------------------------")
        (ti-error))))

(defun notify-type-clash2 (defined-result-type result-type)
  (format t "~% ------------------ warning ------------------------")
  (format t "~%Defined result type of ~A function "
          (funtype-of (analysed-fun)))
  (format t "~A::~A"
          (?module-id (analysed-fun))
          (name-of (analysed-fun)))
  (format t "~%   ~A" (ti-print-string-no-cr defined-result-type))
  (format t "~%does not correspond to the analysed result type")
  (format t "~%   ~A" (ti-print-string-no-cr result-type))
  (format t "~%continuing with result type")
  (format t "~%   ~A" (ti-print-string-no-cr defined-result-type))
  (format t "~% ---------------------------------------------------")
  (ti-error))

(defun notify-type-clash3 (type-expr1 type-expr2)
  (format t "~% ------------------ warning ------------------------")
  (format t "~%Assignment in ~A function "
          (funtype-of (analysed-fun)))
  (format t "~A::~A"
          (?module-id (analysed-fun))
          (name-of (analysed-fun)))
  (format t "~%~%with following types")
  (format t "~%   ~A <-> ~A"
          (ti-print-string-no-cr type-expr1)
          (ti-print-string-no-cr type-expr2))
  (format t "~% ---------------------------------------------------")
  (ti-error))

;;;-----------------------------------------------------------------------------
;;; WRITING TYPE DESCRIPTORS SHORTLY
;;;-----------------------------------------------------------------------------

(defgeneric ti-short-write (stream obj))

(defmethod ti-short-write (stream descrs)
  (if (and *ti-short-print* (cons? descrs))
      (ti-short-write stream (join-descrs-min (ti-copy-descr (car descrs))
                                              (cdr descrs)))
    (ti-write stream descrs)))

(defmethod ti-short-write (stream
                           (descr <type-descr>))
  (let ((arity (- (length (?type-vec descr)) 1)))
    (dotimes (i arity)
             (let ((expr (get-arg-type descr (+ i 1))))
               (ti-write stream expr)
               (if (< i (- arity 1))
                   (format stream " * ")))))
  (format stream " -> ")
  (ti-write stream (get-arg-type descr 0)))

(defmethod ti-short-write (stream
                           (fun <fun>))
  (format stream "~A::~A : " (?module-id fun) (name-of fun))
  (ti-short-write stream (?signature fun)))

(defun ti-short-write-methods (stream methods)
  (mapc (lambda (method)
          (terpri)
          (ti-short-write stream (?type-descr (?fun method))))
        methods))


#module-end
