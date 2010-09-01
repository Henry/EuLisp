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
;;;  Title: Type Equations for Type Inference
;;;  Description:
;;    Type equations are used in descriptors of type schemes. Type equations
;;    allow to define equation systems to describe the dependencies between
;;    argument and result types of polymorphic functions.
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Andreas Kind
;;;-----------------------------------------------------------------------------


#module ti-eqs
(import (ti
         ti-lattice
         ti-exprs
         ti-meet-join
         (only (position-if
                rplaca
                rplacd
                assoc)
               common-lisp))
 syntax (ti)
 export (<type-var-substitutions>
         ?left-expr ?right-expr set-left-expr set-right-expr
         new-type-equation eval-to-equation
         contains-type-var?
         substitute-type-var
         <type-equation-stack> ?equations
         push-type-equation pop-type-equation
         get-substitution get-last-substitution
         add-substitution add-equation
         join-substitutions append-substitutions condense-substitutions
         reduce-substitutions check-equality
         convert-to-atomic-type convert-to-slot-name
         convert-general-to-%object-type))

;;;-----------------------------------------------------------------------------
;;; TYPE EQUATIONS
;;;-----------------------------------------------------------------------------

(defstandardclass <type-equation-stack> ()
                  (equations :accessor :initarg :initform ()))

(defstandardclass <type-var-substitutions> (<type-equation-stack>))

;;; Create a new type equation.
(defun new-type-equation (left-expr right-expr)
  (cons left-expr right-expr))

(defun eval-to-equation (expr-list)
  (new-type-equation (eval-to-expr (car expr-list))
                     (eval-to-expr (car (cdr expr-list)))))

;; Answer the left side of a type equation.
(defun ?left-expr (equ)
  (car equ))

;; Answer the right side of a type equation.
(defun ?right-expr (equ)
  (cdr equ))

;; Modify the left side of a type equation.
(defun set-left-expr (equ expr)
  (rplaca equ expr))

;; Modify the right side of a type equation.
(defun set-right-expr (equ expr)
  (rplacd equ expr))

;;;-----------------------------------------------------------------------------
;;; Substitutions of type variables inside of type equations.
(defun substitute-type-var (eqs         ;<type-equation-stack>
                            var-old     ;<type-var>
                            expr-new)   ;<type-expr>
  (if (null? (eq-type-var? var-old expr-new))
      (dolist (equ (?equations eqs))
              (let ((left-expr (?left-expr equ))
                    (right-expr (?right-expr equ)))
                (if (eq-type-var? left-expr var-old)
                    (set-left-expr equ expr-new)
                  (if (eq-type-var? var-old right-expr)
                      (set-right-expr equ expr-new))))))
  eqs)

;;;-----------------------------------------------------------------------------
;;; Add a type equation to a type equation stack.
(defun push-type-equation (equ-stack    ;<type-equation-stack>
                           equ)         ;<pair>
  (setf (?equations equ-stack)
        (cons equ (?equations equ-stack)))
  equ-stack)

;;; Remove a type equation from a type equation stack.
(defun pop-type-equation (eq-stack)     ;<type-equation-stack>
  (let ((equ (car (?equations eq-stack))))
    (setf (?equations eq-stack)
          (cdr (?equations eq-stack)))
    equ))

;;;-----------------------------------------------------------------------------
;;; SUBSTITUTION ACCESS
;;;-----------------------------------------------------------------------------

;; Answer the type equation that has a special type var at the left side.
(defun get-substitution (subs           ;<type-var-substitutions>
                         var)           ;<type-var>
  (assoc var (?equations subs) :test #'eq-type-var?))

;;; Answer the last equation of a list of equations. The var of the
;;; right expression of the last equation does not occur again in the subs.
(defun get-last-substitution (subs      ;<type-var-substitutions>
                              var)      ;<type-var>
  (let ((equ (get-substitution subs var)))
    (if equ
        (let ((right-expr (?right-expr equ)))
          (if (type-var? right-expr)
              (get-last-substitution subs right-expr)
            equ))
      ())))

(defun add-substitution (subs           ;<type-var-substitutions>
                         var            ;<type-var>
                         expr)          ;<type-expr>
  (push-type-equation subs (new-type-equation var expr)))

;;;-----------------------------------------------------------------------------
;;; MERGING SUBSTITUTIONS
;;;-----------------------------------------------------------------------------

;;; Join subs1 and subs2 and put result to subs2.
(defun join-substitutions (subs1        ;<type-var-substitutions>
                           subs2)       ;<type-var-substitutions>
  ;;  (ti-format t "~%join-subs ~A and ~A"
  ;;            (ti-print-string subs1)
  ;;            (ti-print-string subs2))
  (dolist (equ (?equations subs1))
          (join-equation subs2 (?left-expr equ) (?right-expr equ)))
  subs2)

;;; Answer whether two type vars are set equal in substitutions.
(defun check-equality (subs             ;<type-var-substitutions>
                       var1             ;<type-var>
                       var2)            ;<type-var>
  (if (or (check-equality-fwd subs var1 var2)
          (check-equality-fwd subs var2 var1))
      t
    (let ((equ (get-substitution subs var1)))
      (if (and equ (type-var? (?right-expr equ)))
          (check-equality subs (?right-expr equ) var2)
        ()))))

(defgeneric join-equation (subs var expr))

(defmethod join-equation ((subs <type-var-substitutions>)
                          (var <type-var>)
                          (expr <type-expr>))
  (let ((equ (get-last-substitution subs var)))
    (if equ
        (set-right-expr equ (join-type-exprs expr (?right-expr equ)))
      (add-substitution subs var expr))))

(defmethod join-equation ((subs <type-var-substitutions>)
                          (var <type-var>)
                          (expr <type-var>))
  (if (null? (check-equality subs var expr))
      (let ((equ (get-last-substitution subs var)))
        (if equ
            (let ((right-expr (?right-expr equ)))
              (set-right-expr equ expr)
              (join-equation subs expr right-expr))
          (add-substitution subs var expr)))))

;;;-----------------------------------------------------------------------------
(defgeneric add-equation (subs var expr))

(defmethod add-equation ((subs <type-var-substitutions>)
                         (var <type-var>)
                         (expr <type-expr>))
  (let ((equ (get-last-substitution subs var)))
    (if equ
        (let ((new-expr (meet-type-exprs expr (?right-expr equ))))
          (if new-expr
              (set-right-expr equ new-expr)
            ()))
      (add-substitution subs var expr))))

(defmethod add-equation ((subs <type-var-substitutions>)
                         (var <type-var>)
                         (expr <type-var>))
  (if (null? (check-equality subs var expr))
      (let ((equ (get-last-substitution subs var)))
        (if equ
            (let ((right-expr (?right-expr equ)))
              (set-right-expr equ expr)
              (add-equation subs expr right-expr))
          (add-substitution subs var expr)))
    t))

(defgeneric check-equality-fwd (var1 var2 subs))

(defmethod check-equality-fwd ((subs <type-var-substitutions>)
                               (var1 <type-var>)
                               (var2 <type-var>))
  (if (eq-type-var? var1 var2) t
    (let ((equ (get-substitution subs var1)))
      (if equ
          (check-equality-fwd subs (?right-expr equ) var2)
        ()))))

(defmethod check-equality-fwd ((subs <type-var-substitutions>)
                               (var1 <type-expr>)
                               (var2 <type-expr>))
  ())

;;;-----------------------------------------------------------------------------
;;; Reduce the number of type variables inside of substitutions.
(defun reduce-substitutions (subs1      ;<type-var-substitutions>
                             subs2      ;<type-var-substitutions>
                             vec)       ;<vector>
  (dovector (var i vec)
            (extract-equation var subs1 subs2 vec))
  subs2)

(defun extract-equation (vec-var        ;<type-var>
                         subs1          ;<type-var-substitutions>
                         subs2          ;<type-var-substitutions>
                         vec)           ;<vector>
  (let ((index (position-if (lambda (vec-var2)
                              (and
                               (check-equality-fwd subs1 vec-var vec-var2)
                               (null? (eq-type-var? vec-var vec-var2))))
                            vec)))
    (if index
        (add-substitution subs2 vec-var (vector-ref vec index))
      (add-substitution subs2 vec-var
                        (convert-to-atomic-type vec-var subs1)))))

;;; Append subs1 and subs2 and put result to subs1.
(defun append-substitutions (subs1      ;<type-var-substitutions>
                             subs2)     ;<type-var-substitutions>
  (setf (?equations subs1)
        (append (?equations subs1) (?equations subs2)))
  subs1)

;;;-----------------------------------------------------------------------------
;;; CONVERSION OF TYPE EXPRESSIONS
;;;-----------------------------------------------------------------------------

;;; Convert a type expr to an atomic type.
(defgeneric convert-to-atomic-type (expr subs . other-subs))

(defmethod convert-to-atomic-type ((expr <type-expr>)
                                   subs . other-subs)
  expr)

(defmethod convert-to-atomic-type ((expr <type-var>)
                                   (subs <type-var-substitutions>) . other-subs)
  (let ((equ (get-last-substitution subs expr)))
    (if equ
        (?right-expr equ)
      (if other-subs
          (convert-to-atomic-type expr
                                  (append-substitutions subs (car other-subs))
                                  (cdr other-subs))
        (progn
          (ti-format t "~%Warning: variable var~A not in substitutions"
                     (?id expr))
          (ti-error)
          (general-type))))))

;;; Convert all general types to %object types (see also ti-signature).
(defgeneric convert-general-to-%object-type (subs))

(defmethod convert-general-to-%object-type ((subs <type-var-substitutions>))
  (dolist (equ (?equations subs))
          (let ((expr (?right-expr equ)))
            (if (and (atomic-type? expr) (general-type-p expr))
                (set-right-expr equ (%object-type))))))

;;;-----------------------------------------------------------------------------
;;; CONVERSION OF TYPE EXPRESSIONS TO SLOT NAME
;;;-----------------------------------------------------------------------------

;;; Convert a type expr to a slot name.
(defgeneric convert-to-slot-name (expr subs))

(defmethod convert-to-slot-name ((expr <type-expr>)
                                 subs)
  (ti-format t "~%Warning: type expr cannot be converted to slot name")
  (ti-error)
  ())

(defmethod convert-to-slot-name ((expr <slot-id>)
                                 subs)
  (?slot-name expr))

(defmethod convert-to-slot-name ((expr <type-var>)
                                 (subs <type-var-substitutions>))
  (let ((equ (get-last-substitution subs expr)))
    (cond (equ
           (?slot-name (?right-expr equ)))
          (t
           (ti-format t "~%Warning: type var cannot be converted to slot name")
           (ti-error)
           ()))))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
