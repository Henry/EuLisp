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
;;;  Title: A Modified Unification Algorithm for Type Inference
;;;  Description:
;;    Insert equations of type expressions into a given set of variable
;;    substitutions.
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Andreas Kind
;;;-----------------------------------------------------------------------------

#module ti-unify
(import (lzs
         mzs
         ti
         ti-codes
         ti-lattice
         ti-exprs
         ti-meet-join
         ti-eqs
         ti-write
         ti-copy)
 syntax (ti)
 export (unify))

;;;-----------------------------------------------------------------------------
;;; GENERAL UNIFICATION
;;;-----------------------------------------------------------------------------

(defgeneric unify (stack subs))

(defmethod unify ((stack <null>)        ;empty stack
                  (subs <type-var-substitutions>))
  subs)   ;;unification succeeded

(defmethod unify ((stack <pair>)        ;stack not empty
                  (subs <type-var-substitutions>))
  (let* ((actual-equ (car stack))
         (left-expr (?left-expr actual-equ))
         (right-expr (?right-expr actual-equ)))
    (cond ((unify-exprs left-expr right-expr subs) ;unify first equation
           (ti-format2 t "~%new subs ~A" (ti-print-string subs))
           (unify (cdr stack) subs))    ;unification goes ahead
          (t nil))))     ;;unification failed

;;;-----------------------------------------------------------------------------
;;; UNIFICATION OF TYPE EXPRESSIONS
;;;-----------------------------------------------------------------------------

(defgeneric unify-exprs (expr1 expr2 subs))

(defmethod unify-exprs ((expr1 <atomic-type>)
                        (expr2 <atomic-type>)
                        (subs <type-var-substitutions>))
  (meet-type-exprs expr1 expr2))

(defmethod unify-exprs ((expr1 <slot-id>)
                        (expr2 <slot-id>)
                        (subs <type-var-substitutions>))
  (meet-type-exprs expr1 expr2))

(defmethod unify-exprs ((expr1 <slot-id>)
                        (expr2 <atomic-type>)
                        (subs <type-var-substitutions>))
  (meet-type-exprs expr1 expr2))

(defmethod unify-exprs ((expr1 <atomic-type>)
                        (expr2 <slot-id>)
                        (subs <type-var-substitutions>))
  (meet-type-exprs expr1 expr2))

;;;-----------------------------------------------------------------------------
(defmethod unify-exprs ((expr1 <type-var>)
                        (expr2 <type-expr>)
                        (subs <type-var-substitutions>))
  (add-unify-equation subs expr1 expr2))

(defmethod unify-exprs ((expr1 <type-expr>)
                        (expr2 <type-var>)
                        (subs <type-var-substitutions>))
  (add-unify-equation subs expr2 expr1))

;;;-----------------------------------------------------------------------------
(defgeneric add-unify-equation (subs var expr))

(defmethod add-unify-equation ((subs <type-var-substitutions>)
                               (var <type-var>)
                               (expr <type-expr>))
  (let ((equ (get-last-substitution subs var)))
    (if equ
        (let ((new-expr (unify-exprs expr (?right-expr equ) subs)))
          (if new-expr
              (set-right-expr equ new-expr)
            nil))
      (add-substitution subs var expr))))

(defmethod add-unify-equation ((subs <type-var-substitutions>)
                               (var <type-var>)
                               (expr <type-var>))
  (if (null? (check-equality subs var expr))
      (let ((equ (get-last-substitution subs var)))
        (if equ
            (let ((right-expr (?right-expr equ)))
              (set-right-expr equ expr)
              (add-unify-equation subs expr right-expr))
          (add-substitution subs var expr)))
    t))


;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
