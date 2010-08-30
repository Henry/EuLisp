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
;;;  Title: Operations on Type Expressions for Type Inference
;;;  Description:
;;    This module includes functions to compute the least upper bound (join) and
;;    greatest lower boun (meet) of type expressions.
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Andreas Kind
;;;-----------------------------------------------------------------------------


#module ti-meet-join
(import (ti ti-codes ti-lattice ti-exprs
            messages
            (only (format) common-lisp))
 syntax (ti)
 export (meet-type-exprs-p subtype-expr-p true-subtype-expr-p eq-expr-p
                           meet-type-exprs join-type-exprs joined-type-exprs
                           eval-to-expr compute-to-atom normalize-atomic-type
                           complement-type-expr-of-lattice-type
                           not-%false-type))

;;;-----------------------------------------------------------------------------
;;; MEET TYPE EXPRESSIONS
;;;-----------------------------------------------------------------------------

(defgeneric meet-type-exprs (expr1 expr2))

(defmethod meet-type-exprs ((expr1 <type-expr>)
                            (expr2 <type-expr>))
  (ti-format t "~%Warning: subclass responsibility")
  (ti-error)
  (general-type))

(defmethod meet-type-exprs ((expr1 <atomic-type>)
                            (expr2 <atomic-type>))
  (let ((code1 (?code expr1))
        (code2 (?code expr2)))
    (cond ((general-type-p expr1) expr2)
          ((general-type-p expr2) expr1)
          ((complement-codes-p code1 code2) nil)
          ((subcode-p code1 code2) expr1)
          ((subcode-p code2 code1) expr2)
          (t
           (let ((new-code (meet-codes (?code expr1) (?code expr2))))
             (if (bottom-code-p new-code)
                 nil
               (make <atomic-type>
                     :code new-code
                     :name (list ^and (?name expr1) (?name expr2))
                     :comp-name (or (?comp-name expr1) (?comp-name expr2)))))))))

(defmethod meet-type-exprs ((expr1 <slot-id>)
                            (expr2 <slot-id>))
  (if (eq (?slot-name expr1) (?slot-name expr2))
      expr1
    nil))

(defmethod meet-type-exprs ((expr1 <slot-id>)
                            (expr2 <atomic-type>))
  (if (or (general-type-p expr2)
          (%object-type-p expr2))
      expr1 nil))

(defmethod meet-type-exprs ((expr1 <atomic-type>)
                            (expr2 <slot-id>))
  (meet-type-exprs expr2 expr1))

;;;-----------------------------------------------------------------------------
;;; Answer whether a type expr meets another.
;;;-----------------------------------------------------------------------------

(defgeneric meet-type-exprs-p (expr1 expr2))

(defmethod meet-type-exprs-p ((expr1 <type-expr>)
                              (expr2 <type-expr>))
  (meet-type-exprs expr1 expr2))

(defmethod meet-type-exprs-p ((expr1 <atomic-type>)
                              (expr2 <atomic-type>))
  (meet-codes-p (?code expr1) (?code expr2)))

;;;-----------------------------------------------------------------------------
;;; Answer whether a type expr is subtype of another.
;;;-----------------------------------------------------------------------------

(defgeneric subtype-expr-p (expr1 expr2))

(defmethod subtype-expr-p ((expr1 <type-expr>)
                           (expr2 <type-expr>))
  nil)

(defmethod subtype-expr-p ((expr1 <atomic-type>)
                           (expr2 <atomic-type>))
  (subcode-p (?code expr1) (?code expr2)))

(defmethod subtype-expr-p ((expr1 <slot-id>)
                           (expr2 <slot-id>))
  (eq (?slot-name expr1) (?slot-name expr2)))

;;;-----------------------------------------------------------------------------
;;; Answer whether a type expr is a true subtype of another.
;;;-----------------------------------------------------------------------------

(defgeneric true-subtype-expr-p (expr1 expr2))

(defmethod true-subtype-expr-p ((expr1 <type-expr>)
                                (expr2 <type-expr>))
  nil)

(defmethod true-subtype-expr-p ((expr1 <atomic-type>)
                                (expr2 <atomic-type>))
  (let ((code1 (?code expr1))
        (code2 (?code expr2)))
    (and (subcode-p code1 code2)
         (null? (eq-code-p code1 code2)))))

;;;-----------------------------------------------------------------------------
;;; Answer whether a type expr is equal to another.
;;;-----------------------------------------------------------------------------

(defgeneric eq-expr-p (expr1 expr2))

(defmethod eq-expr-p ((expr1 <type-expr>)
                      (expr2 <type-expr>))
  nil)

(defmethod eq-expr-p ((expr1 <atomic-type>)
                      (expr2 <atomic-type>))
  (eq-code-p (?code expr1) (?code expr2)))

(defmethod eq-expr-p ((expr1 <slot-id>)
                      (expr2 <slot-id>))
  (eq (?slot-name expr1) (?slot-name expr2)))

;;;-----------------------------------------------------------------------------
;;; Join type expressions
;;;-----------------------------------------------------------------------------

(defgeneric join-type-exprs (expr1 expr2))

(defmethod join-type-exprs ((expr1 <type-expr>)
                            (expr2 <type-expr>))
  (ti-format t "~%Warning: subclass responsibility")
  (ti-error)
  (general-type))

(defmethod join-type-exprs ((expr1 <atomic-type>)
                            (expr2 <atomic-type>))
  (let ((code1 (?code expr1))
        (code2 (?code expr2)))
    (cond ((or (general-type-p expr1) (general-type-p expr2)) (general-type))
          ((complement-codes-p code1 code2) (general-type))
          ((subcode-p code1 code2) expr2)
          ((subcode-p code2 code1) expr1)
          (t
           (let ((new-code (join-codes code1 code2)))
             (make <atomic-type>
                   :code new-code
                   :name (list ^or (?name expr1) (?name expr2))
                   :comp-name (or (?comp-name expr1) (?comp-name expr2))))))))

(defmethod join-type-exprs ((expr1 <slot-id>)
                            (expr2 <type-expr>))
  (if (or (general-type-p expr2)
          (%object-type-p expr2))
      expr1 expr2))

(defmethod join-type-exprs ((expr1 <type-expr>)
                            (expr2 <slot-id>))
  (join-type-exprs expr2 expr1))

(defmethod join-type-exprs ((expr1 <slot-id>)
                            (expr2 <slot-id>))
  (cond ((eq (?slot-name expr1) (?slot-name expr2))
         expr1)
        (t
         (ti-format t "~%Warning: can't join different slot names")
         (ti-error)
         expr1)))

(defun joined-type-exprs (exprs)
  (let ((result (car exprs)))
    (dolist (expr (cdr exprs))
            (setq result (join-type-exprs expr result)))
    result))

;;;-----------------------------------------------------------------------------
;;; Creating type expressions.
;;;-----------------------------------------------------------------------------

(defun eval-to-expr (def-list)
  (let ((op-symbol (car def-list))
        (arg-def (cdr def-list)))
    (cond ((eq ^var op-symbol)          ; VAR
           (make <type-var> :id (car arg-def)))
          ((eq ^atom? op-symbol)         ; ATOM
           (eval-to-atom (car arg-def)))
          (t             ;; undefined
           (write-message ^warning "incorrect definition of a type expression:~%~A"
                          def-list) ; *IM* 01.03.94
           (ti-error)
           (general-type)))))

;;; Convert lists like ^(and (not <integer>) <null>) to atoms.
(defun eval-to-atom (def-list)
  (compute-to-atom (convert-def-list-to-expr-name def-list)))

;;; Convet lists like ^(and (not <integer>) <null>) to (^and (^not #..) #..).
(defun convert-def-list-to-expr-name (def-list)
  (if (consp def-list)
      (cons (car def-list)
            (mapcar #'convert-def-list-to-expr-name (cdr def-list)))
    (get-lattice-type def-list)))

;;; Convert lists like (^and (^not #..) #..) to atoms.
(defun compute-to-atom (name)
  (if (consp name)       ;; nested?
      (let ((op-symbol (car name)))
        (if (eq ^not op-symbol)         ; NOT
            (complement-type-expr-of-lattice-type (car (cdr name)))
          (let ((subatom1 (compute-to-atom (car (cdr name))))
                (subatom2 (compute-to-atom (car (cdr (cdr name))))))
            (cond ((eq ^and op-symbol)  ; AND
                   (meet-type-exprs subatom1 subatom2))
                  ((eq ^or op-symbol)   ; OR
                   (join-type-exprs subatom1 subatom2))
                  (t
                   (write-message ^warning "incorrect definition of an atomic type: ~A"
                                  name) ; *IM* 01.03.94
                   (general-type))))))
    (lattice-type-to-atomic-type name)))

;; Answer a a corresponding complement type expr of a lattice type.
(defun complement-type-expr-of-lattice-type (name)
  (if (consp name)       ;;nested?
      (progn
        (write-message ^warning "operator NOT only defined on atomic types: ~A" name) ; *IM* 01.03.94
        (ti-error)
        (general-type))  ;; continue with general type
    (make <atomic-type>
          :code (complement-code (?code name))
          :name (list ^not name)
          :comp-name (?compound name))))

;;;-----------------------------------------------------------------------------

(defun not-%false-type ()
  (compute-to-atom (list ^not *%false*)))


#module-end
