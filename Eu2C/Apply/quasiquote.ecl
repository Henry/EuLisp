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
;;;  Title: backquote from feel for apply
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: 
;;;-----------------------------------------------------------------------------



#module quasiquote

(import
 ((except (STRINGP append) eulisp1)
  (only (CADAR
         CADR
         ERROR
         STRINGP
         append)
        common-lisp)
  )

 syntax
 (eulisp1)

 export
 (unquote-constructor)
 )

;; Quasi-quoting

(defun unquote-constructor (x)
  (cond ((atom? x)
         (cond ((or (null? x) (numberp x) (stringp x) (eq x t)) x)
               (t (mkquote x))))

        ((eq (car x) ^unquote) (cadr x))
        ((eq (car x) ^unquote-splicing)
         (error "Illegal use of ,@ marker"))
        ((eqcar (car x) ^unquote-splicing)
         (list ^append (cadar x) (unquote-constructor (cdr x))))
        ;;      ((contains-no-unquote x) (mkquote x))
        (t (list ^cons
                 (unquote-constructor (car x))
                 (unquote-constructor (cdr x))))))

(defun contains-no-unquote (x)
  (cond ((atom? x) t)
        ((or (eq (car x) ^unquote) (eq (car x) ^unquote-splicing))
         nil)
        (t (and (contains-no-unquote (car x))
                (contains-no-unquote (cdr x))))))

(defun mkquote (x) (list ^quote x))

(defun eqcar (a b) (cond ((atom? a) nil) ((eq (car a) b) t) (t nil)))

;; (defmacro quasiquote (dummy form) (unquote-constructor form))


;;(defmacro quasiquote (skel) (unquote-constructor skel))
