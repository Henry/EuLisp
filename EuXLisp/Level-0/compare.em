;;; Copyright 1994 Russell Bradford
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'EuXLisp'
;;;-----------------------------------------------------------------------------
;;
;;  EuXLisp is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  EuXLisp is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: EuLisp Level-0 compare module
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule compare
  (syntax (syntax)
   import (root)
   export (binary=
           =
           binary<
           < > <= >=
           max
           min
           member-alist))

(defgeneric equal (a b))

;; not as in definition, as it says eql
(defmethod equal ((a <object>) (b <object>))
               (%equal a b))

(defgeneric binary< (a b))

(defmethod binary< ((a <number>) (b <number>))
               (%< a b))

(defmethod binary< ((a <char>) (b <char>))
               (char<? a b))

(defmethod binary< ((a <string>) (b <string>))
               (string<? a b))

(defun < (arg . args)
        (cond ((null? args) t)
              ((null? (cdr args)) (binary< arg (car args)))
              ((binary< arg (car args)) (apply < args))
              (t ())))

(defun > (arg . args)
        (cond ((null? args) t)
              ((null? (cdr args)) (binary< (car args) arg))
              ((binary< (car args) arg) (apply > args))
              (t ())))

(defgeneric binary= (a b))

(defmethod binary= ((a <object>) (b <object>))
               (%equal a b))

(defmethod binary= ((a <number>) (b <number>))
               (%= a b))

(defmethod binary= ((a <char>) (b <char>))
               (char=? a b))

(defmethod binary= ((a <string>) (b <string>))
               (string=? a b))

(defun = (arg . args)
        (cond ((null? args) t)
              ((null? (cdr args)) (binary= arg (car args)))
              ((binary= arg (car args))
               (apply = (car args) (cdr args)))
              (t ())))

(defun <= (arg . args)
        (cond ((null? args) t)
              ((null? (cdr args)) (or (binary< arg (car args))
                                      (binary= arg (car args))))
              ((or (binary< arg (car args))
                   (binary= arg (car args))) (apply <= args))
              (t ())))

(defun >= (arg . args)
        (cond ((null? args) t)
              ((null? (cdr args)) (or (binary< (car args) arg)
                                      (binary= arg (car args))))
              ((or (binary< (car args) arg)
                   (binary= arg (car args))) (apply <= args))
              (t ())))

(defun max (arg . args)
        (cond ((null? args) arg)
              ((null? (cdr args))
               (if (binary< arg (car args))
                   (car args)
                 arg))
              (t (apply max (max arg (car args)) (cdr args)))))

(defun min (arg . args)
        (cond ((null? args) arg)
              ((null? (cdr args))
               (if (binary< arg (car args))
                   arg
                 (car args)))
              (t (apply min (min arg (car args)) (cdr args)))))

(defun member-alist (obj list . comp)
        (member-alist-loop obj list (if (null? comp) eq (car comp))))

(defun member-alist-loop (obj list comp)
        (cond ((atom? list) ())
              ((comp obj (caar list)) (car list))
              (t (member-alist-loop obj (cdr list) comp))))

;;;-----------------------------------------------------------------------------
)  ;; End of module compare
;;;-----------------------------------------------------------------------------
