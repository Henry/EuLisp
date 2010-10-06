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
;;; Title: Functions to work with option lists
;;;  Authors: Ingo Mohr, Rainer Rosenmuller
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule option-lists
  (import (tail
           (only (atom?)
                 pair))
   syntax (tail
           (only (cond
                   and
                   when)
                 syntax-i))
   export (find-option
           get-option
           check-options))

;;   (defun map-option-list (function option-list)
;;     (cond ((null? option-list) ())
;;           ((atom? (cdr option-list))
;;            (warn "option list with uneven number of elements; last one was ~A"
;;                  (car option-list)))
;;           (t (funcall function (car option-list) (cadr option-list))
;;              (map-option-list function (cddr option-list)))))

;;   (defun mapl-option-list (function option-list)
;;     (cond ((null? option-list) ())
;;           ((atom? (cdr option-list))
;;            (warn "option list with uneven number of elements; last one was ~A"
;;                  (car option-list)))
;;           (t (funcall function option-list)
;;              (mapl-option-list function (cddr option-list)))))

(defun find-option (key option-list error-if-not-found?)
  (cond ((null? option-list)
         (when error-if-not-found?
               (warn "option ~A not found in option list ~A" key option-list))
         ())
        ((atom? (cdr option-list))
         (warn "option list with uneven number of elements; last one was ~A"
               (car option-list))
         ())
        ((eq key (car option-list))
         (cdr option-list))
        (t (find-option key (cddr option-list) error-if-not-found?))))

(defun get-option (key option-list default)
  (let ((entry (find-option key option-list ())))
    (if entry
        (car entry)
      default)))

;; (defun replace-option-value (function key option-list)
;;   ;;function is applied to the old value and must return the new
;;   ;;value for the specified option
;;   ;; the changed option-list is returned
;;   (let ((entry (find-option key option-list ())))
;;     (when entry
;;       (setf (car entry) (funcall function (car entry))))
;;     option-list))

(defun check-options (required-options facultative-options multiple-options
                                       option-list)
  (cond ((null? option-list)
         (if (null? required-options)
             t
           (progn (warn "missing options: ~A "
                        required-options)
                  ())))
        ((atom? (cdr option-list))
         (warn "option list with uneven number of elements; last one was ~A"
               (car option-list))
         ())
        ((find (car option-list) required-options)
         (check-options (remove (car option-list) required-options)
                        facultative-options multiple-options
                        (cddr option-list)))
        ((find (car option-list) facultative-options)
         (check-options required-options
                        (remove (car option-list) facultative-options)
                        multiple-options
                        (cddr option-list)))
        ((find (car option-list) multiple-options)
         (check-options required-options facultative-options multiple-options
                        (cddr option-list)))
        (t
         (warn "undefined option: ~A" (car option-list))
         (check-options required-options facultative-options multiple-options
                        (cddr option-list))
         ())))

(defun warn (str . args) ())

(defun find (obj list)
  (%member obj list))

(defun remove (obj list)
  (if (eq obj (car list))
      (cdr list)
    (cons (car list) (remove obj (cdr list)))))

(defun cddr (obj) (cdr (cdr obj)))

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------
(%annotate-function
  find-option new-signature
  (((var0 var1 var2 var3)
    ((var var0) (atom? <object>))
    ((var var1) (atom? <object>))
    ((var var2) (atom? <list>))
    ((var var3) (atom? <object>)))))

;;;-----------------------------------------------------------------------------
)  ;; End of module option-lists
;;;-----------------------------------------------------------------------------
