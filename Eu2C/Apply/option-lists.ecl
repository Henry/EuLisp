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
;;;  Title: Functions to work with option lists
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module option-lists

(import (eulisp0
         pair-ext
         list-ext
         (only (format find remove) common-lisp))

 syntax (eulisp0
         ;;(only (when) level-1-eulisp)
         )

 export (map-option-list
         find-option
         get-option
         replace-option-value
         check-options
         mapl-option-list)
 )

;;;-----------------------------------------------------------------------------
;;; errors, should be replaced by condition signalling

(defconstant <undefined-option> '<undefined-option>)
(defconstant <uneven-option-list> '<uneven-option-list>)
(defconstant <mising-options> '<mising-options>)
(defconstant <option-not-found> '<undefined-option>)

(defun option-error (condition . initlist)
  (cond ((eq condition <undefined-option>)
         (format t "~%ERROR: undefined option: ~A"
                 (get-option 'option initlist nil)))
        ((eq condition <uneven-option-list>)
         (format t "~%ERROR: option list with uneven number of elements; last one was ~A"
                 (get-option 'option initlist nil)))
        ((eq condition <mising-options>)
         (format t "~%ERROR: missing options: ~A "
                 (get-option 'options initlist nil)))
        ((eq condition <option-not-found>)
         (format t "~%ERROR: option ~A not found in option list ~A"
                 (get-option 'option initlist nil)
                 (get-option 'options initlist nil)))
        ))
;;;-----------------------------------------------------------------------------

(defun map-option-list (function option-list)
  (cond ((null option-list) nil)
        ((atom (cdr option-list))
         (option-error <uneven-option-list> 'option (car option-list)))
        (t (funcall function (car option-list) (cadr option-list))
           (map-option-list function (cddr option-list)))))

(defun mapl-option-list (function option-list)
  (cond ((null option-list) nil)
        ((atom (cdr option-list))
         (option-error <uneven-option-list> 'option (car option-list)))
        (t (funcall function option-list)
           (mapl-option-list function (cddr option-list)))))

(defun find-option (key option-list error-if-not-found?)
  (cond ((null option-list)
         (when error-if-not-found?
               (option-error <option-not-found> 'option key 'options option-list))
         nil)
        ((atom (cdr option-list))
         (option-error <uneven-option-list> 'option (car option-list))
         nil)
        ((eq key (car option-list))
         (cdr option-list))
        (t (find-option key (cddr option-list) error-if-not-found?))))

(defun get-option (key option-list default)
  (let ((entry (find-option key option-list nil)))
    (if entry
        (car entry)
      default)))

(defun replace-option-value (function key option-list)
  ;;function is applied to the old value and must return the new
  ;;value for the specified option
  ;; the changed option-list is returned
  (let ((entry (find-option key option-list nil)))
    (when entry
          (setf (car entry) (funcall function (car entry))))
    option-list))

(defun check-options (required-options facultative-options multiple-options
                                       option-list)
  (cond ((null option-list)
         (if (null required-options)
             t
           (progn (option-error <mising-options>
                                'options required-options)
                  nil)))
        ((atom (cdr option-list))
         (option-error <uneven-option-list> 'option (car option-list))
         nil)
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
         (option-error <undefined-option> 'option (car option-list))
         (check-options required-options facultative-options multiple-options
                        (cddr option-list))
         nil)))


#module-end
