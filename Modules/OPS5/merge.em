;;; Copyright 1988 J.Heinanen
;;; Copyright 1995 T. Gardner & University of Bath
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;
;;  Youtoo is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: Merge sort
;;;  Library: OPS5
;;;  Authors: Juha Heinanen, Tracy Gardner
;;;  Description:
;;    Juha Heinanen's merge sort modified to run under euscheme
;;;-----------------------------------------------------------------------------

(defmodule merge
  (syntax (syntax-0
           macros-tag)
   import (level-0
           basic)
   export (merge-sort))

(print "### merge" nl)

(defun merge! (list1 list2 previous less?)
  ; Merges list1 and list2 into cdr of previous
  (if (null? list1)
      ((setter cdr) previous list2)
    (if (null? list2)
        ((setter cdr) previous list1)
      (if (less? (car list2) (car list1))
          (progn
            ((setter cdr) previous list2)
            (merge! list1 (cdr list2) list2 less?))
        (progn
          ((setter cdr) previous list1)
          (merge! (cdr list1) list2 list1 less?))))))

(defun merge-pass! (list-of-lists previous less?)
  ; Two way merges lists in list-of-lists into cdr of previous
  ; merge-pass!
  (when (not (null? list-of-lists))
        (if (null? (cdr list-of-lists))
            ((setter cdr) previous list-of-lists)
          (let ((sorted-list (list '*header-node*)))
            (merge! (car list-of-lists) (cadr list-of-lists) sorted-list less?)
            ((setter cdr) previous (list (cdr sorted-list)))
            (merge-pass! (cddr list-of-lists) (cdr previous) less?)))))

(defun merge-sort (value-list less?)
  ; Sorts value-list according to less?
  ; merge-sort!
  (let ((merged-lists (list '*header-node* '())))
    (let loop ((list-of-lists (map list value-list)))
         (merge-pass! list-of-lists merged-lists less?)
         (if (null? (cddr merged-lists))
             (cadr merged-lists)
           (loop (cdr merged-lists))))))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
