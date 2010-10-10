;;; Copyright 1985 Yale University
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
;;; Title: Stable sort, exports: sort, sort!
;;;  Authors: N Adams, R Kelsey, D Kranz, J Philbin, J Rees, Russell Bradford
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    This material was developed by the T Project at the Yale
;;    University Computer Science Department.  Permission to copy this
;;    software, to redistribute it, and to use it for any purpose is
;;    granted, subject to the following restric- tions and
;;    understandings.
;;    1. Any copy made of this software must include this copyright
;;       notice in full.
;;    2. Users of this software agree to make their best efforts (a) to return
;;       to the T Project at Yale any improvements or extensions that they make,
;;       so that these may be included in future releases; and (b) to inform
;;       the T Project of noteworthy uses of this software.
;;    3. All materials developed as a consequence of the use of this software
;;       shall duly acknowledge such use, in accordance with the usual standards
;;       of acknowledging credit in academic research.
;;    4. Yale has made no warrantee or representation that the operation of
;;       this software will be error-free, and Yale is under no obligation to
;;       provide any services, by way of maintenance, update, or otherwise.
;;    5. In conjunction with products arising from the use of this material,
;;       there shall be no use of the name of the Yale University nor of any
;;       adaptation thereof in any advertising, promotional, or sales literature
;;       without prior written consent from Yale in each case.
;;;-----------------------------------------------------------------------------

(defmodule sort
  (import (level-0)
   export (sort
           sort!))

(defconstant set-cdr! (setter cdr))
(defconstant set-car! (setter car))

;;;-----------------------------------------------------------------------------
;;; (sort l comp) comparator comp, or (sort l) default comparator <
;;;-----------------------------------------------------------------------------
(defun sort (l . comp)
  (cond ((or (null? l)
             (null? (cdr l)))
         l)
        (t
         (online-merge-sort!
          (append l '())              ; copy-list
          (if (null? comp) < (car comp))))))

;;;-----------------------------------------------------------------------------
;;; Destructive sort
;;;-----------------------------------------------------------------------------
(defun sort! (l . comp)
  (cond ((or (null? l)
             (null? (cdr l)))
         l)
        (t
         (online-merge-sort!
          l
          (if (null? comp) < (car comp))))))


;;;-----------------------------------------------------------------------------
;;; The real sort procedure.
;;;-----------------------------------------------------------------------------
;; Elements of L are added to B, a list of sorted
;; lists as defined above.  When all elements of L have been added to B
;; the sublists of B are merged together to get the desired sorted list.

(defun online-merge-sort! (l obj-<)
  (let ((b (cons '() '())))
    (let loop ((l l))
         (cond ((null? l)
                (let doloop ((c (cddr b)) (r (cadr b)))
                     (if (null? c)
                         r
                       (doloop (cdr c) (list-merge! (car c) r obj-<)))))
               (t
                (let ((new-l (cdr l)))
                  (set-cdr! l '())
                  (add-to-sorted-lists l b obj-<)
                  (loop new-l)))))))

;;;-----------------------------------------------------------------------------
;;; X is a list that is merged into B, the list of sorted lists.
;;;-----------------------------------------------------------------------------
(defun add-to-sorted-lists (x b obj-<)
  (let loop ((x x) (b b))
       (let ((l (cdr b)))
         (cond ((null? l)
                (set-cdr! b (cons x '())))
               ((null? (car l))
                (set-car! l x))
               (t
                (let ((y (list-merge! x (car l) obj-<)))
                  (set-car! l '())
                  (loop y l)))))))

;;;-----------------------------------------------------------------------------
;;; Does a stable side-effecting merge of L1 and L2.
;;;-----------------------------------------------------------------------------
(defun list-merge! (l1 l2 obj-<)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((obj-< (car l1) (car l2))
         (real-list-merge! l2 (cdr l1) obj-< l1)
         l1)
        (t
         (real-list-merge! l1 (cdr l2) obj-< l2)
         l2)))

;;;-----------------------------------------------------------------------------
;;; Does the real work of LIST-MERGE!.  L1 is assumed to be non-empty.
;;;-----------------------------------------------------------------------------
(defun real-list-merge! (l1 l2 obj-< prev)
  (let loop ((a l1) (b l2) (prev prev))
       (cond ((null? b)
              (set-cdr! prev a))
             ((obj-< (car a) (car b))
              (set-cdr! prev a)
              (loop b (cdr a) a))
             (t
              (set-cdr! prev b)
              (loop a (cdr b) b)))))

;;;-----------------------------------------------------------------------------
)  ;; End of module sort
;;;-----------------------------------------------------------------------------
