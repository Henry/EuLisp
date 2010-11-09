;;; Copyright 1997 A. Kind & University of Bath
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
;;; Title: Macros for extras for match
;;;  Library: match
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    See match.em
;;;-----------------------------------------------------------------------------

(defmodule extras0
  (syntax (syntax-0)
   import (level-0))

(defmacro and-let* (vars . body)
  (letfuns ((expand (vars body)
                   (cond
                     ((null? vars)
                      `(progn ,@body))
                     ((cons? vars)
                      (let ((exp (car vars)))
                        (cond
                          ((cons? exp)
                           (cond
                             ((null? (cdr exp))
                              `(and ,(car exp) ,(expand (cdr vars) body)))
                             (t
                              (let ((var (car exp))
                                    (val (cadr exp)))
                                `(let (,exp)
                                   (and ,var ,(expand (cdr vars) body)))))))
                          (t
                           `(and ,exp ,(expand (cdr vars) body))))))
                     (t
                      (error <condition> (fmt "not a proper list~a~%" vars))))))
          (expand vars body)))

;;;-----------------------------------------------------------------------------
)  ;; End of module extras0
;;;-----------------------------------------------------------------------------
