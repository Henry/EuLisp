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
;;;  Title:
;;;  Description:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

(defmodule syntax-0
  (import (tail
           basic-list
           basic-syntax)
   syntax (tail)
   expose (basic-list
           basic-compare
           basic-syntax))

;;;-----------------------------------------------------------------------------
;;; cond
;;;-----------------------------------------------------------------------------
(defmacro cond clauses
  (if (null? clauses) ()
    (if (null? (cdr (car clauses))) `(or ,(car (car clauses))
                                        (cond ,@(cdr clauses)))
      (if (eq (car (car clauses)) 't) `(progn ,@(cdr (car clauses)))
        `(if ,(car (car clauses))
             (progn ,@(cdr (car clauses)))
           (cond ,@(cdr clauses)))
        ))))

(defmacro and forms
  (if (null? forms) 't
    (if (null? (cdr forms)) (car forms)
      `(if ,(car forms)
           (and ,@(cdr forms))
         ())
      )))

(defmacro when (cond . forms)
  (if (null? forms) ()
    `(if ,cond (progn ,@forms) () )))

(defmacro unless (cond . forms)
  (if (null? forms) ()
    `(if ,cond () (progn ,@forms) )))

(defmacro block (identifier . forms)
  (if (null? forms) ()
    `(let/cc ,identifier ,@forms)))

(defmacro return-from (identifier . form)
  `(,identifier ,(if (null? form) () (car form))))

;;;-----------------------------------------------------------------------------
) ;end of module syntax-0
;;;-----------------------------------------------------------------------------
