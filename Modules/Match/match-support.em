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
;;; Title: Scheme compatibility for match.em
;;;  Library: match
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    See match.em
;;;-----------------------------------------------------------------------------

(defmodule match-support
  (syntax ((except (match-let)
                   syntax-0)
           match0)
   import (level-0)
   export (memq
           boolean?
           ;; How much of this is actually necessary?
           match:error
           match:andmap
           match:syntax-err
           match:set-error
           match:error-control
           match:disjoint-predicates
           match:vector-structures))

;; List-traversing
(defun memq (item collection) (member item collection eq))

;; Type predicates
(defun boolean? (obj) (or (eq obj t) (eq obj '())))

;; IO
(defconstant print-error (lambda (msg val)
                           (error <condition> (fmt "~a ~s~%" msg val))))

;; Match support routines
(deflocal match:error
  (lambda (val . args)
    (do print args)
    (print-error "no matching clause for " val)))

(defun match:andmap (f l)
  (if (null? l)
      (and)
    (and (f (car l)) (match:andmap f (cdr l)))))

(defun match:syntax-err (obj msg)
  (print-error msg obj))

(defun match:set-error (v)
  (setq match:error v))

(deflocal match:error-control 'error)

(defun match:set-error-control (v)
  (setq match:error-control v))

(deflocal match:disjoint-predicates
  (cons 'null
        '(cons?
          symbol?
          boolean?
          number?
          string?
          character?
          function?
          vector?)))

(deflocal match:vector-structures '())

;;;-----------------------------------------------------------------------------
)  ;; End of module match-support
;;;-----------------------------------------------------------------------------
