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
;;; Title: definitions for the transformation from LZS to MZS
;;;  Description:
;;;  Authors: Horst Friedrich
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

#module analyse-h
(import ((except (format)
                 level-0)
         dynamic
         lzs
         mzs
         accessors
         (only (assoc
                format)
               common-lisp))
 syntax (level-0
         dynamic
         apply-standard)
 export  (<cast>
          closure
          env
          pathes
          typepathes
          block
          rec-calls
          calls
          tests
          moves
          get-slot-value
          set-slot-value
          $normal
          $data
          $closure
          $funcall
          warning
          *started-and-not-finished-functions* *counter*
          l2m-a
          append-stat
          cons-block
          a-number
          finish-a
          *actual-method-subset*))

;;;-----------------------------------------------------------------------------
;;; Classes
;;;-----------------------------------------------------------------------------
(defstandardclass <cast> ()
  (type :accessor :initarg :initform ()))

;;;-----------------------------------------------------------------------------
;;; global-environments
;;;-----------------------------------------------------------------------------
(defglobal closure  ())
(defglobal env       ())
(defglobal pathes ())
(defglobal typepathes ())
(defglobal block ())

;;;-----------------------------------------------------------------------------
;;; annotations for the function
;;;-----------------------------------------------------------------------------
(defglobal rec-calls ())
(defglobal calls    ())
(defglobal tests ())
(defglobal moves ())
(defglobal get-slot-value ())
(defglobal set-slot-value ())
;; function types
(defconstant $normal 0)
(defconstant $data   1)
(defconstant $closure 2)
(defconstant $funcall -99)

;;;-----------------------------------------------------------------------------
;;; local variables
;;;-----------------------------------------------------------------------------
(defglobal *started-and-not-finished-functions* ())
(defglobal *counter*                               1)

(defun a-number ()
  (let ((n (dynamic *counter*)))
    (setf (dynamic *counter*) (+ (dynamic *counter*) 1))
    n))

;;;-----------------------------------------------------------------------------
;;; generic-function
;;;-----------------------------------------------------------------------------
(defgeneric l2m-a (context lzs-object))

(defun warning (string . args)
  (apply #'format t string args))

(defun append-stat (list ele)
  (if (null? list) (list ele)
    (progn (append-stat1 list (list ele))
           list)))

(defun append-stat1 (list ele)
  (if (null? (cdr list))
      (setf (cdr list) ele)
    (append-stat1 (cdr list) ele)))

(defgeneric finish-a (con var-or-constant))

(defun cons-block (block pathes)
  (if pathes
      (cons (cons block (car pathes))
            (cons-block block (cdr pathes)))
    ()))

(deflocal *actual-method-subset* ())

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
