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
;;; Title:
;;;  Description:
;;    definition of the structures for side-effect analyse and teh function
;;    set-side-effect
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Horst Friedrich
;;;-----------------------------------------------------------------------------

#module side-effects-h
(import (level-1
         accessors
         )

 syntax  (level-1
          apply-standard
          (only (make) simple-programming))

 export (set-read-side-effect
         set-write-side-effect
         <gloc> <fgloc>
         ?gplace ?fun ?glocs
         gloc?
         )
 )

;; class-definitions
;; ------------------

;;accessors
(defgeneric ?gplace (gloc))
(defgeneric ?fun (fgloc))
(defgeneric ?glocs (fgloc))

(defstandardclass <gloc> ()
  (gplace :accessor :initarg :initform ())
  (type   :accessor :initarg :initform ())
  :predicate
  )

(defstandardclass <fgloc> ()
  (fun :accessor :initarg) ; fgloc belongs to this function
  (glocs :accessor :initarg :initform ())
  :predicate
  )

;;definitions and init-forms

(defun set-read-side-effect (fun key value)
  (let ((gloc (make <gloc> :gplace value))
        (fread-gloc (?fread-gloc fun)))
    (if fread-gloc
        (setf (?glocs fread-gloc)
              (cons gloc (?glocs fread-gloc)))
      (setf (?fread-gloc fun)
            (make <fgloc> :fun fun
                  :glocs (list gloc))))
    (setf (?sys-glocs fun) ^t))
  )

(defun set-write-side-effect (fun key value)
  (let ((gloc (make <gloc> :gplace value))
        (fwrite-gloc (?fwrite-gloc fun)))
    (if fwrite-gloc
        (setf (?glocs fwrite-gloc)
              (cons gloc (?glocs fwrite-gloc)))
      (setf (?fwrite-gloc fun)
            (make <fgloc> :fun fun
                  :glocs (list gloc))))
    (setf (?sys-glocs fun) ^t))
  )

#module-end
