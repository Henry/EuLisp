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
;;; Title: EuLisp Level-0 list module
;;;  Authors: Horst Friedrich
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule list
  (import (tail
           apply-level-1
           object-0
           basic-list-0
           (only (copy-list
                  copy-tree)
                 pair)
           (only (deep-copy
                  shallow-copy)
                 copy)
           (only (equal)
                 compare-generic))
   syntax (tail
           (only (and)
                 syntax-0))
   expose ((only (<null>
                  <cons>
                  <list>
                  null?
                  cons
                  car
                  cdr
                  list
                  cons?
                  t)
                 basic-list)
           (only (atom?)
                 pair))
   export (deep-copy
           equal
           shallow-copy))

;;;-----------------------------------------------------------------------------
;;; Convert
;;;-----------------------------------------------------------------------------
(defgeneric (converter <list>)(object))

;;;-----------------------------------------------------------------------------
;;; deep-copy
;;;-----------------------------------------------------------------------------
(defmethod deep-copy ((object <null>)) ())

(defmethod deep-copy ((object <cons>))
  (cons (deep-copy (car object))
        (deep-copy (cdr object))))


;; the default method is in module copy
;; (defmethod deep-copy ((object <object>))
;;   object)

;; (defmethod deep-copy ((object <cons>)) (copy-tree object))

;;;-----------------------------------------------------------------------------
;;; shallow-copy
;;;-----------------------------------------------------------------------------
(defmethod shallow-copy ((object <null>)) ())

(defmethod shallow-copy ((object <cons>))
  (cons (car object)
        (shallow-copy (cdr object))))

;; the default method is in module copy
;; (defmethod shallow-copy ((object <object>))
;;   object)

;; (defmethod shallow-copy ((object <cons>)) (copy-list object))

;;;-----------------------------------------------------------------------------
;;; equal
;;;-----------------------------------------------------------------------------
(defmethod equal ((pair1 <cons>) (pair2 <cons>))
  (equal-cons pair1 pair2))

(defun equal-cons(pair1 pair2)
  (and (equal (car pair1) (car pair2))
       (equal (cdr pair1) (cdr pair2))))

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------
(%annotate-function equal-cons new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <object>))
    ((var var1) (atom? <cons>))
    ((var var2) (atom? <cons>)))))

;;;-----------------------------------------------------------------------------
)  ;; End of module list
;;;-----------------------------------------------------------------------------
