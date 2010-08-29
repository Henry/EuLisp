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
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Horst Friedrich
;;;-----------------------------------------------------------------------------

#module setq-form

(import
 (eulisp1
  context
  LZS
  analyse-h
  MZS
  move
  gutter
  accessors
  )

 syntax
 (eulisp1
  simple-programming)

 export
 (setq-form-a) ; con, form
 )


;;--- definitions, exportations, initialization forms


(defun setq-form-a (con form)
  (let ((location (?location form)))
    (setq-form1
     (if (var-ref-p location) (?var location) location)
     (l2m-a (dynamic *arg-context*) (?form form))
     (?write-gloc form)
     (let ((rgloc (?read-gloc form)))
       (if rgloc
           (if (consp rgloc) rgloc (list rgloc))
         ()))))
  )


(defgeneric setq-form1 (var res rgloc wglocs))

(defmethod setq-form1 ((con <named-const>) res rgloc wglocs)
  (add-move1 con res () (if rgloc (list rgloc) ()) wglocs)
  res)

(defmethod setq-form1 ((var <local-static>) (res <tempvar>) rgloc wglocs)
  (if (?closure var)
      (progn (set-closure-var var res (dynamic block)) res)
    ;; subst tempvar by local-static
    (let* ((link (?link res))
           (stat (car (car link)))
           (newlocal (make <local-static> :identifier (?identifier var)
                           :link (?link res))))
      (setf (vector-ref (?var-vec (?var-descr stat)) 0) newlocal)
      (add-env var newlocal)
      newlocal))
  )

(defmethod setq-form1 ((var <local-static>) res rgloc wglocs)
  (if (?closure var)
      (progn
        (set-closure-var var res (dynamic block))
        res)
    (let ((tmpvar (make <tempvar> :tnr (a-number))))
      (add-move1 tmpvar res () (if rgloc (list rgloc) ()) wglocs)
      (add-env var tmpvar)
      tmpvar))
  )

(defmethod setq-form1 ((var <dynamic>) res rgloc wglocs)
  (add-move1 var res () (if rgloc (list rgloc) ()) wglocs)
  res
  )

(defmethod setq-form1 ((var <global-static>) res rgloc wglocs)
  (add-move1 var res () (if rgloc (list rgloc) ()) wglocs)
  res
  )

(defmethod setq-form1 ((var <imported-static>) res rgloc wglocs)
  (add-move1 var res () (if rgloc (list rgloc) ()) wglocs)
  res
  )

#module-end
