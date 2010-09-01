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

#module slot-value
(import (level-1-eulisp
         context
         LZS
         analyse-h
         MZS
         type-propagation
         type-inference)
 syntax (level-1-eulisp
         simple-programming)
 export (get-slot-value-a
         set-slot-value-a))

(defun get-slot-value-a (con form)
  (let* ((from (l2m-a (dynamic *arg-context*)
                      (?instance form)))
         (res (make <tempvar> :tnr (a-number)))
         (varvec (make-vector 2))
         (td (empty-actual-descr 1))
         (block (dynamic block))
         (var-descr (make <var-descr> :var-vec varvec
                          :constant-counter 0))
         (type-descr-s (inference-get-slot-value
                        (get-slot-tds (dynamic typepathes)
                                      from form)
                        (?slot form))))
    (dynamic-setq typepathes type-descr-s)
    (setf (vector-ref varvec 1) from)
    (setf (vector-ref varvec 0) res)
    (setf (?var-descr form) var-descr) ; *hf* 21.01
    (setf (?type-descr form) td)
    (setf (?type-descr-s form) type-descr-s)
    (setf (?arg-num form) 1)
    (setf (?block form) block)
    (setf (dynamic get-slot-value) (cons form (dynamic get-slot-value)))
    (setf (?body block)
          (append-stat (?body block) form))
    ;; variablen linken
    (setf (?link res)
          (cons (cons form 0) ()))
    (if (or (local-static? from)
            (tempvar? from))
        (setf (?link from)
              (cons (cons form 1) (?link from)))
      ())
    res))

(defun set-slot-value-a (con form)
  (let* ((init (l2m-a (dynamic *arg-context*)
                      (?value form)))
         (from (l2m-a (dynamic *arg-context*)
                      (?instance form)))
         (res (make <tempvar> :tnr (a-number)))
         (varvec (make-vector 3))
         (td (empty-actual-descr 2))
         (block (dynamic block))
         (var-descr (make <var-descr> :var-vec varvec
                          :constant-counter 0))
         (type-descr-s (inference-set-slot-value
                        (set-slot-tds (dynamic typepathes)
                                      from init form)
                        (?slot form))))
    (dynamic-setq typepathes type-descr-s)
    (setf (vector-ref varvec 2) init)
    (setf (vector-ref varvec 1) from)
    (setf (vector-ref varvec 0) res)
    (setf (?var-descr form) var-descr) ; *hf* 21.01
    (setf (?type-descr form) td)
    (setf (?type-descr-s form) type-descr-s)
    (setf (?arg-num form) 2)
    (setf (?block form) block)
    (setf (dynamic set-slot-value) (cons form (dynamic set-slot-value)))
    (setf (?body block)
          (append-stat (?body block) form))
    ;; variablen linken
    (setf (?link res)
          (cons (cons form 0) ()))
    (if (or (local-static? from)
            (tempvar? from))
        (setf (?link from)
              (cons (cons form 1) (?link from)))
      ())
    (if (or (local-static? init)
            (tempvar? init))
        (setf (?link init)
              (cons (cons form 2) (?link init)))
      ())
    res))


#module-end
