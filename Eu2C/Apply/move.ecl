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
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Horst Friedrich
;;;-----------------------------------------------------------------------------

#module move
(import (level-1
         context
         LZS
         analyse-h
         MZS
         type-propagation
         type-inference)
 syntax (level-1
         simple-programming)
 export (add-move
         add-move1
         add-env))

(defun add-env (var init)
  (setf (dynamic env) (cons (cons var init) (dynamic env))))

(defun add-move (var init type rgloc wgloc)
  (add-env var var)
  (add-move1 var init type rgloc wgloc))

(defun add-move1 (var init type rgloc wgloc)
  (let* ((varvec (make-vector 2))
         (td (empty-actual-descr 1))
         (block (dynamic block))
         (move (make <move>
                     :arg-num 2
                     :var-descr (make <var-descr>
                                      :var-vec varvec
                                      :constant-counter 0)
                     :read-glocs  rgloc
                     :write-glocs wgloc
                     :block block
                     :type-descr td
                     ))

         (type-descr-s (make-move-tds (dynamic typepathes)
                                      init move)))
    (if type
        (progn (set-descr-type td 0 type)
               (set-descr-type td 1 type)
               (check-result-subtypes type-descr-s td))
      ())
    (setf (?type-descr-s move) type-descr-s)
    (dynamic-setq typepathes type-descr-s)
    (setf (dynamic moves) (cons move (dynamic moves)))
    (setf (vector-ref varvec 0) var)
    (setf (vector-ref varvec 1) init)
    (setf (?body block)
          (append-stat (?body block) move))
    (if (or (local-static? var)
            (tempvar? var))
        (setf (?link var)
              (cons (cons move 0) (?link var)))
      ())
    (if (or (local-static? init)
            (tempvar? init))
        (setf (?link init)
              (cons (cons move 1) (?link init)))
      ())))

#module-end
