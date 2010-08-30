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
;;;  Notes:  change pass from 3 to 5
;;;  Requires:
;;;  Problems:
;;;  Authors: Horst Friedrich
;;;-----------------------------------------------------------------------------

#module types-hand-on
(import (LZS
         MZS
         type-propagation
         debugging
         type-inference
         null
         (only (specialize-descrs
                get-previous-subs)
               ti-signature)
         name-of-fun
         (only (?arg-num-fun)
               function-label)
         (only (get-functions-used-in-literals)
               expand-literal)
         (only (error
                append
                format
                mapc)
               common-lisp))
 syntax (level-1-eulisp)
 export (types-hand-on-modules))


(defun types-hand-on-modules (m-list)
  (if (null? m-list)
      (types-hand-on-funs (get-functions-used-in-literals))
    (progn
      ;;(pause-types-hand-on-modules)
      (types-hand-on-module (car m-list))
      (types-hand-on-modules (cdr m-list)))))

(defun types-hand-on-module (modul)
  (let ((tlf (?toplevel-forms modul)))
    (if tlf (types-hand-on-fun tlf) ())
    (types-hand-on-funs (?fun-list modul))))

(defun types-hand-on-funs (fun-list)
  (if (null? fun-list) ()
    (progn (types-hand-on-fun (car fun-list))
           (types-hand-on-funs (cdr fun-list)))))

(defvar cur-fun ()) ; currend function

(defun types-hand-on-fun (fun)
  (start-analyse-fun fun);; for debugging
  ;;  (format t "+")
  (if (eq (?pass fun) 3)
      (dynamic-let ((typepathes (list (?type-descr fun)))
                    (cur-fun fun))      ; *hf* 11.01
                   (setf (?type-descr-s fun) ())  ; *hf* 11.01
                   (setf (?pass fun) 4)
                   (setf (?start-block (?function-label fun))
                         (walk-block (?start-block (?function-label fun)) ))
                   (setf (?pass fun) 5)
                   fun)
    fun)
  (end-analyse-fun fun))

;; walk-result (?result block) (?out-label block)
(defgeneric walk-result (result out-label))

(defun walk-block (block)
  (setf (?body block) (walk-stats (?body block)))
  (setf (?interface block) (walk-stats (?interface block)))
  (walk-result (?result block) (?out-label block))
  ;; *hf* hier muss das richtige Ergebnis zurueckgegeben werden
  block)

(defun walk-stats (stat-list)
  (if (null? stat-list) ()
    (let ((stat (walk-stat (car stat-list))))
      (if stat (cons stat (walk-stats (cdr stat-list)))
        (walk-stats (cdr stat-list))))))

(defun find-join-label1 (block)
  (let ((result (?result block))
        (out-label (?out-label block)))
    (if (join-label-p out-label) (cons out-label 1)
      (if (test-p result)
          (let* ((then (find-join-label1 (?then-block result)))
                 (else (if then (find-join-label1 (?else-block result)) ())))
            (if else
                (progn
                  (if (eq (car then) (car else)) ()
                    (error "~% differen then and else-blocks "))
                  (if (eq (length (?in-block (car then)))
                          (+ (cdr then) (cdr else)))
                      (find-join-label1 (?out-block (car then)))
                    (cons (car then) (+ (cdr then) (cdr else)))))
              ()))
        ())))
  )

(defun find-join-label (then-block else-block)
  (let* ((then (find-join-label1 then-block))
         (else (if then (find-join-label1 else-block) ())))
    (if else
        (if (and (eq (car then) (car else))
                 (eq (length (?in-block (car then)))
                     (+ (cdr then) (cdr else))))
            (car then)
          ()))))

(defun walk-call (stat)
  (let* ((fun (?function stat))
         (arg-num (?arg-num stat))
         (typedescrs
          (make-actual-type-descr (dynamic typepathes)
                                  stat
                                  (?var-descr stat)
                                  arg-num
                                  ())))
    (types-hand-on-fun fun)
    (setq typedescrs (inference fun typedescrs))
    (specialize-descrs typedescrs (?type-descr stat)) ;ak
    (setf (?type-descr-s stat) typedescrs)
    (dynamic-setq typepathes typedescrs)
    stat))

(defgeneric walk-stat (stat))

(defmethod walk-stat ((stat <call>)) (walk-call stat))
(defmethod walk-stat ((stat <asm>)) (walk-call stat))
(defmethod walk-stat ((stat <last-call>)) (walk-call stat))
(defmethod walk-stat ((stat <last-asm>)) (walk-call stat))

(defmethod walk-stat ((stat <set-slot-value>))
  (let ((typedescrs
         (make-actual-type-descr (dynamic typepathes)
                                 stat (?var-descr stat) 2 ())))
    (setq typedescrs (inference-set-slot-value typedescrs (?slot stat)))
    (specialize-descrs typedescrs (?type-descr stat))
    (setf (?type-descr-s stat) typedescrs)
    (dynamic-setq typepathes typedescrs)
    stat))

(defmethod walk-stat ((stat <get-slot-value>))
  (let ((typedescrs
         (make-actual-type-descr (dynamic typepathes)
                                 stat (?var-descr stat) 1 ())))
    (setq typedescrs (inference-get-slot-value typedescrs (?slot stat)))
    (specialize-descrs typedescrs (?type-descr stat))
    (setf (?type-descr-s stat) typedescrs)
    (dynamic-setq typepathes typedescrs)
    stat))

(defmethod walk-stat ((stat <move>))
  (let ((typedescrs
         (make-actual-type-descr (dynamic typepathes)
                                 stat
                                 (?var-descr stat)
                                 1 ; arg-num
                                 ())))
    (mapc #'get-previous-subs typedescrs)
    ;; ak hier nur im Notfall!!
    (setf (?type-descr-s stat) typedescrs)
    (dynamic-setq typepathes typedescrs)
    stat))

(defmethod walk-result ((result <return>) out-label
                        )
  ;; *hf* 11.01 start changes
  (let ((fun (dynamic cur-fun)))
    (setf (?pathes result) (dynamic typepathes))
    (setf (?type-descr-s fun)
          (make-formal-type-descr
           (dynamic typepathes)
           result
           (?value result) ; var or constant
           ()
           (?var-descr fun)
           (?arg-num-fun fun)
           (?type-descr-s fun)
           () ; no recursive
           )))
  ;; *hf* end changes
  ()
  )

(defmethod walk-result ((result <void>) (out-label <zykl-label>))
  (walk-block (?out-block out-label))
  )

(defmethod walk-result ((result <void>) out-label)
  () )

(defmethod walk-result ((result <null>) out-label
                        )
  () )

(defmethod walk-result ((result <goto>) out-label
                        )
  () )

(defmethod walk-result ((test <test>) out-label
                        )
  (let* ((fun (?function test))
         ;;       (arg-num (?arg-num test)) ;; removed by ak (arg-num unbound)
         (arg-num 2)
         (typedescrs
          (make-actual-type-descr (dynamic typepathes)
                                  test
                                  (?var-descr test)
                                  arg-num
                                  ()))
         (join-label (find-join-label (?then-block test)
                                      (?else-block test))))
    (types-hand-on-fun fun)
    (setq typedescrs (inference fun typedescrs))
    (let ((then-typedescrs (select-then-type-descr
                            (?function test)
                            typedescrs))
          (else-typedescrs (select-else-type-descr
                            (?function test)
                            typedescrs)))
      (if (or (and then-typedescrs else-typedescrs)
              (and (null? then-typedescrs) (null? else-typedescrs)))
          ;; both true or both ()
          (progn
            (specialize-descrs typedescrs (?type-descr test)) ;ak
            (setf (?type-descr-s test) typedescrs)
            (setf (?then-type-descr-s test) then-typedescrs)
            (setf (?else-type-descr-s test) else-typedescrs)
            (dynamic-let ((typepathes then-typedescrs))
                         (walk-block (?then-block test)))
            (dynamic-let ((typepathes else-typedescrs))
                         (walk-block (?else-block test)))
            ())
        ;; one () other not ()
        (let ((child-block (if then-typedescrs (?then-block test)
                             (?else-block test)))
              (block (?block test)))
          (dynamic-let ((typepathes (if then-typedescrs
                                        then-typedescrs
                                      else-typedescrs)))
                       (walk-block child-block)
                       (setf (?out-label block) (?out-label child-block))
                       (setf (?body block)
                             (append (?body block) (?body child-block)))
                       (setf (?interface block)
                             (?interface child-block))
                       (setf (?result block) (?result child-block))
                       ()))))
    (if join-label
        (walk-block (?out-block join-label))
      ())
    test)
  )

#module-end
