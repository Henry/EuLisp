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
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

#module gutter
(import
 (level-1
  SIMPLE-PROGRAMMING
  lzs
  mzs
  accessors
  vector
  analyse-h
  type-propagation
  type-inference
  side-effects-h
  lzs-to-mzs-fun
  apply-funs
  tail-module
  expand-literal
  (only (assoc) common-lisp))

 syntax
 (level-1)

 export
 (make-a-closure-function ; (fun block)
  set-closure-var         ; (var value block)
  add-closure-var-value   ; (var value block)
  add-closure-var         ; (var block)
  rename   ;; (var)
  )
 )


;;--- definitions, exportations, initialization forms

(defun rename (var)
  (if (?closure var)
      (read-closure-var var (dynamic block))
    (cdr (assoc var (dynamic env))))
  )

;;  (let ((newvar (assoc var (dynamic env))))
;;    (cond ((null? newvar)
;;           (setq newvar (assoc var (dynamic globenv)))
;;           (cond ((null? newvar)
;;                  (warning
;;                 "seltsam, die Variable ~s ist nicht in der Umgebung"
;;                          var)
;;                  var)
;;                 (t (setq newvar (cdr newvar))
;;                    (add-closure-var newvar) newvar)))
;;          (t (cdr newvar)))))

(defun add-closure-var (var block)
  (let ((result (add-function-call %closure-push
                                   block 2 var
                                   (if (dynamic closure)
                                       (car (car (dynamic closure))) ())
                                   ())))
    ;; extension of the closure (description)
    (dynamic-setq closure
                  (cons (cons result var)
                        (dynamic closure))))
  )

(defun add-closure-var-value (var value block)
  (let ((result (add-function-call %closure-push
                                   block 2 value
                                   (if (dynamic closure)
                                       (car (car (dynamic closure))) ())
                                   ())))
    ;; extension of the closure (description)
    (dynamic-setq closure
                  (cons (cons result var)
                        (dynamic closure))))
  )
(defun read-closure-var (var block)
  (let ((cl (dynamic closure)))
    (add-function-call %closure-value block 2 (car (car cl))
                       (get-closure-number var cl 0) ()))
  )

(defun set-closure-var (var value block)
  (let ((cl (dynamic closure)))
    (add-function-call %set-closure-value block 3
                       (car (car cl))
                       (get-closure-number var cl 0)
                       value)))

(defun make-a-closure-function (fun block)
  ;; add-function in fun-list !!!

  (add-function-call %make-function block 3
                     (compute-arg-descr fun)
                     (car (car (dynamic closure)))
                     (%function-literal fun)
                     ;;                     (make <literal-instance>
                     ;;                           :value-list (list fun)
                     ;;                           :class %function)
                     ))

(defun compute-arg-descr (fun)
  (let* ((p (?params fun))
         (n (compute-arg-descr1 (?var-list p))))
    (if (?rest p)
        (mk-sigwi (- 0 (+ n 1)))
      (mk-sigwi n))))

(defun compute-arg-descr1 (var)
  (if (null? var) 0
    (+ 1 (compute-arg-descr1 (cdr var)))))

(defun mk-sigwi (n)
  (make-literal-instance %signed-word-integer (list n))
  )
;;  (make <literal-instance> :value-list (list n)
;;        :class %signed-word-integer))


(defun get-closure-number (var cl nr)
  (if (null? cl)
      (progn (warning "Var ~s not found in closure" var) nr)
    (if (eq (cdr (car cl)) var) (mk-sigwi nr)
      (get-closure-number var (cdr cl) (+ nr 1))))
  )





#module-end
