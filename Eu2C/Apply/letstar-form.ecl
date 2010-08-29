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

#module letstar-form
(import (eulisp1
         accessors
         context
         LZS
         analyse-h
         MZS
         inline-method
         move
         gutter
         type-inference)
 syntax (eulisp1
         simple-programming)
 export (letstar-a))


;;--- definitions, exportations, initialization forms

(defun letstar-a (con form)
  ;;   [let* var-list              ;; List of var
  ;;         init-list             ;; List of initial values
  ;;         type-list
  ;;         read-gloc-list        ;; read-glocs of initial values
  ;;         write-gloc-list       ;; write-glocs of var's
  ;;         body]
  (dynamic-let ((closure (dynamic closure)))
               (let ((oldenv (dynamic env))
                     (var-list (?var-list form))
                     res)
                 (bind-vars var-list (?init-list form)
                            (?type-list form)
                            (?read-gloc-list form)
                            (?write-gloc-list form))
                 (if (join-label-p con)
                     (setf (?rebind-vars con)
                           (cons var-list
                                 (?rebind-vars con)))
                   ())
                 (setq res (l2m-a con (?body form)))
                 ;; !!!!! missing unbind !!!!!
                 (if (or (function-label-p con)
                         (join-label-p con))
                     () (unbind var-list oldenv))
                 res
                 )))

(defun unbind (vlist oenv)
  (let ((cenv (dynamic env)))
    (setf (dynamic env) (unbind1 vlist cenv oenv))))

(defun unbind1  (vl cenv oenv)
  (cond ((eq cenv oenv) oenv)
        ((member (car (car cenv)) vl) (unbind1 vl (cdr cenv) oenv))
        (t (cons (car cenv) (unbind1 vl (cdr cenv) oenv)))))

(defun bind-vars (var-list init-list type-list rgloc-list wgloc-list)
  (cond ((null var-list) ())
        (t (bind-vars1 (car var-list)
                       (if init-list (l2m-a (dynamic *arg-context*)
                                            (car init-list)) ())
                       (if type-list (class-as-type-expr
                                      (car type-list))
                         ())
                       (car rgloc-list) (car wgloc-list))
           (bind-vars (cdr var-list) (if init-list (cdr init-list) ())
                      (if type-list (cdr type-list) ())
                      (cdr rgloc-list) (cdr wgloc-list)))))


(defgeneric bind-vars1 (var init type rgloc wgloc))

(defmethod bind-vars1 ((var <local-static>) init type rgloc wgloc)
  (if (?closure var)
      (add-closure-var-value var init (dynamic block))
    (if (tempvar-p init)
        (progn
          (subst-and-check-tempvar (?link init) var type)
          (add-env var var))
      (add-move var init type rgloc wgloc))))

(defun subst-and-check-tempvar (link locstat type)
  (setf (?link locstat) link)
  (subst-and-check-tempvar1 link locstat type))

(defun subst-and-check-tempvar1 (link var type)
  (if (null link) ()
    (let* ((stat (car (car link)))
           (in (cdr (car link)))
           (td (?type-descr stat))
           (tds (?type-descr-s stat)))
      (setf (vector-ref (?var-vec (?var-descr stat)) in) var)
      (if type
          (progn
            (set-descr-type td in type)
            (if tds (check-result-subtypes tds td) ()))
        ())
      (subst-and-check-tempvar1 (cdr link) var type))))

(defmethod bind-vars1 ((var <global-static>) init type rgloc wgloc)
  (add-move var init type rgloc wgloc))

(defmethod bind-vars1 ((var <imported-static>) init type rgloc wgloc)
  (add-move var init type rgloc wgloc))

(defmethod bind-vars1 ((var <dynamic>) init type rgloc wgloc)
  (print "dynamic-let not jet implemented"))


#module-end
