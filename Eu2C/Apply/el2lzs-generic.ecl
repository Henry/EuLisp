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
;;;  Title: Transformation of generic function definitions into LZS
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module el2lzs-generic
(import (level-1
         el2lzs-rules
         el2lzs-error
         pair-ext
         list-ext
         tail-module
         lzs-mop
         quasiquote
         option-lists
         (only (%add-method) apply-funs)
         (only (warn list? append mapcar mapc make-instance vector)
               common-lisp))
 syntax (level-1
         el2lzs-main)
 )
;;;-----------------------------------------------------------------------------
;;; defgeneric
;;;-----------------------------------------------------------------------------

(deftranssyn (defgeneric gf-spec lambda-list . options)
  (with-defining-form
   (when options
         (warn-defgeneric-options-not-analyzed options))
   (setf (cdr (cddr (whole-form))) ())
   (whole-form)))

(deftransmod (defgeneric gf-spec lambda-list . options)
  (cond ((symbol? gf-spec)
         (add-function (make-instance <global-generic-fun>
                                      :identifier gf-spec)))
        ;; otherwise no binding must be generated
        (t ())))

(deftransdef (defgeneric gf-spec lambda-list . options)
  (with-defining-form
   (let* ((name (fun-spec-name gf-spec))
          (type (fun-spec-type gf-spec))
          (gf (find-in-lex-env name)))
     (cond ((null? type))
           ((eq type ^setter)
            (setq gf (set-setter gf gf-spec)))
           ((eq type ^converter)
            ;;ATTN: in this case gf is a class from which the converter gf must be
            ;;taken
            (setq gf (set-converter gf gf-spec)))
           (t (error-invalid-generic-function-spec gf-spec)
              (setq gf ())))
     (~initialize gf
                  (list ^name gf-spec
                        ^domain (lambda-specializers lambda-list)
                        ^range %object
                        ;;^method-class
                        ^parameters (trans-params
                                     (lambda-parameters lambda-list)
                                     ())
                        ))
     ())))

(defun set-converter (class-def gf-spec)
  (if (class-def? class-def)
      (if (?converter class-def)
          (error-converter-redefinition class-def)
        (setf (?converter class-def)
              (add-function
               (make-instance <global-generic-fun>))))
    (error-class-required-in-converter-spec gf-spec)))

(defun set-setter (fun gf-spec)
  (if (fun? fun)
      (if (?setter fun)
          (error-setter-redefinition fun)
        (setf (?setter fun)
              (add-function
               (make-instance <global-generic-fun>))))
    (error-function-required-in-setter-spec gf-spec)))


;;;-----------------------------------------------------------------------------
;;; defmethod
;;;-----------------------------------------------------------------------------

(deftranssyn (defmethod gf-spec lambda-list . body)
  (with-defining-form
   (setf (cdr (cddr (whole-form)))
         (list (transsyn-progn body)))
   (whole-form)))

;; transmod isn't necessary because no top level binding must be created

(deftransdef (defmethod gf-spec lambda-list body)
  (with-defining-form
   (let* ((name (fun-spec-name gf-spec))
          (type (fun-spec-type gf-spec))
          (gf (find-in-lex-env name))
          (specializers (lambda-specializers lambda-list)))
     (cond ((null? type))
           ((eq type ^setter)
            (setq gf (get-setter gf gf-spec)))
           ((eq type ^converter)
            ;;ATTN: in this case gf is a class from which the converter gf must be
            ;;taken
            (setq gf (get-converter gf gf-spec)))
           (t (error-invalid-generic-function-spec gf-spec)
              (setq gf ())))
     (when gf
           (let ((method (~initialize (make-instance <method-def>)
                                      (list   ^domain specializers
                                              ^range %object
                                              ^function (trans-method-function-lambda
                                                         gf
                                                         <global-fun>
                                                         (lambda-parameters lambda-list)
                                                         specializers
                                                         body)
                                              ^generic-function gf))))
             (~add-method gf method)
             (if (imported? gf)
                 (make-dynamic-add-method-form gf method)
               ())))
     )))

(defun make-dynamic-add-method-form (gf method)
  (list (make-instance <app>
                       :function %add-method
                       :arg-list (list gf method)
                       )))

(defun get-converter (class-def gf-spec)
  (if (class-def? class-def)
      (or (?converter class-def)
          (and (error-no-converter class-def) ()))
    (progn (error-class-required-in-converter-spec gf-spec)
           ())))

(defun get-setter (fun gf-spec)
  (if (fun? fun)
      (or (?setter fun)
          (and (error-no-setter fun) ()))
    (progn (error-function-required-in-setter-spec gf-spec)
           ())))

;;;-----------------------------------------------------------------------------
;;; method-function-lambda
;;;-----------------------------------------------------------------------------

(defun trans-method-function-lambda (gf function-class lambda-params
                                        lambda-specs body)
  (let ((gf-id (?identifier gf)))
    (unless (list? gf-id) (setq gf-id (list gf-id)))
    (trans-lambda body
                  (add-function
                   (make-instance function-class
                                  :identifier (append gf-id
                                                      (mapcar #'?identifier lambda-specs))))
                  (trans-params lambda-params ()))))

;;;-----------------------------------------------------------------------------
;;; %declare-external-generic
;;;-----------------------------------------------------------------------------

(deftranssyn (%declare-external-generic fun-spec params . options)
  (with-defining-form
   (check-options () ^(external-name language methods) () options)
   (whole-form)))

(deftransmod (%declare-external-generic fun-spec params . options)
  (let ((ID (first fun-spec)))
    (add-function (make-instance <imported-generic-fun> :identifier ID))))

(deftransdef (%declare-external-generic fun-spec PARAMETERS . OPTIONS)
  ;; this transformation doesn't use initialize
  ;; a future reimplementation should work with ~initialize to make handling of
  ;; generic functions and methods uniform
  (with-defining-form
   (let* ((ID (first fun-spec))
          (TYPE (second fun-spec))
          (gf (find-in-lex-env ID))
          (external-name (get-option ^external-name OPTIONS ()))
          (language (get-option ^language OPTIONS ()))
          (methods (get-option ^methods OPTIONS ()))
          (domain (lambda-specializers PARAMETERS)))
     (setf (?params gf)
           (trans-params (lambda-parameters PARAMETERS) ()))
     (setf (?range-and-domain gf)
           (apply #'vector (trans TYPE) domain))
     (setf (?domain gf) domain)
     (setf (?code-identifier gf) external-name)
     (mapc (lambda (method-spec)
             (add-method-fun (find-in-lex-env (car method-spec)) ; the method function
                             ;; the result class is ignored
                             (mapcar #'find-in-lex-env
                                     (cdr (cdr method-spec))) ; the domain
                             gf))
           methods)
     ())))

(defun add-method-fun (method-fun domain gf)
  (~add-method gf
               (~initialize (make-instance <method-def>)
                            (list ^domain domain
                                  ^range %object
                                  ^function method-fun
                                  ^generic-function gf))))

;;;-----------------------------------------------------------------------------
;;; TAIL: %define-generic
;;;-----------------------------------------------------------------------------
;;
;;(deftranssyn (%define-generic genfun-spec spec-lambda-list)
;;  (progn (mapc (lambda (slot-description)
;;                 (setf (third slot-description)
;;                       (transsyn (third slot-description))))
;;               slot-descriptions)
;;         (whole-form)))
;;
;;(deftransmod (%define-generic genfun-spec spec-lambda-list)
;;  (let* ((ID (first class-spec))
;;         (class (make-instance <standard-class-def>
;;                  :identifier ID)))
;;    (add-class class)
;;    (nconc (list class)
;;           (transmod-slot-descriptions slot-descriptions)
;;           (transmod-class-options class-options))))
;;
;;
;;(deftransdef (%define-generic genfun-spec spec-lambda-list)
;;  (let* ((id (first class-spec))
;;         (metaclass (second class-spec))
;;         (class-def (find-in-lex-env id))
;;         (supers (list (find-in-lex-env superclass))))
;;    (setf (?class class-def) (find-in-lex-env metaclass))
;;???    (transdef-slot-descriptions slot-descriptions (?direct-slots class-def) class-def)
;;    (~initialize class-def
;;       (list* ^name id
;;              ^direct-superclasses supers
;;              ^direct-slot-descriptions (mapcar #'slot-name-type-init
;;                                                slot-descriptions)
;;              class-options))
;;    (bind-slot-accessors class-def slot-descriptions)
;;    (bind-class-functions class-def class-options)
;;    ()))

#module-end
