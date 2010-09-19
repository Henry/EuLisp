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
;;;  Authors: Ingo Mohr
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defpackage apply-standard
  (:export defstandardclass handle-slot-desc make-eulisp-class-id)
  (:use #:cl)) ;;***HGW

(in-package apply-standard)

(defun make-eulisp-class-id (identifier)
  (intern (concatenate 'string "<" (symbol-name identifier) ">")))

(defclass <defstandardclass-handler> ()
  ((state-key :reader state-key
              :initarg :key)
   (state-handler :reader state-handler
                  :initarg :handler)
   (result-handler :reader result-handler
                   :initarg :result)
   (accumulator :accessor accumulator
                :initform () )))

(defvar *defstandardclass-handlers*)
(setq *defstandardclass-handlers* nil)

(defun define-defstandardclass-handler (key element-handler result-handler)
  (if (find key *defstandardclass-handlers* :key #'state-key)
      (error "defstandardclass-handler for key ~A already exists" key)
    (setq *defstandardclass-handlers*
          (cons (make-instance '<defstandardclass-handler>
                               :key key :handler element-handler :result result-handler)
                *defstandardclass-handlers*))))

(defun get-defstandardclass-state (key)
  (accumulator (find key *defstandardclass-handlers* :key #'state-key)))

(defun new-name (prefix name suffix &optional (trim-bag ""))
  (intern (concatenate 'string prefix (string-trim trim-bag (string name)) suffix)))

(defvar *standard-slot-options* ())
(setq *standard-slot-options* '(:accessor))

(defun handle-slot-desc (class-name slot-desc)
  (cond ((null slot-desc) nil)
        ((eq slot-desc :slots) nil)
        ((symbolp slot-desc)
         (handle-slot-desc class-name
                           `(,slot-desc ,@*standard-slot-options*)))
        (t `(,(first slot-desc)
             ,@(handle-structured-slot-desc (first slot-desc) (rest slot-desc)
                                            nil)))))

(defun handle-structured-slot-desc (name options reader-or-accessor?)
  (if (null options) nil
    (case (first options)
      (:reader (if (not reader-or-accessor?)
                   `(:reader ,(new-name "?" name "")
                             ,@(handle-structured-slot-desc
                                name (rest options) t))
                 (handle-structured-slot-desc name (rest options)
                                              reader-or-accessor?)))
      (:accessor (if (not reader-or-accessor?)
                     `(:accessor ,(new-name "?" name "")
                                 ,@(handle-structured-slot-desc
                                    name (rest options) t))
                   (handle-structured-slot-desc name (rest options)
                                                reader-or-accessor?)))
      (:writer `(:writer ,(new-name "!" name "")
                         ,@(handle-structured-slot-desc name (rest options)
                                                        reader-or-accessor?)))
      (:initarg `(:initarg ,(intern (string name) (find-package "KEYWORD"))
                           ,@(handle-structured-slot-desc name (rest options)
                                                          reader-or-accessor?)))
      (:standard-options (handle-structured-slot-desc
                          name
                          (append *standard-slot-options* (rest options))
                          reader-or-accessor?))
      (t `(,(first options) ,(second options)
           ,@(handle-structured-slot-desc name (cddr options)
                                          reader-or-accessor?)))
      )))

(define-defstandardclass-handler :slots
  #'handle-slot-desc
  #'(lambda (result)
      (list (reverse result)            ; slots
            nil          ;; class options
            nil          ;; top-level forms
            )))

(define-defstandardclass-handler :define-defstandardclass-handlers
  #'(lambda (class-name value)
      (declare (ignore class-name))
      (apply #'define-defstandardclass-handler value))
  #'(lambda (result)
      (declare (ignore result))
      nil))

(define-defstandardclass-handler :default-slot-options
  #'(lambda (class-name value)
      (declare (ignore class-name))
      (setq *standard-slot-options* value))
  #'(lambda (result)
      (declare (ignore result))
      nil))

(define-defstandardclass-handler :predicate
  #'(lambda (class-name value)
      (declare (ignore value))
      `(defun ,(new-name "" class-name "?" "<>") (x)
         (typep x ',class-name)))
  #'(lambda (result)
      (list nil nil result)))

(defmacro defstandardclass (name supers &rest slots-and-options)
  (let ((*defstandardclass-handlers* *defstandardclass-handlers*)
        (*standard-slot-options* *standard-slot-options*))
    (let ((state (find :slots *defstandardclass-handlers*
                       :key #'state-key)))
      (mapc #'(lambda (element)
                (when (find element *defstandardclass-handlers*
                            :key #'state-key)
                  (setq state
                        (find element *defstandardclass-handlers*
                              :key #'state-key)))
                (let ((result (funcall (state-handler state) name element)))
                  (when result (push result (accumulator state)))))
            slots-and-options))
    (let ((slots () )
          (class-options () )
          (top-level () ))
      (mapc #'(lambda (handler)
                (let ((result (funcall (result-handler handler)
                                       (accumulator handler))))
                  (setf (accumulator handler) nil)
                  (when result
                    (setq slots (append slots (first result)))
                    (setq class-options (append class-options (second result)))
                    (setq top-level (append top-level (third result))))))
            *defstandardclass-handlers*)
      `(progn (defclass ,name ,supers     ; it's the CL-defclass
                ,slots ,@class-options)
              (defvar ,name (find-class ',name))
              ,@top-level
              ',name))))

;; Examples

;; (defstandardclass <foo> ()
;;   bar
;;   (baz :reader :initform 123)
;;   (foobar :reader :standard-options)
;;   (foobarbaz :initarg :accessor)
;;   :predicate)

;; (defstandardclass <bar> ()
;;   :default-slot-options (:reader :initarg)
;;   :slots
;;   foo
;;   (bar :accessor :standard-options))
