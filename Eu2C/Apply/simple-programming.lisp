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
;;;  Title: simple-programming.lisp
;;;  Description:
;;    provides macro to facilitate class definition
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors:
;;;-----------------------------------------------------------------------------

(defpackage "SIMPLE-PROGRAMMING"
  (:shadowing-import-from el-modules eval-when)
  (:use #:cl) ;;***HGW
  (:export defclass-simple
           make-class-definition
           annotations
           structure-part
           make))

(in-package "SIMPLE-PROGRAMMING")

;;***HGW Transferred to `defpackage' declaration
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (export '(defclass-simple
;;              make-class-definition
;;              annotations
;;              structure-part
;;              make)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun apply-class-name (name)
    (intern (concatenate 'string "<" (string name) ">"))))

(defmacro make (class &rest options)
  `(make-instance ,class ,@options))

(defun make-class-definition (name supers slots)
  (let ((new-names nil)
        class-name
        (structure-part nil)
        (annotations nil)
        (annotations? nil))
    (flet ((new-name (prefix name suffix)
                     (setq name
                           (intern (concatenate 'string
                                                prefix (string name) suffix)))
                     (push name new-names)
                     name))
      (setq slots
            (mapcan
             #'(lambda (slot-desc)
                 (if (eq slot-desc :annotations)
                     (progn (setq annotations? t) nil)
                   (progn
                     (when (atom slot-desc) (setq slot-desc (list slot-desc)))
                     (let* ((name (car slot-desc)))
                       (if annotations?
                           (push name annotations)
                         (push name structure-part))
                       (list
                        (append slot-desc
                                (list :accessor
                                      (new-name "?" name "")
                                      :writer
                                      (new-name "!" name "")
                                      :initarg
                                      (intern (string name)
                                              (find-package "KEYWORD")))))))))
             slots))
      (push `(structure-part :allocation :class :initform ',structure-part
                             :reader get-structure-part)
            slots)
      (push `(annotations :allocation :class :initform ',annotations
                          :reader get-annotations)
            slots)
      (setq class-name (new-name "<" name ">"))
      (setq supers (mapcar #'apply-class-name supers))
      `(progn
         (defclass ,class-name ,supers (,@slots))
         (setq ,class-name (find-class ',class-name))
         (defun ,(new-name "" name "?") (x) (typep x ',class-name))
         (eval-when
             (:compile-toplevel :load-toplevel :execute)
           (export ',new-names))
         ',class-name))))

(defmacro defclass-simple (name supers &rest slots)
  (make-class-definition name supers slots))
