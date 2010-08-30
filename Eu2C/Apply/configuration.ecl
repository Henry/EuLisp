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
;;;  Title: interface to configuration of apply
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------

#module configuration
(import ((except (member read) eulisp1)
         (only (read-eulisp $eulisp-readtable) el-modules)
         (rename ((second cl:second)
                  (member cl:member)
                  (set-dispatch-macro-character cl::set-dispatch-macro-character)
                  (error cl:error)
                  (char= cl:char=)
                  (not cl:not)
                  (some cl:some)
                  (every cl:every)
                  (values cl:values)
                  (read cl:read)
                  (with-open-file cl:with-open-file)
                  (make-pathname cl:make-pathname))
                 (only (values not some every make-pathname member second
                               read set-dispatch-macro-character error char= with-open-file)
                       common-lisp)
                 (only ($applyroot)) common-lisp-user)
         )
 syntax (eulisp1)
 export (?configuration
         ?configuration-value
         ?configuration-values
         configurationp
         *global-optimization*
         init-configuration-table
         *ti-break*))

;;;-----------------------------------------------------------------------------
;;; initialization of configuration
;;;-----------------------------------------------------------------------------
(deflocal configuration-table  ())

(defun init-configuration-table ()
  (cl:with-open-file (s (cl:make-pathname
                         :directory `(,@common-lisp-user::$applyroot "EuLispModules")
                         :name "eu2c.config") :direction :input)
                     (setq configuration-table (read-eulisp s))
                     (if (eq s configuration-table)
                         (progn (cl:error "Error empty configuration file... Exit")
                                (cl-user::quit))
                       (initialize-configuration-variables))))

;;;-----------------------------------------------------------------------------
;;; interface functions
;;;-----------------------------------------------------------------------------

(defun configurationp (key value)
  (if (cl:member value (cdr(car(cl:member key configuration-table :key #'car))))
      t
    ()))

(defun ?configuration (key)
  (let ((entry (cl:member key configuration-table :key #'car)))
    (and entry (car entry))))

(defun ?configuration-value (key)
  (cl:second (?configuration key)))

(defun ?configuration-values (key)
  (cdr (?configuration key)))

;;;-----------------------------------------------------------------------------
;;; conditional reader macros for using configuration tests in applications
;;;-----------------------------------------------------------------------------

(defun read-configuration-expression
  (s macro-char arg)
  (let ((conf (read-eulisp s))
        (expr (read-eulisp s)))
    (if (or (and (cl:char= macro-char #\+)
                 (check-configuration conf))
            (and (cl:char= macro-char #\-)
                 (cl:not (check-configuration conf))))
        expr
      (cl:values)
      )))

(defun check-configuration
  (conf)
  (if (consp conf)
      ()
    (cl:error "~%Error wrong expression ~s for conditional read" conf))
  (let ((op (car conf)))
    (cond ((eq op 'eulisp-symbol::not)
           (cl:not (check-configuration (car (cdr conf)))))
          ((eq op 'eulisp-symbol::or)
           (cl:some #'check-configuration (cdr conf)))
          ((eq op 'eulisp-symbol::and)
           (cl:every #'check-configuration (cdr conf)))
          (t (configurationp (car conf) (car (cdr conf)))))))



(cl:set-dispatch-macro-character #\# #\+ #'read-configuration-expression
                                 $eulisp-readtable)
(cl:set-dispatch-macro-character #\# #\- #'read-configuration-expression
                                 $eulisp-readtable)

;;;-----------------------------------------------------------------------------
;;; configuration variables
;;;-----------------------------------------------------------------------------

(defvar *inline* ())
;; used in function-call
;; () - no inlining at all
;; 0   - only inlining of slot-accessors and slot-default-functions if they meet the
;;       requirement of (dynamic *inline*) = 1
;; n   - inlining takes place if the "complexity" of the function is less than n
(defvar *info-level* 2)

(defvar *system-info-level* 2)
;; 0 no infos, very short warnings and errors
;; 1 very short infos
;; 2 some infos
;; 3 all infos

(defvar *static-mm-type* ^t)
;; t  - sytem generates values during compile time
;;      only applicable for total and rt-system compilation
;; () - results in one additional indirection for each allocation of an instance

(defvar *static-mm-card* ^t)
;; t  - sytem generates values during compile time
;;      only applicable for total and rt-system compilation
;; () - results in one additional indirection for each allocation of an instance

(deflocal *global-optimization* ^t)
;; t  - global optimization on; function type schemes are reduced with type
;;      schemes of applications (if known)
;; () - global optimization off

(deflocal *ti-break* ())
;; t  - compilation is suspended if a type clash occurs
;; () - compilation is not suspended if a type clash occurs

(defun initialize-configuration-variables ()
  (dynamic-setq *inline* (?configuration-value ^inline))
  (dynamic-setq *info-level* (?configuration-value ^info-level))
  (dynamic-setq *system-info-level* (?configuration-value ^system-info-level))
  (dynamic-setq *static-mm-type* (?configuration-value ^static-mm-type))
  (dynamic-setq *static-mm-card* (?configuration-value ^static-mm-card))
  (setq *global-optimization* (?configuration-value ^global-optimization))
  (setq *ti-break* (?configuration-value ^ti-break))
  )

#module-end
