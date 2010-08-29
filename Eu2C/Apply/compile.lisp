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
;;;  Authors:
;;;-----------------------------------------------------------------------------
(in-package cl-user)

(import '(el-modules::module-package
          el-modules::module-path
          el-modules::*eulisp-modules*))

(defun compile-apply-compiler ()
  (mapc #'compile-eulisp-module *eulisp-modules*))

(defun compile-eulisp-module (module)
  (when (module-path module)
    ;;then it is not a module made from a CL package
    (compile-eulisp-module-package (module-package module))))

(defun compile-eulisp-module-package (package)
  (do-symbols (s package)
    ;;(compile-el2lzs-transformations s)
    (when (and (fboundp s)
               (null (macro-function s))
               (eq package (symbol-package s)))
      (compile-function-or-method s)
      )))

(defun compile-function-or-method (s)
  (let ((fcn (symbol-function s)))
    (if (subtypep (class-of fcn) 'standard-generic-function)
        ;;method branch
        (dolist (method
                 #+ :sbcl (sb-mop:generic-function-methods fcn)
                 #+ :cmu (pcl:generic-function-methods fcn)
                 )
          (when (not (compiled-function-p
                      #+ :sbcl (sb-mop:method-function method)
                      #+ :cmu (pcl:method-function method)
                      ))
            #+ (or :sbcl :cmu) (progn (print "***ERROR excl::method-to-definition-spec needed")
                                      (cl-user::quit)))
          )
      ;;simple function
      (when (not (compiled-function-p (symbol-function s)))
        (compile s)))))

(defun compile-el2lzs-transformations (s)
  (mapc #'(lambda (ind)
            (let ((fun (get s ind nil)))
              (when (and fun
                         (functionp fun)
                         (not (compiled-function-p fun)))
                (setf (get s ind)
                      (compile fun)))))
        ^(trans transmod transdef transsyn)))

(compile-apply-compiler)
