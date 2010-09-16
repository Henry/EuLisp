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
;;; Title: Loading of EuLisp sources
;;;  Description:
;;    EL2LZS-LOAD contains all stuff for loading EuLisp sources, which depends on the
;;    underlying file system and on the Lisp-system in which the compiler is running.
;;;  Documentation:
;;    The interface function load-apply-module accepts
;;    - a string, which is used as the argument for load
;;    - "", in which case a dialog is opened to ask for the file to be loaded
;;    - a symbol - the name of a module - for which the path is determined using the
;;    content of $apply-module-search-path. The constant $apply-module-search-path
;;    must be set before loading this module. Their name is placed in the CL-package
;;    USER.
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------


#module el2lzs-load
(import (level-1
         el2lzs-error
         (only (first second last directory
                      merge-pathnames make-pathname
                      string pathname-name pathname string-equal find)
               common-lisp)
         (only ($apply-module-search-path) cl-user))
 syntax (level-1
         (only (with-open-file)
               common-lisp))
 export (load-apply-module
         load-def-file)
 )

(defun check-module (path module-source)
  (cond ((null? (and (cons? module-source)
                    ;;is it a true list?
                    (null? (cdr (last module-source)))
                    ;;is it at least (defmodule name directives) ?
                    (<= 3 (length module-source))
                    (eq (first module-source)
                        'ES::defmodule)
                    ;;is the module-name a symbol?
                    (symbol? (second module-source))))
         (error-invalid-module-definition module-source path)
         ())
        ((null? (string-equal (string (second module-source))
                             (pathname-name (pathname path))))
         (warning-differing-names-for-module-and-file
          (second module-source)
          path)
         module-source)
        (t
         module-source)))

(defun load-module-file (path function)
  (dynamic-let ((*load-level* (+ 1 (dynamic *load-level*))))
               (info-loading-module path)
               (with-open-file (file path :direction :input :if-does-not-exist ())
                               (if file
                                   (let ((module-source (check-module path (read file))))
                                     (if module-source
                                         (cl:prog1
                                          (funcall function module-source)
                                          (info-module-loaded (second module-source)))
                                       (progn (error-cannot-load-file path) ())))
                                 (progn (error-cannot-open-file path) ())))))

(defglobal apply-module-directory ())

(defconstant $apply-module-file-extension
  (make-pathname :name :wild :type "em"))

(defconstant $def-module-file-extension
  (make-pathname :name :wild :type "def"
                 :directory `(:relative
                              "platforms" ,common-lisp-user::*arch*)))

(defglobal *apply-module-file-extension* $apply-module-file-extension)

(defun directory-of-apply-modules (pathes)
  (cl:mapcan (lambda (path)
               (directory
                (merge-pathnames (dynamic *apply-module-file-extension*)
                                 path)))
             pathes))

(defgeneric load-apply-module (module-name-or-path function))

(defmethod load-apply-module ((path <string>) function)
  ;; This method is activated only by top level loads and not by implicit ones
  ;; (which means not for import handling). Therefore we must compute the list of
  ;; available files in every case.
  #+:MCL
  (when (equal path "")
        (setq path (choose-file-dialog
                    :mac-file-type :text
                    :button-string "Load")))
  (dynamic-let ((apply-module-directory
                 (directory-of-apply-modules
                  (cons path $apply-module-search-path))))
               (load-module-file (merge-pathnames path
                                                  (dynamic *apply-module-file-extension*))
                                 function)))

(defmethod load-apply-module ((name <symbol>) function)
  ;; This method is activated by top level loads and by implicit loads for
  ;; imports. Only in the first case we must compute the list of all available
  ;; files.
  (dynamic-let ((apply-module-directory
                 (or (dynamic apply-module-directory)
                     (directory-of-apply-modules $apply-module-search-path))))
               (let ((file (find (string name) (dynamic apply-module-directory)
                                 :key #'pathname-name :test #'string-equal)))
                 (if file
                     (load-module-file file function)
                   (progn (error-cannot-find-file name) ())))))

(defun load-def-file (module-name-or-path function)
  (dynamic-let ((*apply-module-file-extension* $def-module-file-extension))
               (load-apply-module module-name-or-path function)))

#module-end
