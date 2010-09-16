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
;;;  Authors:
;;;-----------------------------------------------------------------------------
;;; Load the CL compiler specific modifications
#+ :cmu (load "Apply/cmu.lisp")

;; -----------------------------------------------------------------------------
(in-package #:cl-user)

;; -----------------------------------------------------------------------------
;;; Set the machine word-length keyword
;;;  from the first command-line argument
;; -----------------------------------------------------------------------------

(defparameter *arch*
  (cadr
   #+ :sbcl sb-ext:*posix-argv*
   #+ :cmu ext:*command-line-strings*
   ))
(export '*arch*)

(cond
 ((search "m32" *arch*)
  (setq *features* (append *features* '(:word32))))
 ((search "m64" *arch*)
  (setq *features* (append *features* '(:word64))))
 (t
  (format
   t
   "~%First command line argument does not specify machine type m32 or m64~%")
  (quit))
 )

;; -----------------------------------------------------------------------------
;;; Portable POSIX functions
;; -----------------------------------------------------------------------------
(defun getenv (name &optional default)
  "Portable wrapper for the POSIX getenv function."
  #+ :cmu
  (let ((x (assoc name ext:*environment-list* :test #'string=)))
    (if x (cdr x) default))
  #- :cmu
  (or
   #+ :clisp (ext:getenv name)
   #+ :sbcl (sb-unix::posix-getenv name)
   default))
(export 'getenv)

(defun split-path (path)
  "Returns a list of substrings for the components of PATH
split at the UNIX separator #\/."
  (loop for i = 0 then (1+ j)
        as j = (position #\/  path :start i)
        collect (subseq path i j)
        while j))
(export 'split-path)

;; -----------------------------------------------------------------------------
;;; Set the $applyroot path parameter
;; -----------------------------------------------------------------------------
(defparameter $applyroot
  (pathname-directory
   (make-pathname :directory
                  (when (getenv "Eu2CROOT")
                    (cons :absolute (split-path (getenv "Eu2CROOT")))))))

(unless $applyroot
  (format t "~%Environmen Variable Eu2CROOT is not defined -- Sorry~%")
  (quit))

(export '$applyroot)

;; -----------------------------------------------------------------------------
;;; Installing file extensions for eulisp modules
;; -----------------------------------------------------------------------------
(defvar *eulisp-module-file-extension*
  (make-pathname :type "ecl"))

(defvar *compiled-eulisp-module-file-extension*
  #+ :sbcl (make-pathname :type "fasl")
  #+ :cmu (make-pathname :type "sse2f")
  )

;; -----------------------------------------------------------------------------
;;; Setting *eulisp-module-search-path* for Eulisp-in-CL modules
;; -----------------------------------------------------------------------------
(defparameter *eulisp-module-search-path*
  (list
   (make-pathname :directory '(:relative))
   (make-pathname :directory '(:relative "EclModules"))
   (make-pathname :directory '(:relative "Apply"))
   ))
(export '*eulisp-module-search-path*)

(defparameter $apply-module-search-path nil)
(export '$apply-module-search-path)

;; -----------------------------------------------------------------------------
;;; Set compilation environment
;; -----------------------------------------------------------------------------
;;(declaim (optimize (speed 1) (safety 3) (space 1) (debug 3)))
(declaim (optimize (speed 3) (safety 1) (space 1) (debug 1)))

;; suppress printing toplevel-form currently compiled
;;#+ :sbcl (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

;; -----------------------------------------------------------------------------
;;; Function to dump the image
;; -----------------------------------------------------------------------------

#+(or :sbcl :cmu)
(defvar saved-readtable nil)

#+(or :sbcl :cmu)
(defun image-init ()
  (setq *readtable* saved-readtable)
  (unless
      (setq $applyroot
            (pathname-directory
             (make-pathname
              :directory
              (when (getenv "Eu2CROOT")
                (cons :absolute (split-path (getenv "Eu2CROOT")))))))
    (format t "~%Environment variable Eu2CROOT is not defined -- Sorry~%")
    (quit))
  (when (boundp '$apply-module-search-path)
    (makunbound '$apply-module-search-path))
  (setq $apply-module-search-path
        (list
         (make-pathname :directory '(:relative))
         (make-pathname :directory '(:relative "EuLispModules"))
         (make-pathname :directory `(,@$applyroot "EuLispModules")))))

#+(or :sbcl :cmu)
(defun dump-apply (name)
  (load (make-pathname :directory `(,@$applyroot "Apply") :name "compile"))
  (setq saved-readtable (copy-readtable *readtable*))
  #+ :sbcl (setq sb-ext:*init-hooks* '(image-init))
  #+ :cmu (setf ext:*after-save-initializations*
                (append ext:*after-save-initializations* (list 'image-init)))
  #+ :sbcl (save-lisp-and-die (concatenate 'string "Bin/" name) :executable t)
  #+ :cmu (save-lisp (concatenate 'string "Lib/" *arch* "/" name ".cmu"))
  )

;; -----------------------------------------------------------------------------
;;; the function to delete compiled files
;; -----------------------------------------------------------------------------
(defun delete-compiled-apply-files ()
  (mapc #'delete-file
        (directory
         (merge-pathnames
          (make-pathname
           :directory $applyroot
           :name :wild)
          *compiled-eulisp-module-file-extension*)))
  (mapc #'delete-file
        (directory
         (merge-pathnames
          (make-pathname
           :directory `(,@$applyroot "EclModules")
           :name :wild)
          *compiled-eulisp-module-file-extension*)))
  (mapc #'delete-file
        (directory
         (merge-pathnames
          (make-pathname
           :directory `(,@$applyroot "Apply")
           :name :wild)
          *compiled-eulisp-module-file-extension*))))

;; -----------------------------------------------------------------------------
;;; End of environment.lisp
;; -----------------------------------------------------------------------------
